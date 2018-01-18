module TrainPlan.Solver (
  plan, Route(..), Train(..)
  ) where

import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Control.Monad (forM, forM_, join)
import qualified Data.Set as Set
import SAT
import SAT.Val
import SAT.Bool

import TrainPlan.Infrastructure hiding (Route)
import TrainPlan.UsagePattern
import TrainPlan.Schedule
import qualified TrainPlan.Parser
import qualified TrainSim.ConvertInput as SimConv
import qualified TrainSim.Builder as SimBuilder


import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure,exitSuccess)
logmsg = hPutStrLn stderr
output = putStrLn


type SignalId = String
type RouteId = Int
type TrainId = Int

 -- data RouteEntry = NoEntry | SourceNode | SignalEntry SignalId
 -- data RouteExit  = NoExit  | SinkNode   | SignalExit  SignalId

data Route
  = Route
  { routeId        :: RouteId
  , routeEntry     :: Maybe SignalId
  , routeExit      :: Maybe SignalId
  , routeConflicts :: [RouteId]
  , routeLength    :: Double
  } deriving (Eq, Ord, Show)

data Train
  = Train
  { trainId     :: TrainId
  , trainLength :: Double
  , trainVisits :: [RouteId]
  } deriving (Eq, Ord, Show)

type State    = [(RouteId, Val (Maybe TrainId))]

type RoutePlan = [[(RouteId, Maybe TrainId)]]

testRoutes =
  [ Route 1 Nothing (Just "1") [] 50.0
  , Route 2 (Just "1") Nothing [] 78.0
  , Route 3 (Just "1") Nothing [] 74.0
  , Route 4 (Just "1") Nothing [] 78.0
  , Route 5 (Just "1") Nothing [] 78.0
  , Route 6 (Just "1") (Just "66") [] 73.0
  , Route 7 (Just "1") Nothing [] 78.0
  , Route 8 (Just "1") Nothing [] 78.0
  ]

testTrains =
  [ Train 1 75.0 [1,2]
  , Train 2 75.0 [1,2]
  , Train 3 75.0 [1,2]
  , Train 4 75.0 [1,2]
  , Train 5 75.0 [1,2]
  ]


birthCondition :: Solver -> [Route] -> [Train] -> [State] -> IO ()
birthCondition s routes trains states = do
  forM_ trains $ \train -> do
    birthState <- sequence [ newLit s | _ <- states ]
    atMostOne s birthState

    forM_ routes $ \route -> do
      case (routeEntry route) of
        Nothing -> do 
          let fstSt = head states
          addClause s [ neg ((stVal fstSt (routeId route)) .= Just (trainId train)),
                        birthState !! 0]
          forM_ (zip [1..] (succPairs states)) $ \(i,(s1,s2)) -> do
            addClause s [ neg ((stVal s2 (routeId route)) .= Just (trainId train)),
                               (stVal s1 (routeId route))  .= Just (trainId train),
                              birthState !! i ]
        Just _ -> return ()
      

nubOrd = Set.toList . Set.fromList

newState :: Solver -> [Route] -> [Train] -> IO State
newState s routes trains = do
  routeStates <- sequence [ newVal s (Nothing : [ Just (trainId t) | t <- trains ])
                          | _ <- routes ]
  let state = [(routeId r, occ) | (r,occ) <- zip routes routeStates]
  -- TODO initial state must have some contiguity (sammenhengende) constraint?


  -- Mutually excluded routes
  -- TODO remove duplicates
  forM_ routes $ \route -> do
    forM_ (routeConflicts route) $ \otherRouteId -> do
      addClause s [(stVal state (routeId route)) .= Nothing,
                   (stVal state (otherRouteId))  .= Nothing]

  let startPts = nubOrd (fmap routeEntry routes)

  -- At most one alternative route is taken by a train
  -- This includes source nodes, i.e. a train cannot be born
  -- in many places 
  forM_ trains $ \train -> do
    forM_ startPts $ \signal -> do
      atMostOne s [(stVal state (routeId route)) .= Just (trainId train)
                  | route <- [route | route <- routes
                             , (routeEntry route) == signal]]

  return state


freeable :: Solver -> [Route] -> Route -> Train -> State -> IO [Lit]
-- returns alternative sufficient conditions for freeing a route from this train
freeable s routes route train state = go route (trainLength train)
  where 
    go route remainingLength = case (routeExit route) of
      Nothing -> return [true]
      Just signal -> do
        let alternatives = routesStartingIn routes signal
        forM alternatives $ \nextRoute -> do
          -- if next route is long enough
          if (routeLength nextRoute) >= remainingLength then do
            return ((stVal state (routeId nextRoute)) .= Just (trainId train))
          else do
            nexts <- go nextRoute (remainingLength - (routeLength nextRoute))
            anyNext <- orl s nexts
            andl s [(stVal state (routeId nextRoute)) .= Just (trainId train), 
                    anyNext]

newTransition :: Solver -> [Route] -> [Train] -> (State,State) -> IO ()
newTransition s routes trains (s1,s2) = do
  forM_ trains $ \train -> do
    forM_ routes $ \route -> do

      -- freeable?
      freeConditions <- freeable s routes route train s1
      addClause s ([     (stVal s2 (routeId route)) .= Just (trainId train),
                    neg ((stVal s1 (routeId route)) .= Just (trainId train))]
                  ++ freeConditions )

 


      case (routeEntry route) of
        -- either the start pt is undefined, or the previous route was/is allocated
        -- note that allocating source routes is handled specially in *birthCondition*
        Nothing -> return ()
        Just signal -> do
          -- If we allocate, a previous route must already be allocated
          addClause s ([ neg ((stVal s2 (routeId route)) .= Just (trainId train))
                       ,     ((stVal s1 (routeId route)) .= Just (trainId train))]
                       ++
                       [ (stVal s2 (routeId otherRoute)) .= Just (trainId train)
                       | otherRoute <- routesEndingIn routes signal ])
          --
          -- If we free, we need to free all previous allocations
          -- if not, the earlier allocations can start travelling again

          previousFree <- andl s 
            [ neg ((stVal s2 (routeId otherRoute)) .= Just (trainId train))
            | otherRoute <- routesEndingIn routes signal]

          addClause s [     (stVal s2 (routeId route)) .= Just (trainId train),
                       neg ((stVal s1 (routeId route)) .= Just (trainId train)),
                       previousFree]

        -- TODO composite routes are allocated together
        --

stVal :: State -> RouteId -> Val (Maybe TrainId)
stVal s r = fromMaybe (error "stateLookup") $ 
  listToMaybe [ val | (id,val) <- s, id == r ]

routesEndingIn :: [Route] -> SignalId -> [Route]
routesEndingIn routes signal  = [ route | route <- routes
                                , routeExit route == Just signal ]
routesStartingIn :: [Route] -> SignalId -> [Route]
routesStartingIn routes signal  = [ route | route <- routes
                                  , routeEntry route == Just signal ]

visitConstraint :: Solver -> [State] -> Train -> IO ()
visitConstraint s states train = do
  -- This one is covered by *before*  (?)
  -- -- train needs to visit in some state
  let visits = trainVisits train

  -- The first visit needs to happen sometime
  addClause s [ val .= (Just (trainId train)) 
                          | state <- states
                          , (rid,val) <- state
                          , rid == head visits ]
    
  -- train needs to visit in correct order
  forM_ (succPairs visits) $ \(v1,v2) -> do
    before s states (trainId train) v1 v2

  return ()

before :: Solver -> [State] -> TrainId -> RouteId -> RouteId -> IO ()
before s states trainId v1 v2 = do
  v1 <- forM (zip [0..] states) $ \(s_v1_i,s_v1) -> do
    -- is V1 activated in this state?
    let is_v1 = fromMaybe (error "before") $ 
                  listToMaybe  [ val .= (Just trainId) 
                               | (rid, val) <- s_v1
                               , rid == v1 ]
    
    -- is V2 activated AFTER this state?
    is_v2 <- orl s [val .= (Just trainId)
                | state <- drop s_v1_i states
                , (rid, val) <- state
                , rid == v2]
    andl s [is_v1,is_v2]
    
    
  addClause s v1 -- any of them will do

  
succPairs x = zip x (tail x)

plan :: [Route] -> [Train] -> (RoutePlan -> IO Bool) -> IO (Maybe RoutePlan)
plan routes trains test = do
  withNewSolver $ \s -> do
    putStrLn "creating"
    putStrLn $ show routes
    putStrLn $ show trains
    let n = 1
    states <- sequence [ newState s routes trains | _ <- [1..n] ]

    forM_ (succPairs states) $ \(s1,s2) -> newTransition s routes trains (s1,s2)
    birthCondition s routes trains states

    -- first state
    let fstState = head states -- If a route is active in the first state,
    -- it must trace back to a source node
    forM_ routes $ \route -> do
      forM_ trains $ \train -> do
        case (routeEntry route) of 
          Nothing -> return ()
          Just signal -> do 
            addClause s ( [ neg ((stVal fstState (routeId route)) .= Just (trainId train))]
                        ++ 
                           [ (stVal fstState (routeId otherRoute)) .= Just (trainId train)
                           | otherRoute <- routesEndingIn routes signal ])
    

    -- Visit constraints
    sequence_ [ visitConstraint s states train | train <- trains]

    solveLoop s states
  where
    solveLoop s states = do 
      putStrLn "solving"
      b <- SAT.solve s []
      if b then do 
        putStrLn "*** solution"
        scheduleValues <- forM states $ \state -> do
          forM state $ \(r,var) -> do
            x <- SAT.Val.modelValue s var
            return (r,x, var .= x)
        let schedule = (fmap.fmap) (\(a,b,_) -> (a,b)) scheduleValues

        putStrLn $ showSchedule schedule
        acceptable <- test schedule
        if acceptable then return (Just schedule)
        else do
          let values = [ lit | stateSchedule <- scheduleValues
                             , (_,_,lit) <- stateSchedule ]
          addClause s (map neg values)
          solveLoop s states
          
      else do
        putStrLn "*** no solution"
        return Nothing
    

showSchedule :: RoutePlan -> String
showSchedule s = join [ line ++ "\n" | line <- fmap showState s]
  where
    showState s = join [ cell ++ " " | cell <- fmap showRoute s]
    showRoute (r,Nothing) = (show r) ++ "_"
    showRoute (r,Just t) = (show r) ++ (show t)

-- showSchedule :: RoutePlan -> String
-- showSchedule s = join [ line ++ "\n", 
--                       | line <- fmap showState s ]
--   where
--     showState :: [(RouteId, Maybe TrainId)]
-- 
-- diffSchedule :: RoutePlan -> RoutePlan
-- diffSchedule s = map (\(s1,s2) -> ) s
--   where
--     pairs = succPairs ([(r,Nothing) | (r,i) <- head s ] : s)
