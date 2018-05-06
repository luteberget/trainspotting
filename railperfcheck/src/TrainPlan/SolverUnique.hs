module TrainPlan.SolverUnique (
  plan
  ) where

-- Train route planner, enhanced version of Solver.hs with:
--   * incremental bound on number of transitions 
--   * maximal progress, which should ensure unique solutions.
--   * maximal progress on boundary
--   * Partial release (done in the route converter)
--   * Timing (ordering) constraints between visits on different trains
--
-- TODOs:
--   * internal visits 
--     - find route and direction(?) which covers specified point
--     - total length until stops for simulator input
--   * Repeated visits? How to handle ordering.
--   * trains turning directions up/down -- is it always allowed? how to communicate to solver.


import Data.Maybe

import SAT
import SAT.Val
import SAT.Bool
import SAT.Equal
import TrainPlan.SolverInput
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join, forM)
forM_ = flip mapM_

type NodeMap = Map NodeRef (Set RoutePartId)
type Occupation = [(RoutePartId, Val (Maybe TrainName))]
data State
  = State
  { occupation     :: Occupation
  , progressBefore :: [(Train, [(RoutePart, Lit)])]
  , bornBefore     :: [(Train, Lit)]
  , visitBefore    :: [(Train, [([NodeRef], Lit)])]
  } deriving (Eq, Ord, Show)

rId = routePartName
tId = trainName

succPairs x = zip x (tail x)

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList

endingIn   routes x = filter (\r -> (routePartExit r)  == x) routes 
startingIn routes x = filter (\r -> (routePartEntry r) == x) routes 

occupationValue :: [(RoutePartId, Val (Maybe TrainName))] -> RoutePartId -> Val (Maybe TrainName)
occupationValue s r = fromMaybe (error "state lookup") $
  listToMaybe [ val | (rid, val) <- s, rid == r ]
(.!) = occupationValue

exactlyOne :: Solver -> [Lit] -> IO ()
exactlyOne s xs = do
  atMostOne s xs
  addClause s xs

newState :: Solver -> Problem -> NodeMap -> Maybe State -> IO State
newState s (routes,partialroutes,trains,ords) nodeMap prevState = do
  routeStates <- sequence [ newVal s (Nothing :  [ Just (tId t) | t <- trains ])
                          | _ <- routes ]
  let occ = [(rId r, occ) | (r,occ) <- zip routes routeStates ]

  -- Exclude conflicting routes
  sequence_ [ do
      addClause s [ occ .! (rId route) .= Nothing,
                    occ .! conflicting .= Nothing]
    | route <- routes, conflicting <- routePartConflicts route ]

  -- At most one alternative routes is taken
  let startPts = nubOrd (fmap routePartEntry routes)
  sequence_ [ atMostOne s [ occ .! (rId route) .= Just (tId train)
                          | route <- routes `startingIn` entry ]
            | train <- trains, entry <- startPts ]

  -- Partial routes are allocated together
  sequence_ [ do
      sequence_ [ do
          sequence_ [ do
              let activated r = andl s [ occ .! r .= Just (tId train),
                    fromMaybe true $ fmap (\s -> neg ((occupation s) .! r 
                                             .= Just (tId train))) prevState ]
              r1a <- activated r1
              r2a <- activated r2
              equal s r1a r2a
            | (r1,r2) <- succPairs routeSet ]
        | routeSet <- partialroutes ]
    | train <- trains ]
  
  -- Allocate route constraints
  sequence_ [ allocateRoute s routes route train (fmap occupation prevState, occ)
            | route <- routes, train <- trains ]

  -- Free routes which are no longer needed
  sequence_ [
        sequence_ [ freeRoute s routes route train (occupation prevState, occ)
        | route <- routes, train <- trains ]
    | Just prevState <- [prevState] ]

  let allFalseProgress = [ (t, [ (r, false) | r <- routes ] ) | t <- trains ]
  let progress = fromMaybe allFalseProgress (fmap progressBefore prevState)
  progressFuture <- sequence [ do 
      p <- sequence [ do
          p <- allocateAhead s routes partialroutes route train (fmap occupation prevState, occ) progressBefore
          return (route, p)
        | (route, progressBefore) <- trainProgress ]
      return (train, p)
    | (train, trainProgress) <- progress ]

  let allFalseBorn = [ (t, false) | t <- trains ]
  let born = fromMaybe allFalseBorn (fmap bornBefore prevState)
  bornFuture <- sequence [ do
       b <- bornCondition s nodeMap routes partialroutes train (fmap occupation prevState, occ) born
       return (train,b)
     | (train, born) <- born ]

  let allFalseVisit = [ (t, [(r, false) | r <- trainVisits t] ) | t <- trains ]
  let visit = fromMaybe allFalseVisit (fmap visitBefore prevState)
  visitFuture <- sequence [ do
        v <- sequence [ do
                v <- visitConstraint s nodeMap occ train nodes visitBefore
                return (nodes, v)
              | (nodes, visitBefore) <- trainVisitsBefore ]
        return (train, v)
    | (train,trainVisitsBefore) <- visit  ]

  -- putStrLn $ "ORDS: " ++ (show ords)
  sequence_ [ do
      let (nodes1,before1) = head [ (nodes,visitBefore) 
            | (train, (_,trainVisitsBefore)) <- zip trains visit 
            , (tId train) == t1
            , (nodes, visitBefore) <- trainVisitsBefore
            , nodes == (trainVisits train) !! v1 ]
 
      let (nodes2,after2) = head [ (nodes,neg visitBefore) 
            | (train, (_,trainVisitsFuture)) <- zip trains visitFuture
            , (tId train) == t2
            , (nodes, visitBefore) <- trainVisitsFuture
            , nodes == (trainVisits train) !! v2 ]

      --putStrLn $ "SEQ " ++ (show (t1,r1,before1)) ++ "----" ++ (show (t2,r2,after2))
      visitOrd s nodeMap occ (t1,nodes1,before1) (t2,nodes2,after2)
    | ((t1,v1),(t2,v2)) <- ords]

  --return (State occ progressFuture bornFuture visitFuture)
  return (State occ progressFuture bornFuture visitFuture)

freeRoute :: Solver -> [RoutePart] -> RoutePart -> Train -> (Occupation,Occupation) -> IO ()
freeRoute s routes route train (s1,s2) = do
  free <- orl s =<< isFreeable route (trainLength train)
  equalOr s [neg (s1 .! (rId route) .= Just (tId train))] 
     (neg free) (s2 .! (rId route) .= Just (tId train))
  where
    isFreeable r remainingLength = case routePartExit r of
      Nothing -> return [true] -- Can always free after exiting the model
      Just signal ->  sequence [ do
          if routePartLength nextRoute >= remainingLength then do
            return (s1 .! (rId nextRoute) .= Just (tId train))
          else do
            nexts <- isFreeable nextRoute (remainingLength - (routePartLength nextRoute))
            anyNext <- orl s nexts
            andl s [s1 .! (rId nextRoute) .= Just (tId train), anyNext]
        | nextRoute <- routes `startingIn` (Just signal) ]

allocateRoute :: Solver -> [RoutePart] -> RoutePart -> Train -> (Maybe Occupation,Occupation) -> IO ()
allocateRoute s routes route train (s1,s2) = case routePartEntry route of
  Nothing -> return () -- Not relevant for boundary entry routes
  Just signal -> do
    let wasAllocated        = fmap (\s -> s .! (rId route) .= Just (tId train)) s1
    let becomesAllocated    =   Just (s2 .! (rId route)       .= Just (tId train))
    let previousIsAllocated = [ Just (s2 .! (rId otherRoute)  .= Just (tId train))
                              | otherRoute <- routes `endingIn` (Just signal) ]
    -- Constraint on which routes are allowed to be allocated
    -- New allocations must have a preceding route active in the same step,
    -- i.e. trains cannot "swap" places in one step.
    addClause s $ catMaybes 
      ([fmap neg becomesAllocated, wasAllocated] ++ previousIsAllocated)

wholeRouteConflicts :: [RoutePart] -> [[RoutePartId]] -> RoutePart -> [RoutePartId]
wholeRouteConflicts routes partialroutes route = nubOrd $ join [ routePartConflicts r | r <- parts ]
  where parts = fmap (\x -> head [ r | r <- routes, rId r == x ]) partIds
        partIds = head [ whole | whole <- partialroutes, (rId route) `elem` whole ]

allocateAhead :: Solver -> [RoutePart] -> [[RoutePartId]] ->  RoutePart -> Train -> (Maybe Occupation, Occupation) -> Lit -> IO Lit
allocateAhead s routes partialroutes route train (prevState,state) progressBefore = case routePartExit route of
  Nothing -> return true -- Not relevant for boundary exit routes
  Just signal -> do
    let isAllocated = state .! (rId route) .= Just (tId train)
    let nextRs = routes `startingIn` (Just signal)
    let progressNow = [state .! (rId r) .= Just (tId train)    | r <- nextRs ]
    progressFuture <- newLit s
    addClause s ([neg isAllocated, progressFuture] ++ progressNow)

    forM_ prevState $ \prev -> do 
      let hadConflict = [ [ neg (prev .! conflicting .= Nothing),
                            neg (prev .! (rId nextRoute) .= Just (tId train)),
                            state .! (rId nextRoute) .= Just (tId train) ]
                        | nextRoute <- nextRs
                        , conflicting <- ( (rId nextRoute) : (wholeRouteConflicts routes partialroutes nextRoute)) ]
      conflictResolved <- mapM (andl s) hadConflict
      addClause s ([progressBefore, progressFuture] ++ conflictResolved)
    return (neg progressFuture)

bornCondition :: Solver -> NodeMap -> [RoutePart] -> [[RoutePartId]] -> Train -> (Maybe Occupation, Occupation) -> Lit -> IO Lit
bornCondition s nodeMap routes partialroutes train (prevState,state) bornBefore = do
   -- Is this train born in this step?
   let trainBirthPlaces = [ route
                          | rid <- nodesToRoutes nodeMap (head (trainVisits train))
                          , route <- routes
                          , rid == routePartName route ]
   let bornNowAlternatives = 
         [ catMaybes [ fmap (\prev -> neg (prev .! (rId route) .= Just (tId train))) prevState,
                       Just (state .! (rId route) .= Just (tId train)) ]
         | route <- trainBirthPlaces ]

   -- Don't get born in other places
   sequence_ [ addClause s [ neg (state .! (rId r) .= Just (tId train)) ]
             | r <- routes `startingIn` Nothing , not ((rId r) `elem` (fmap rId trainBirthPlaces)) ]

   bornNow <- orl s =<< mapM (andl s) bornNowAlternatives
   bornFuture <- newLit s   -- Or is it born sometime in the future?
   exactlyOne s [bornBefore, bornNow, bornFuture]
   --let trainBirthPlaces = head $ filter (\r -> (rId r) == head (trainVisits train)) routes 
   forM_ prevState $ \prev -> do 
       -- If train is born, and we are not on the first step,
       -- then it must be after a conflict has been resolved.
       resolvedAlternatives <- forM trainBirthPlaces $ \trainBirthPlace -> do
         let hadConflict = [ [ neg (prev .! conflicting .= Nothing),
                               neg (prev .! (rId trainBirthPlace) .= Just (tId train)),
                               state .! (rId trainBirthPlace) .= Just (tId train) ]
                           | conflicting <- ((rId trainBirthPlace):
                                            (wholeRouteConflicts routes partialroutes trainBirthPlace)) ]
         conflictResolved <- mapM (andl s) hadConflict
         -- conflictResolved :: [Lit] is a list of ways that the given
         -- birth place could have had a conflict which is resolved.
         return conflictResolved
       addClause s ([neg bornNow] ++ (join resolvedAlternatives))
   return (neg bornFuture)

visitConstraint :: Solver -> NodeMap -> Occupation -> Train -> [NodeRef] -> Lit -> IO Lit
visitConstraint s nodeMap occ train nodes visitBefore = do
  -- Visits must happen
  visitNow <- orl s [occ .! route .= Just (tId train)
                    | route <- nodesToRoutes nodeMap nodes]
  visitFuture <- newLit s
  addClause s [visitBefore, visitNow, visitFuture]
  return (neg visitFuture)

visitOrd :: Solver -> NodeMap -> Occupation -> (TrainName,[NodeRef],Lit) -> (TrainName,[NodeRef],Lit) -> IO ()
visitOrd s nodeMap occ (t1,nodes1,before1) (_t2,_nodes2,future2) = do
  precVisitNow <- orl s [ occ .! r .= Just t1
                        | r <- nodesToRoutes nodeMap nodes1 ]
  addClause s [before1, precVisitNow, future2]

--   sequence [ do
--       let precedingVisitNow = occ .! precedingRoute .= Just precedingTrain
--       addClause s [precedingVisitBefore, precedingVisitNow, visitFuture]
--     | Just (precedingTrain,precedingRoute,precedingVisitBefore) <- [precedingVisit] ]

nodesToRoutes :: Map NodeRef (Set RoutePartId) -> [NodeRef] -> [RoutePartId]
nodesToRoutes map nodes = Set.toList (Set.unions [map Map.! name | name <- nodes ])

endStateCond :: State -> [Lit]
endStateCond s = [ l | (_, l) <- bornBefore s] ++
                 [ l | (_, ls) <- progressBefore s, (_, l) <- ls ] ++
                 [ l | (_, ls) <- visitBefore s, (_, l) <- ls ]

plan :: Int -> Int -> Problem -> (RoutePlan -> IO Bool) -> IO (Maybe RoutePlan)
plan nBefore nAfter problem@(routes,partialroutes,trains,orderings) test = withNewSolver $ \s -> do
  solveNewState Nothing s 0 []

  where
    nodeMap :: NodeMap
    --type NodeMap = Map NodeRef (Set RoutePartId)
    nodeMap = Map.fromListWith Set.union [ (node, Set.singleton (rId route))
                                         | route <- routes
                                         , node <- routePartContains route ]

    solveNewState :: Maybe Int -> Solver -> Int -> [State] -> IO (Maybe RoutePlan)
    solveNewState failedSteps s n states = do
      state <- newState s problem nodeMap (listToMaybe (reverse states))
      solveAndTest failedSteps s (n+1) (states ++ [state])

    solveAndTest :: Maybe Int ->  Solver -> Int -> [State] -> IO (Maybe RoutePlan)
    solveAndTest failedSteps s n states = do
      putStrLn $ "*** Solving for n=" ++ (show n) 
      b <- SAT.solve s (endStateCond (last states))
      if b then do
        putStrLn "*** Solution"
        schedule <- sequence [ sequence [ do v <- SAT.Val.modelValue s x
                                             return (r,v)
                                        | (r,x) <- occupation state ]
                             | state <- states ] 
        --putStrLn $ showSchedule schedule
        accept <- test schedule
        if accept then return (Just schedule)
        else do
          -- Remove this solution
          addClause s [ neg (x .= v) | (st,stv) <- zip states schedule 
                                     , ((_,x),(_,v)) <- zip (occupation st) stv]
          solveAndTest (Just 0) s n states
      else do
        putStrLn ("*** No more solutions for n=" ++ (show n))

        let increase = case failedSteps of
              Just f  -> f < nAfter
              Nothing -> n < nBefore

        if increase then do
          putStrLn "*** Increasing transitions bound"
          solveNewState (fmap (+1) failedSteps) s n states
        else do
          putStrLn "*** Maximum transition count reached"
          return Nothing

showSchedule :: RoutePlan -> String
showSchedule s = join [ line ++ "\n" | line <- fmap showState s]
  where
    showState s = join [ cell ++ " " | cell <- fmap showRoute s]
    showRoute (r,Nothing) = (show r) ++ "-[ ]"
    showRoute (r,Just t) = (show r) ++ "-[" ++ (show t) ++ "]"

