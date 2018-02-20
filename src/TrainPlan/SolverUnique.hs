module TrainPlan.SolverUnique (
  plan
  ) where

-- Train route planner, enhanced version of Solver.hs with:
--   * incremental bound on number of transitions 
--   * maximal progress, which should ensure unique solutions.
--   * maximal progress on boundary
--
-- TODOs:
--  * Partial release (done in the route converter)
--  * Timing (ordering) constraints between visits on different trains
--  * Repeated visits? How to handle ordering.


import Data.Maybe

import SAT
import SAT.Val
import SAT.Bool
import SAT.Equal
import TrainPlan.SolverInput
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (join)
forM_ = flip mapM_

--type State    = [(RouteId, Val (Maybe TrainId))]
--
type Occupation = [(RouteId, Val (Maybe TrainId))]
data State
  = State
  { occupation     :: Occupation
  , progressBefore :: [(Train, [(Route, Lit)])]
  , bornBefore     :: [(Train, Lit)]
  , visitBefore    :: [(Train, [(RouteId, Lit)])]
  } deriving (Eq, Ord, Show)

rId = routeId
tId = trainId

succPairs x = zip x (tail x)
nubOrd = Set.toList . Set.fromList

endingIn   routes x = filter (\r -> (routeExit r)  == x) routes 
startingIn routes x = filter (\r -> (routeEntry r) == x) routes 

occupationValue :: [(RouteId, Val (Maybe TrainId))] -> RouteId -> Val (Maybe TrainId)
occupationValue s r = fromMaybe (error "state lookup") $
  listToMaybe [ val | (rid, val) <- s, rid == r ]
(.!) = occupationValue

exactlyOne :: Solver -> [Lit] -> IO ()
exactlyOne s xs = do
  atMostOne s xs
  addClause s xs

newState :: Solver -> Problem -> Maybe State -> IO State
newState s (routes,partialroutes,trains,orderings) prevState = do
  -- putStrLn $ "NEWSTATE " ++ (show prevState)
  routeStates <- sequence [ newVal s (Nothing :  [ Just (tId t) | t <- trains ])
                          | _ <- routes ]
  let occ = [(rId r, occ) | (r,occ) <- zip routes routeStates ]

  -- Exclude conflicting routes
  sequence_ [ do
      addClause s [ occ .! (rId route) .= Nothing,
                    occ .! conflicting .= Nothing]
    | route <- routes, conflicting <- routeConflicts route ]

  -- At most one alternative routes is taken
  let startPts = nubOrd (fmap routeEntry routes)
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
          p <- allocateAhead s routes route train (fmap occupation prevState, occ) progressBefore
          return (route, p)
        | (route, progressBefore) <- trainProgress ]
      return (train, p)
    | (train, trainProgress) <- progress ]

  let allFalseBorn = [ (t, false) | t <- trains ]
  let born = fromMaybe allFalseBorn (fmap bornBefore prevState)
  bornFuture <- sequence [ do
       b <- bornCondition s routes train (fmap occupation prevState, occ) born
       return (train,b)
     | (train, born) <- born ]

  let allFalseVisit = [ (t, [(r, false) | r <- trainVisits t] ) | t <- trains ]
  let visit = fromMaybe allFalseVisit (fmap visitBefore prevState)
  visitFuture <- sequence [ do
        v <- sequence [ do
                let prevVisit = fmap (\(r,l) -> (tId train, r, l)) prev
                let thisVisit = (tId train, route, lit)
                v <- visitConstraint s occ prevVisit thisVisit
                return (route, v)
              | (prev, (route, lit)) <- zip (Nothing:(fmap Just visits)) visits ]
        return (train, v)
    | (train,visits) <- visit  ]

  --return (State occ progressFuture bornFuture visitFuture)
  return (State occ progressFuture bornFuture visitFuture)

freeRoute :: Solver -> [Route] -> Route -> Train -> (Occupation,Occupation) -> IO ()
freeRoute s routes route train (s1,s2) = do
  free <- orl s =<< isFreeable route (trainLength train)
  equalOr s [neg (s1 .! (rId route) .= Just (tId train))] 
     (neg free) (s2 .! (rId route) .= Just (tId train))
  where
    isFreeable r remainingLength = case routeExit r of
      Nothing -> return [true] -- Can always free after exiting the model
      Just signal ->  sequence [ do
          if routeLength nextRoute >= remainingLength then do
            return (s1 .! (rId nextRoute) .= Just (tId train))
          else do
            nexts <- isFreeable nextRoute (remainingLength - (routeLength nextRoute))
            anyNext <- orl s nexts
            andl s [s1 .! (rId nextRoute) .= Just (tId train), anyNext]
        | nextRoute <- routes `startingIn` (Just signal) ]

allocateRoute :: Solver -> [Route] -> Route -> Train -> (Maybe Occupation,Occupation) -> IO ()
allocateRoute s routes route train (s1,s2) = case routeEntry route of
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

allocateAhead :: Solver -> [Route] -> Route -> Train -> (Maybe Occupation, Occupation) -> Lit -> IO Lit
allocateAhead s routes route train (prevState,state) progressBefore = case routeExit route of
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
                        , conflicting <- ( (rId nextRoute) : (routeConflicts nextRoute)) ]
      conflictResolved <- mapM (andl s) hadConflict
      addClause s ([progressBefore, progressFuture] ++ conflictResolved)
    return (neg progressFuture)

bornCondition :: Solver -> [Route] -> Train -> (Maybe Occupation, Occupation) -> Lit -> IO Lit
bornCondition s routes train (prevState,state) bornBefore = do
   -- Is this train born in this step?
   let bornNowAlternatives = 
         [ catMaybes [ fmap (\prev -> neg (prev .! (rId route) .= Just (tId train))) prevState,
                       Just (state .! (rId route) .= Just (tId train)) ]
         | route <- routes `startingIn` Nothing ]
   bornNow <- orl s =<< mapM (andl s) bornNowAlternatives
   bornFuture <- newLit s   -- Or is it born sometime in the future?
   exactlyOne s [bornBefore, bornNow, bornFuture]
   let trainBirthPlace = head $ filter (\r -> (rId r) == head (trainVisits train)) routes 
   forM_ prevState $ \prev -> do 
       -- If train is not born in the first step,
       -- then it must be after a conflict has been resolved.
       let hadConflict = [ [ neg (prev .! conflicting .= Nothing),
                             neg (prev .! (rId trainBirthPlace) .= Just (tId train)),
                             state .! (rId trainBirthPlace) .= Just (tId train) ]
                         | conflicting <- ((rId trainBirthPlace):
                              (routeConflicts trainBirthPlace)) ]
       conflictResolved <- mapM (andl s) hadConflict
       addClause s ([neg bornNow] ++ conflictResolved)
   return (neg bornFuture)

visitConstraint :: Solver -> Occupation -> Maybe (TrainId,RouteId,Lit) -> (TrainId, RouteId,Lit) -> IO Lit
visitConstraint s occ precedingVisit (train,route,visitBefore) = do
  -- Visits must happen
  let visitNow = occ .! route .= Just train
  visitFuture <- newLit s
  addClause s [visitBefore, visitNow, visitFuture]

  -- ... and in the given order
  sequence [ do
      let precedingVisitNow = occ .! precedingRoute .= Just precedingTrain
      addClause s [precedingVisitBefore, precedingVisitNow, visitFuture]
    | Just (precedingTrain,precedingRoute,precedingVisitBefore) <- [precedingVisit] ]

  return (neg visitFuture)

endStateCond :: State -> [Lit]
endStateCond s = [ l | (_, l) <- bornBefore s] ++
                 [ l | (_, ls) <- progressBefore s, (_, l) <- ls ] ++
                 [ l | (_, ls) <- visitBefore s, (_, l) <- ls ]

plan :: Int -> Problem -> (RoutePlan -> IO Bool) -> IO (Maybe RoutePlan)
plan maxN problem@(routes,partialroutes,trains,orderings) test = withNewSolver $ \s -> do
  solveNewState s 0 []

  where
    solveNewState :: Solver -> Int -> [State] -> IO (Maybe RoutePlan)
    solveNewState s n states = do
      state <- newState s problem (listToMaybe (reverse states))
      solveAndTest s (n+1) (states ++ [state])

    solveAndTest :: Solver -> Int -> [State] -> IO (Maybe RoutePlan)
    solveAndTest s n states = do
      putStrLn $ "*** Solving for n=" ++ (show n) 
      b <- SAT.solve s (endStateCond (last states))
      if b then do
        putStrLn "*** Solution"
        schedule <- sequence [ sequence [ do v <- SAT.Val.modelValue s x
                                             return (r,v)
                                        | (r,x) <- occupation state ]
                             | state <- states ] 
        putStrLn $ showSchedule schedule
        accept <- test schedule
        if accept then return (Just schedule)
        else do
          -- Remove this solution
          addClause s [ neg (x .= v) | (st,stv) <- zip states schedule 
                                     , ((_,x),(_,v)) <- zip (occupation st) stv]
          solveAndTest s n states
      else do
        putStrLn ("*** No more solutions for n=" ++ (show n))
        if n < maxN then do
          putStrLn "*** Increasing transitions bound"
          solveNewState s n states
        else do
          putStrLn "*** Maximum transition count reached"
          return Nothing

showSchedule :: RoutePlan -> String
showSchedule s = join [ line ++ "\n" | line <- fmap showState s]
  where
    showState s = join [ cell ++ " " | cell <- fmap showRoute s]
    showRoute (r,Nothing) = (show r) ++ "-[ ]"
    showRoute (r,Just t) = (show r) ++ "-[" ++ (show t) ++ "]"

