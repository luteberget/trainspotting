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
import Control.Monad (join, foldM, foldM_, forM, forM_, mapM)

rId = routeId
tId = trainId

succPairs x = zip x (tail x)
nubOrd = Set.toList . Set.fromList

endingIn   routes x = filter (\r -> (routeExit r)  == x) routes 
startingIn routes x = filter (\r -> (routeEntry r) == x) routes 

stateValue :: State -> RouteId -> Val (Maybe TrainId)
stateValue s r = fromMaybe (error "state lookup") $
  listToMaybe [ val | (rid, val) <- s, rid == r ]
(.!) = stateValue

exactlyOne :: Solver -> [Lit] -> IO ()
exactlyOne s xs = do
  atMostOne s xs
  addClause s xs

newState :: Solver -> Problem -> Maybe State -> IO State
newState s (routes,trains,orderings) prevState = do
  routeStates <- sequence [ newVal s (Nothing :  [ Just (tId t) | t <- trains ])
                          | _ <- routes ]
  let state = [(rId r, occ) | (r,occ) <- zip routes routeStates ]

  -- Exclude conflicting routes
  sequence_ [ do
      addClause s [ state .! (rId route) .= Nothing,
                    state .! conflicting .= Nothing]
    | route <- routes, conflicting <- routeConflicts route ]

  -- At most one alternative routes is taken
  let startPts = nubOrd (fmap routeEntry routes)
  sequence_ [ atMostOne s [ state .! (rId route) .= Just (tId train)
                          | route <- routes `startingIn` entry ]
            | train <- trains, entry <- startPts ]
  
  -- Special first state "transition" from nothing.
  -- This constraint is applied unconditionally.
  sequence_ [ allocateRoute s routes route train (prevState, state)
            | route <- routes, train <- trains ]

  return state

freeRoute :: Solver -> [Route] -> Route -> Train -> (State,State) -> IO ()
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

allocateRoute :: Solver -> [Route] -> Route -> Train -> (Maybe State,State) -> IO ()
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

allocateAhead :: Solver -> [Route] -> Route -> Train -> (Maybe State, State) -> Lit -> IO Lit
allocateAhead s routes route train (prevState,state) progressBefore = case routeExit route of
  Nothing -> return true -- Not relevant for boundary exit routes
  Just signal -> do
    let isAllocated = state .! (rId route) .= Just (tId train)
    let nextRs = routes `startingIn` (Just signal)
    let progressNow = [state .! (rId r) .= Just (tId train)    | r <- nextRs ]
    progressFuture <- newLit s
    addClause s ([neg isAllocated, progressFuture] ++ progressNow)

    case prevState of
      Just prev -> do
        let hadConflict = [ [ neg (prev .! conflicting .= Nothing),
                              state .! (rId nextRoute) .= Just (tId train) ]
                          | nextRoute <- routes `startingIn` (Just signal) 
                          , conflicting <- ( (rId nextRoute) : (routeConflicts nextRoute)) ]
        conflictResolved <- mapM (andl s) hadConflict
        addClause s ([progressBefore, progressFuture] ++ conflictResolved)
      Nothing -> return ()
    return (neg progressFuture)

bornCondition :: Solver -> [Route] -> Train -> (Maybe State, State) -> Lit -> IO Lit
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
   case prevState of 
     Just prev -> do
       -- If train is not born in the first step,
       -- then it must be after a conflict has been resolved.
       let hadConflict = [ [ neg (prev .! conflicting .= Nothing),
                             state .! (rId trainBirthPlace) .= Just (tId train) ]
                         | conflicting <- ((rId trainBirthPlace):
                              (routeConflicts trainBirthPlace)) ]
       conflictResolved <- mapM (andl s) hadConflict
       addClause s ([neg bornNow] ++ conflictResolved)
     Nothing -> return ()
   return (neg bornFuture)

visitConstraint :: Solver -> State -> Maybe (TrainId,RouteId,Lit) -> (TrainId, RouteId,Lit) -> IO Lit
visitConstraint s state precedingVisit (train,route,visitBefore) = do
  -- Visits must happen
  let visitNow = state .! route .= Just train
  visitFuture <- newLit s
  addClause s [visitBefore, visitNow, visitFuture]

  -- ... and in the given order
  case precedingVisit of
    Just (precedingTrain,precedingRoute,precedingVisitBefore) -> do
      let precedingVisitNow = state .! precedingRoute .= Just precedingTrain
      addClause s [precedingVisitBefore, precedingVisitNow, visitFuture]
    Nothing -> return ()

  return (neg visitFuture)

plan :: Int -> Problem -> (RoutePlan -> IO Bool) -> IO (Maybe RoutePlan)
plan maxN problem@(routes,trains,orderings) test = withNewSolver $ \s -> do
  firstState <- newState s problem Nothing

  progressFuture <- sequence [ sequence [ allocateAhead s routes route train (Nothing, firstState) false
                                        | route <- routes ]
                             | train <- trains ]
  bornFuture <- sequence [ bornCondition s routes train (Nothing, firstState) false | train <- trains ]
  visitFuture <- sequence [ do
    let visits = zip (trainVisits train) (repeat false)
    sequence [ do
      let prevVisit = fmap (\(prevRoute, prevLit) -> (prevRoute, tId train, prevLit)) prev
      let nextVisit = (nextRoute, tId train, nextLit)
      visitConstraint s firstState prevVisit nextVisit
      | (prev, (nextRoute, nextLit)) <- zip (Nothing:(fmap Just visits)) visits ]
    | train <- trains ]
  -- The problem can now be solved for n=1 by solving conditionally on 
  -- the negation of all the future variables.
  solveAdd s 1 [firstState] progressFuture bornFuture visitFuture

  where
    solveAdd :: Solver -> Int -> [State] -> [[Lit]] -> [Lit] -> [[Lit]]  -> IO (Maybe RoutePlan)
    solveAdd s n states progressFuture bornFuture visitFuture = do
      putStrLn $ "Solving n=" ++ (show n) 
      b <- SAT.solve s ((join progressFuture) ++ bornFuture ++ (fmap neg (join visitFuture)))
      if b then do
        putStrLn "*** Solution"
        schedule <- sequence [ sequence [ do v <- SAT.Val.modelValue s x
                                             return (r,v)
                                        | (r,x) <- state ]
                             | state <- states ] 
        putStrLn $ showSchedule schedule
        accept <- test schedule
        if accept then return (Just schedule)
        else do
          -- Remove this solution
          addClause s [ neg (x .= v) | (st,stv) <- zip states schedule 
                                     , ((_,x),(_,v)) <- zip st stv]
          solveAdd s n states progressFuture bornFuture visitFuture
      else do
        putStrLn ("*** No more solutions for n=" ++ (show n))
        if n < maxN then do
          putStrLn "*** Increasing transitions bound"
          newTransition s (n+1) states progressFuture bornFuture visitFuture
        else do
          putStrLn "*** Maximum transition count reached"
          return Nothing

    newTransition :: Solver -> Int -> [State] -> [[Lit]] -> [Lit] -> [[Lit]] -> IO (Maybe RoutePlan)
    newTransition s n states needProgress needBirths needVisits = do
      let prevState = last states
      state <- newState s problem (Just prevState)

          -- Free routes which are no longer needed
      sequence_ [ freeRoute s routes route train (prevState,state)
                | route <- routes, train <- trains ]


      bornFuture <- sequence [ bornCondition s routes train (Just prevState, state) bornBefore
        | (train, bornBefore) <- zip trains needBirths ]

      visitFuture <- sequence [ do
        let visits = zip (trainVisits train) needs
        sequence [ do
            let prevVisit = fmap (\(prevRoute, prevLit) -> (prevRoute, tId train, prevLit)) prev
            let nextVisit = (nextRoute, tId train, nextLit)
            visitConstraint s state prevVisit nextVisit
          | (prev, (nextRoute, nextLit)) <- zip (Nothing:(fmap Just visits)) visits ]
        | (train,needs) <- zip trains needVisits ]

      progressFuture <- sequence [ 
          sequence [
            allocateAhead s routes route train (Just prevState, state) progressBefore
          | (route, progressBefore) <- zip routes trainProgress ]
        | (train, trainProgress) <- zip trains needProgress ] 

      solveAdd s n (states ++ [state]) progressFuture bornFuture visitFuture

showSchedule :: RoutePlan -> String
showSchedule s = join [ line ++ "\n" | line <- fmap showState s]
  where
    showState s = join [ cell ++ " " | cell <- fmap showRoute s]
    showRoute (r,Nothing) = (show r) ++ "_"
    showRoute (r,Just t) = (show r) ++ (show t)

