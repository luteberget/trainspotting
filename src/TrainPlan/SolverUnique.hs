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

newState :: Solver -> [Route] -> [Train] -> IO State
newState s routes trains = do
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

allocateAhead :: Solver -> [Route] -> Route -> Train -> State -> IO Lit
allocateAhead s routes route train state = case routeExit route of
  Nothing -> return false -- Not relevant for boundary exit routes
  Just signal -> do
    let isAllocated = state .! (rId route) .= Just (tId train)
    let nextRs = routes `startingIn` (Just signal)
    let immediateProgress = [state .! (rId r) .= Just (tId train)    | r <- nextRs ]
    --deferredProgress <- sequence [ newLit s >>= \l -> return (r, l)  | r <- nextRs ]
    deferredProgress <- newLit s
    addClause s ([neg isAllocated, deferredProgress] ++ immediateProgress)
    return deferredProgress

plan :: Int -> [Route] -> [Train] -> (RoutePlan -> IO Bool) -> IO (Maybe RoutePlan)
plan maxN routes trains test = withNewSolver $ \s -> do
  firstState <- newState s routes trains

  -- Special first state "transition" from nothing.
  -- This constraint is applied unconditionally.
  sequence_ [ allocateRoute s routes route train (Nothing, firstState)
            | route <- routes, train <- trains ]

  progressFuture <- sequence [ sequence [ allocateAhead s routes route train firstState
                                        | route <- routes ]
                             | train <- trains ]

  -- First state train birth condition
  -- This constraint is applied conditionally on not having more transitions.
  bornFuture <- sequence [ do
      -- Is this train born in this step?
      bornNow <- orl s [ firstState .! (rId route) .= Just (tId train)
                       | route <- routes `startingIn` Nothing ]
      bornFuture <- newLit s   -- Or is it born sometime in the future?
      equal s bornNow (neg bornFuture)
      return bornFuture
    | train <- trains ]

  -- Visits need to happen, and in order
  -- Only one state at this point, so the ordering does not matter yet.
  visitFuture <- sequence [ do
      sequence [ do
          visitFuture <- newLit s
          addClause s [ firstState .! visit .= Just (tId train), visitFuture]
          return visitFuture
        | visit <- trainVisits train ]
    | train <- trains ]

  -- The problem can now be solved for n=1 by solving conditionally on 
  -- the negation of all the future variables.
  solveAdd s 1 [firstState] progressFuture bornFuture visitFuture

  where
    solveAdd :: Solver -> Int -> [State] -> [[Lit]] -> [Lit] -> [[Lit]]  -> IO (Maybe RoutePlan)
    solveAdd s n states progressFuture bornFuture visitFuture = do
      putStrLn $ "Solving n=" ++ (show n) 
      b <- SAT.solve s (fmap neg (bornFuture ++ 
                                   (join visitFuture) ++ 
                                   (join progressFuture) ))
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
      state <- newState s routes trains

          -- Free routes which are no longer needed
      sequence_ [ freeRoute s routes route train (prevState,state)
                | route <- routes, train <- trains ]

      -- Constraint on allowed allocations
      sequence_ [ allocateRoute s routes route train (Just prevState,state)
                | route <- routes, train <- trains ]

      bornFuture <- sequence [ do
          bornNow <- orl s =<< mapM (andl s) [ [ neg (prevState .! (rId route) .= Just (tId train)),
                                               state .! (rId route) .= Just (tId train) ]
                                             | route <- routes `startingIn` Nothing ]

          bornFuture <- newLit s
          bornSatisfied <- orl s [bornNow, bornFuture]
          equal s needBirth bornSatisfied

          -- Being born now, we need some excuse for not having done it earlier
          let trainBirthPlace = head $ filter (\r -> (rId r) == head (trainVisits train)) routes 
          let hadConflict = [ [ neg (prevState .! conflicting .= Nothing),
                                -- neg (prevState .! conflicting .= Just (tId train)),
                                state .! (rId trainBirthPlace) .= Just (tId train) ]
                            | conflicting <- ((rId trainBirthPlace):(routeConflicts trainBirthPlace)) ]
          conflictResolved <- mapM (andl s) hadConflict
          addClause s ([neg bornNow] ++ conflictResolved)

          return bornFuture
        | (train, needBirth) <- zip trains needBirths ]

      visitFuture <- sequence [ do
          let f prevVisits futures [] = return futures
              f prevVisits futures ((route,needed):xs) = do
                let arrivedNow = state .! route .= Just (tId train)
                thisVisitOk      <- orl s  [neg needed, arrivedNow]
                thisVisitOrdered <- andl s [thisVisitOk, prevVisits]
                futureVisit      <- newLit s
                addClause s [thisVisitOrdered, futureVisit]
                f thisVisitOrdered (futures ++ [futureVisit]) xs
          f true [] (zip (trainVisits train) needVisit)
        | (train, needVisit) <- zip trains needVisits ]

      progressFuture <- forM (zip trains needProgress) $ \(train,needProgressTrain) -> do
        forM (zip routes needProgressTrain) $ \(route,need) -> do
          case routeExit route of
            Nothing -> return false -- Not relevant for boundary exit routes
            Just signal -> do
              let isAllocated = state .! (rId route) .= Just (tId train)
              let immediateProgress = [state .! (rId r) .= Just (tId train) 
                                      | r <- routes `startingIn` (Just signal)]
              deferredProgress <- newLit s
              let hadConflict = [ [ neg (prevState .! conflicting .= Nothing),
                                    -- state .! conflicting .= Nothing,   --- Excluding conflicting is already in another constraint
                                    state .! (rId nextRoute) .= Just (tId train) ]
                                | nextRoute <- routes `startingIn` (Just signal) 
                                , conflicting <- ( (rId nextRoute) : (routeConflicts nextRoute)) ] -- Also conflicts with itself

              conflictResolved <- mapM (andl s) hadConflict

              addClause s ([neg need, deferredProgress] ++ conflictResolved)
              addClause s ([neg isAllocated, deferredProgress] ++ immediateProgress)
              return deferredProgress

      solveAdd s n (states ++ [state]) progressFuture bornFuture visitFuture

showSchedule :: RoutePlan -> String
showSchedule s = join [ line ++ "\n" | line <- fmap showState s]
  where
    showState s = join [ cell ++ " " | cell <- fmap showRoute s]
    showRoute (r,Nothing) = (show r) ++ "_"
    showRoute (r,Just t) = (show r) ++ (show t)

