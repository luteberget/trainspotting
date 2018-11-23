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
import qualified TrainPlan.LoopCheck as LoopCheck
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join, forM, forM_)

type NodeMap = Map NodeRef (Set RoutePartId)
type Occupation = [(RoutePartId, Val (Maybe TrainName))]
type OverlapChoice = [(RoutePartId, Val Int)]
type OccOv = (Occupation, OverlapChoice)
type TrailMarks = [(SignalRef, [Lit])]
data State
  = State
  { occupation     :: Occupation
  , overlapChoice  :: OverlapChoice
  , trailMarks     :: TrailMarks
  , progressBefore :: [(Train, [(RoutePart, Lit)])]
  , bornBefore     :: [(Train, Lit)]
  , visitBefore    :: [(Train, [([NodeRef], Lit)])]
  , hasFinished    :: [Lit] -- one literal per train
  } deriving (Eq, Ord, Show)

rId = routePartName
tId = trainName

succPairs x = zip x (tail x)

pairs x = [ (a,b) | a <- x, b <- x, a /= b ]
setDiff :: Ord a => [a] -> [a] -> [a]
setDiff a b = Set.toList $ Set.difference (Set.fromList a) (Set.fromList b)

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList

endingIn   routes x = filter (\r -> (routePartExit r)  == x) routes 
startingIn routes x = filter (\r -> (routePartEntry r) == x) routes 

occupationValue :: [(RoutePartId, Val (Maybe TrainName))] -> RoutePartId -> Val (Maybe TrainName)
occupationValue s r = fromMaybe (error "state lookup") $
  listToMaybe [ val | (rid, val) <- s, rid == r ]
(.!) = occupationValue

conflictValue :: [(RoutePartId, Val Int)] -> RoutePartId -> Val Int
conflictValue s r = fromMaybe (error "state lookup") $
  listToMaybe [ val | (rid, val) <- s, rid == r ]
(.!!) = conflictValue

exactlyOne :: Solver -> [Lit] -> IO ()
exactlyOne s xs = do
  atMostOne s xs
  addClause s xs

newState :: Solver -> Problem -> NodeMap -> Maybe State -> IO State
newState s problem@(routes,partialroutes,trains,ords) nodeMap prevState = do
  routeStates <- sequence [ newVal s (Nothing :  [ Just (tId t) | t <- trains ])
                          | _ <- routes ]
  let occ = [(rId r, occ) | (r,occ) <- zip routes routeStates ]
  
  overChoice <- sequence [ newVal s (enumOverlaps r) | r <- routes]
  let overlap = [(rId r, ov) | (r,ov) <- zip routes overChoice ]

  -- Each signal each train can be trail marked
  let signals = nubOrd $ catMaybes $ join [ [routePartEntry r, routePartExit r] | r <- routes ]
  trail <- sequence [ do
      trainMarks <- sequence [ newLit s | _ <- trains ]
      return (sig, trainMarks)
    | sig <- signals ]

  -- HasFinished
  finished <- sequence [ newLit s | _ <- trains ]
  forM_ (zip3 [0..] trains finished) $ \(idx, train,finishedNow) -> do
      -- Finished bool cannot be unset
      forM prevState $ \prev -> do
        let finishedBefore = (hasFinished prev) !! idx
        addClause s [neg finishedBefore, finishedNow]

        -- Set when exiting
        sequence_ [ addClause s [neg ((occupation prev) .! (rId r) .= (Just (tId train))), finishedNow ]
                  | r <- routes `endingIn` Nothing ]

      -- Cannot use routes after finished
      sequence_ [ addClause s [neg (occ .! (rId r) .= (Just (tId train))), neg finishedNow ]
                | r <- routes ]
    

  -- Exclude conflicting routes
  sequence_ [ do
      addClause s [ occ .! (rId route) .= Nothing, neg (ol .= idx),
                    occ .! (confl_r) .= Nothing, neg (overlap .!! confl_r .= confl_ol) ]
    | (route,(_,ol)) <- zip routes overlap, idx <- domain ol,
      (confl_r, confl_ol) <- (routePartConflicts route) !! idx ]

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

  -- Trail: mark trail (false -> true) when activating
  -- TODO first state must also be included (mark on first state when occupied)
  let (prevOcc, prevTrail) = (fromMaybe [ (r,val Nothing) | (r,_) <- occ ]  $ fmap occupation prevState, 
                              fromMaybe [ (s,fmap (const false) t) | (s,t) <- trail] $ fmap trailMarks prevState) in 
    forM_ (zip prevTrail trail) $ \((sig, prevMarks),(_, currMarks)) -> do
      forM_ (zip3 prevMarks currMarks trains) $ \(prevMark,currMark,train) -> do
        -- foreach entry signal, foreach train
        let reachable = [ r_x | r_x <- routes, r_y <- routes
                              , Just sig == routePartExit r_y
                              , routePartEntry r_x == routePartEntry r_y ]
        forM_ reachable $ \r -> do
          -- if route became activated, then mark must also activate
          mark <- andl s [ neg prevMark, currMark ]
          -- addClause s [ prevOcc .!  (rId r) .= Just (tId train),  -- it was already active
          --               neg (occ .! (rId r) .= Just (tId train)),-- it is not active now
          --               mark ]                             -- else, mark it
                        
          return ()

  -- A conflict set might be excluded on allocation (overlap timeout)
  forM (zip routes overlap) $ \(r,(rid,ol)) -> do
    case routePartWaitConflict r of
      Nothing -> return ()
      Just idx -> do 
        activated <- andl s [ neg (occ .! rid .= Nothing),
          (fromMaybe true $ fmap (\st -> ((occupation st) .! rid) .= Nothing) prevState)]
        addClause s [ neg (ol .= idx), neg activated ]

  -- Conservative swinging overlap (only swing the overlap when conflict)
  forM prevState $ \prev -> do
      let (prevOcc, prevOv) = (occupation prev, overlapChoice prev)
      forM_ (zip routes overlap) $ \(r,(rid, ol)) -> do
        forM_ (pairs (enumOverlaps r)) $ \(idx1,idx2) -> do 
            let conflictDiff = setDiff ((routePartConflicts r) !! idx1)
                                       ((routePartConflicts r) !! idx2)
            usingAnyConflicting <- forM conflictDiff $ \(confl_r, confl_ol) -> do
              andl s [ neg (occ .! confl_r .= Nothing), overlap .!! confl_r .= confl_ol ]
            addClause s ([ occ .! rid .= Nothing, -- if the route is active 
                          prevOcc .! rid .= Nothing,
                          neg (prevOv .!! rid .= idx1),  -- come to this overlap from another
                          neg (overlap .!! rid .= idx2) ] ++ usingAnyConflicting)

  -- Clear trail markings
  --   Not relevant for before->first state, since all markings are off before,
  --   then sig1train1_prev is false and the implication holds.
  forM prevState $ \prev -> do
    let (prevOcc, prevOv, prevTrail) = (occupation prev, overlapChoice prev, trailMarks prev)
    forM_ (zip prevTrail trail) $ \((sig1, prevMarks1),(_, currMarks1)) -> do
      forM_ (zip3 prevMarks1 currMarks1 trains) $ \(prevMark1,currMark1,train1) -> do
        -- For each sig1, train1, with markings prev and curr
        --
        -- sig1train1_prev => sig1train1_next OR (  find train2 s.t.  
        --                                            becameAllocated(sig1,train2) 
        --                                              AND
        --                                            NOT (find route(sig2) s.t. 
        --                                                   occ_next(route)=train1 AND sig2train2_prev)) 
        --

        canClear <- forM (filter (/= train1) trains) $ \train2 -> do
          -- find route that allocates sig1 to train2
          alloc <- orl s =<< sequence [ andl s [neg (prevOcc .! (rId r) .= (Just (tId train2))), 
                                                     occ     .! (rId r) .= (Just (tId train2))]
                                      | r <- routes, routePartEntry r == Just sig1 ]
          -- check that train1 is not currently allocated to any route that has train2 mark
          standingOnTrail <- sequence [ andl s [ occ .! (rId r) .= (Just (tId train1)), trailTrain2 ]
                                      | r <- routes, (sig, marks) <- trail
                                      , routeAllocSignal problem r == Just sig
                                      , (train, trailTrain2) <- zip trains marks, train == train2 ]

          andl s ([alloc] ++ (fmap neg standingOnTrail))
        -- addClause s $ [neg prevMark1, currMark1] ++ canClear
        return ()

  -- Allocate route constraints
  sequence_ [ allocateRoute s routes route train (fmap occupation prevState, occ)
            | route <- routes, train <- trains ]

  -- Free routes which are no longer needed
  sequence_ [
        sequence_ [ freeRoute s routes route train (occupation prevState, occ)
        | route <- routes, train <- trains ]
    | Just prevState <- [prevState] ]

  let maybeOccOv s = fmap (\st -> (occupation st, overlapChoice st)) s
  let allFalseProgress = [ (t, [ (r, false) | r <- routes ] ) | t <- trains ]
  let progress = fromMaybe allFalseProgress (fmap progressBefore prevState)
  progressFuture <- sequence [ do 
      ps <- sequence [ do
          p <- allocateAhead s routes partialroutes route train (maybeOccOv prevState, (occ,overlap)) progressBefore
          return (route, p)
        | (route, progressBefore) <- trainProgress ]
      return (train, ps)
    | (train, trainProgress) <- progress ]

  let allFalseBorn = [ (t, false) | t <- trains ]
  let born = fromMaybe allFalseBorn (fmap bornBefore prevState)
  bornFuture <- sequence [ do
       b <- bornCondition s nodeMap routes partialroutes train (maybeOccOv prevState, (occ, overlap)) born
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
  return (State occ overlap trail progressFuture bornFuture visitFuture finished)

routeAllocSignal :: Problem -> RoutePart -> Maybe SignalRef
routeAllocSignal (routes, _, _, _) route = join $ fmap (\fr -> routePartEntry (fr)) firstRoute
  where firstRouteName = (fst (routePartName route), 0)
        firstRoute = hd [ r | r <- routes, routePartName r == firstRouteName ]
        hd [] = Nothing
        hd (x:_) = Just x

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

-- -- wholeRouteConflicts :: [Int] -> [RoutePart] -> [[RoutePartId]] -> RoutePartId -> [(Int, [RoutePartId])]
-- -- wholeRouteConflicts routes groups part = 
-- --   nubOrd $ join [ routePartConflicts r | r <- fmap (findRoute routes) (findPartGroup groups part) ]
-- -- 

findPartGroup ::  [[RoutePartId]] -> RoutePartId -> [RoutePartId]
findPartGroup groups partid = head [ group | group <- groups, partid `elem` group ]

findRoute :: [RoutePart] -> RoutePartId -> RoutePart
findRoute routes name = head [ r | r <- routes, rId r == name ]

enumOverlaps :: RoutePart -> [Int]
enumOverlaps r = take (length (routePartConflicts r)) [0..]

resolveConflictWith :: Solver -> [RoutePart] -> [[RoutePartId]] -> Train -> (OccOv,OccOv) -> [RoutePart] -> IO [Lit]
resolveConflictWith s routes partialroutes train ((s1,o1),(s2,o2)) candidates = do
  -- Return a disjunction of possible conflict resolutions that can be 
  -- used to resolve forced progress.
   putStrLn $ "resolveConflictWith"
   cand <- forM candidates $ \nextRoute -> do
       let partGroup = findPartGroup partialroutes (rId nextRoute)
       let partChoices = sequence [[(p,i) | i <- enumOverlaps (findRoute routes p) ] | p <- partGroup ]
       putStrLn $ "    partGroup "  ++  (show (rId nextRoute))
       -- Expand next partial route to its whole-route (with overlap choices)
       forM partChoices $ \choice -> do -- Different choices of overlap
         putStrLn $ "        choice "  ++  (show (choice))
         let externalConflicts = fmap (\(a,b) -> (a, Just b)) $ nubOrd $ join [ (routePartConflicts (findRoute routes r)) !! o | (r,o) <- choice ]
         let conflicts = externalConflicts ++ [(rId nextRoute, Nothing)]
         forM conflicts $ \(conflict_r,conflict_ol) -> do
           putStrLn $ "            resolve alternative" ++ (show (conflict_r, conflict_ol, choice))
           let hadConflict = [ neg (s1 .! conflict_r .= Nothing), -- conflicting route was allocated
                               fromMaybe true $ fmap (\ol -> (o1 .!! conflict_r .= ol)) conflict_ol]
           let resolve = join [ [ neg (s1 .! choice_r .= Just (tId train)), -- choice becomes activated
                                   s2 .! choice_r .= Just (tId train),
                                   o2 .!! choice_r .= choice_ol ] -- With given overlap]
                               | (choice_r, choice_ol) <- choice ] -- for each route/ol in the choice set 
           wasResolved <- andl s (hadConflict ++ resolve)
           return wasResolved
   putStrLn $ "#alt: " ++ (show (length (join $ join cand)))
   -- cand :: foreach nextRoute, 
   --           foreach overlaps(nextroute), 
   --             foreach conflict(nextRoute,overlap) 
   --               big-and(conflict_r was allocated, conflict_r had overlap conflict_ol, 
   --                       choice_r was not allocated to train, choice_r became allocated to train,
   --                       choice_r has overlap choice_ol)
   --                        
   return (join $ join cand)

allocateAhead :: Solver -> [RoutePart] -> [[RoutePartId]] ->  RoutePart -> Train -> (Maybe OccOv, OccOv) -> Lit -> IO Lit
allocateAhead s routes partialroutes route train (prevState,state) noNeedForProgress = case routePartExit route of
  Nothing -> return true -- Not relevant for boundary exit routes
  Just signal -> do
    let (occ,_) = state
    let isAllocated = occ .! (rId route) .= Just (tId train)
    let nextRs = routes `startingIn` (Just signal)
    let progressNow = [occ .! (rId r) .= Just (tId train)    | r <- nextRs ]
    progressFuture <- newLit s
    addClause s ([neg isAllocated, progressFuture] ++ progressNow)

    forM_ prevState $ \prev -> do 
      putStrLn $ "will resolve conflict for " ++ (show (routePartName route))
      conflictResolved <- resolveConflictWith s routes partialroutes train (prev, state) nextRs
-- FIXED: here was a problem: if you resolve in the future, you might resolve multiple times in the future,
-- because resolving doesn't force the progressFuture to be false.
--
-- IF you don't allocate, WHEN you do it has to be a conflict resolved
--  needForProgress AND nextRouteActivated => conflictResolved
--
      anyProgress <- orl s progressNow
      addClause s ([noNeedForProgress, neg anyProgress] ++ conflictResolved)
      addClause s ([noNeedForProgress, progressFuture] ++ conflictResolved)
    return (neg progressFuture)

bornCondition :: Solver -> NodeMap -> [RoutePart] -> [[RoutePartId]] -> Train -> (Maybe OccOv, OccOv) -> Lit -> IO Lit
bornCondition s nodeMap routes partialroutes train (prevState,state) bornBefore = do
   let (occ,_) = state
   let prevOcc = fmap fst prevState
   -- Is this train born in this step?
   let trainBirthPlaces = [ route
                          | rid <- nodesToRoutes nodeMap (head (trainVisits train))
                          , route <- routes
                          , rid == routePartName route ]
   let bornNowAlternatives = 
         [ catMaybes [ fmap (\prev -> neg (prev .! (rId route) .= Just (tId train))) prevOcc,
                       Just (occ .! (rId route) .= Just (tId train)) ]
         | route <- trainBirthPlaces ]

   -- Don't get born in other places
   sequence_ [ addClause s [ neg (occ .! (rId r) .= Just (tId train)) ]
             | r <- routes `startingIn` Nothing , not ((rId r) `elem` (fmap rId trainBirthPlaces)) ]

   bornNow <- orl s =<< mapM (andl s) bornNowAlternatives
   bornFuture <- newLit s   -- Or is it born sometime in the future?
   exactlyOne s [bornBefore, bornNow, bornFuture]
   --let trainBirthPlaces = head $ filter (\r -> (rId r) == head (trainVisits train)) routes 
   forM_ prevState $ \prev -> do 
       -- If train is born, and we are not on the first step,
       -- then it must be after a conflict has been resolved.
       conflictResolved <- resolveConflictWith s routes partialroutes train (prev, state) trainBirthPlaces 
       --resolvedAlternatives <- forM trainBirthPlaces $ \trainBirthPlace -> do
       --  let nextRs = [trainBirthPlace]
         --let hadConflict = [ [ neg (prev .! conflicting .= Nothing),
         --                      neg (prev .! (rId trainBirthPlace) .= Just (tId train)),
         --                      state .! (rId trainBirthPlace) .= Just (tId train) ]
         --                  | conflicting <- ((rId trainBirthPlace):
         --                                   (wholeRouteConflicts routes partialroutes (rId trainBirthPlace))) ]
         -- conflictResolved <- mapM (andl s) hadConflict
         -- conflictResolved :: [Lit] is a list of ways that the given
         -- birth place could have had a conflict which is resolved.
         --return conflictResolved
       addClause s ([neg bornNow] ++ conflictResolved)
   --
   -- If train exists, it must have been born
   sequence_ [ addClause s [ neg (occ .! (rId r) .= (Just (tId train))), bornNow, bornBefore]
             | r <- routes ]


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
        putStrLn $ showSchedule schedule

        putStrLn "*** Checking for route activation loops"
                
        case LoopCheck.check routes (fmap trainName trains) schedule of
          Just loop@(stateNumber, train, loopRoutes) ->  do
            putStrLn $ "Found loop: " ++ (show loop)

            -- remove loop
            addClause s [ neg (x .= (Just train)) 
                        | (r,x) <- occupation (states !! stateNumber)
                        , r `elem` loopRoutes ]

            -- and solve again with same parameters
            solveAndTest failedSteps s n states 

          Nothing -> do
            putStrLn "*** No loops -- running user test (simulator)"
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

