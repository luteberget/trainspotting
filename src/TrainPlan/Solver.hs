module TrainPlan.Solver (
   SegmentId, TrainId, RouteId,
   Route(..),
   Segment(..),
   Train(..),
   Location(..),
 ) where

--
-- TODO: segment border now represents both track branching and 
-- detection section borders. Might need to separate them.
--

import Data.Ratio

import Control.Monad (forM,forM_)
import SAT
import SAT.Bool
import SAT.Equal
import SAT.Order
import SAT.Val
import SAT.Term
import SAT.Value

--------------------------------------------------------------------------------

type SegmentId = Int
type TrainId   = Int
type RouteId   = Int

-- A Route is an allowed allocation of segments to a train,
-- which extends a train's movement authority from
-- the given start segment to the given end segment
-- by allocating the given path segments.
-- The length of the train's movement authority is extended
-- by the given route length.
-- Routes can only be allocated from the given AllocatesFrom list,
-- because of the communcation constraint on the interlocking communicating
-- with the train. 
--  ( For lamp signalling, the limitation is allocating the segments within 
--    sighting distance from a main/distant signal with information about the route.
--    For ERTMS, there is no limitation. )
data Route = Route
  { routeStartSegment :: SegmentId
  , routeEndSegment :: SegmentId
  , routePath :: [SegmentId]
  , routeLength :: Integer
  , routeAllocatesFrom :: [SegmentId] -- TODO: Maybe [SegmentId], where ERTMS has Nothing (no constraint).
  }
  deriving ( Eq, Ord, Show )

data Segment
  = Segment
  { segmentId     :: SegmentId
  , segmentLen    :: Integer
  , segmentMaxVel :: Integer
  , segmentNexts  :: [SegmentId]
  }
 deriving ( Eq, Ord, Show )

data Location
  = Location
  { frontLoc       :: SegmentId
  , frontOffsetLoc :: Int
  , backLoc        :: SegmentId
  , backOffsetLoc  :: Int
  -- TODO velocity constraint here?
  }
 deriving ( Eq, Ord, Show )

data Train
  = Train
  { trainId     :: TrainId
  , trainLen    :: Integer
  , trainMaxVel :: Integer
  -- , trainMaxAcc :: Rational
  -- , trainMaxBrk :: Rational
  -- , trainEnter  :: [Location]
  -- , trainExit   :: [Location]
  }
 deriving ( Eq, Ord, Show )

type System = ([Route],[Segment],[Train])

--------------------------------------------------------------------------------

-- State      = segment occupation, train front/back, train velocity, movement authority length
-- Transition = delta time, train travel distances

data State
  = State
  { segments :: [(SegmentId,Val (Maybe TrainId))]
  , trains   :: [(TrainId,TrainState)]
  }
 deriving ( Eq, Ord, Show )

data TrainState
  = TrainState
  { front       :: Val SegmentId
  , frontOffset :: Term
  , back        :: Val SegmentId
  , backOffset  :: Term
  , trainVel    :: Term
  , authorityLength  :: Term
  , authoritySegment :: Val SegmentId
  -- , trainActive :: Lit
  }
 deriving ( Eq, Ord, Show )

mkState :: System -> [(TrainId,SegmentId,Integer,Integer,SegmentId)] -> State
mkState (routes,tks,tns) tts =
  State
  { segments = [ (i, val (head $ [ Just j | (j,i',_,_,_) <- tts, i'==i ] ++ [Nothing]))
               | t <- tks
               , let i = segmentId t
               ]
  , trains   = [ (i, TrainState (val j) (number (pos+l)) (val j) (number (pos)) (number 0) 
                 (number al) (val as))
               | t <- tns
               , let i = trainId t
                     l = trainLen t
                     (j,pos,al,as) = head [ (j,posi,ali,asi) | (i',j, posi, ali, asi) <- tts, i' == i ]
               ]
  }

newState :: Solver -> System -> IO State
newState s (routes,tks,tns) =
  do xs <- sequence [ newVal s (Nothing : [ Just (trainId t) | t <- tns ])
                    | t <- tks
                    ]
     qs <- sequence [ newTrainState s tks ((map routeEndSegment routes) ++ [2,4]) t | t <- tns ]

     -- front and back occupy segment
     sequence_
       [ do addClause s [ neg (fr .= j), mytrain .= Just i ]
            addClause s [ neg (bk .= j), mytrain .= Just i ]
       | (t,q) <- tns `zip` qs
       , let fr = front q
             bk = back q
             i  = trainId t
       , (t,mytrain) <- tks `zip` xs
       , let j = segmentId t
       ]
     
     -- trains don't go faster than their occupied segments
     -- TODO: can share stuff here
     sequence_
       [ lessThanEqualOr s [neg (mytrain .= Just i)]
                           (trainVel q) (number (segmentMaxVel t))
       | (t,q) <- tns `zip` qs
       , let i = trainId t
       , (t,mytrain) <- tks `zip` xs
       ]

     return $ State
       { segments = [ (segmentId t,x) | (t,x) <- tks `zip` xs ]
       , trains   = [ (trainId t,q)   | (t,q) <- tns `zip` qs ]
       }

newTrainState :: Solver -> [Segment] -> [SegmentId] -> Train -> IO TrainState
newTrainState s tks endpoints t =
  do fr  <- newVal  s is
     fro <- newTerm s l
     bk  <- newVal  s is
     bko <- newTerm s l
     v   <- newTerm s (trainMaxVel t)
     authLength <- newTerm s 2000
     authSegment <- newVal s endpoints
     
     -- offsets fit in current segments
     -- TODO: these can be aggregated for different segments of the same length
     sequence_
       [ do lessThanEqualOr s [neg (fr .= i)] fro (number l)
            lessThanEqualOr s [neg (bk .= i)] bko (number l)
       | t <- tks
       , let i = segmentId t
             l = segmentLen t
       ]

     -- Movement authority constraint:
     -- Required braking distance at this velocity is less than the length of movement authority.
     -- TODO: add perception-reaction distance (reaksjonstid(?) og tilsettingstid for brems)
     -- v^2  < 2 a d
     let p = 2*numerator maxAcc
         q = denominator maxAcc
     v2 <- multiply s v v
     lessThanEqual s (q .* v2) (p .* authLength)

     return $ TrainState
       { front       = fr
       , frontOffset = fro
       , back        = bk
       , backOffset  = bko
       , trainVel    = v
       , authorityLength   = authLength
       , authoritySegment  = authSegment
       }
 where
  is = map segmentId tks
  l  = maximum [ segmentLen t - 1 | t <- tks ]

--------------------------------------------------------------------------------

data Step
  = Step
  { time      :: Term
  , distances :: [(TrainId,Term)]
  }
 deriving ( Eq, Ord, Show )

newStep :: Solver -> System -> State -> State -> IO Step
newStep s (routes,tks,tns) s1 s2 =
  do tm <- newTerm s 500 -- what should this be?
     ds <- sequence [ newTerm s l | t <- tns ]

     -- segment occupance
     -- TODO: WORK
     sequence_
       [ equalOr s [occ .= Nothing, occ' .= Nothing] occ occ'
       | ((_,occ),(_,occ')) <- segments s1 `zip` segments s2
       ]

     -- There is already no way to deallocate a segment without 
     -- running a train over it (zugmitwirkung)
     -- because of the following constraint:
     --
     
     -- segment free => was free or back of train was there
     sequence_
       [ addClause s ( neg (occ' .= Nothing)
                     : occ .= Nothing
                     : [ back q .= i | (_,q) <- trains s1 ]
                     )
       | ((i,occ),(_,occ')) <- segments s1 `zip` segments s2
       ]
     
     -- train movement
     sequence_
       [ trainStep s (routes,tks,tns) tm d s1 s2 i t1 t2
       | (((i,t1),(_,t2)),d) <- (trains s1 `zip` trains s2) `zip` ds
       ]

     return $ Step
       { time      = tm
       , distances = [ (trainId t,d) | (t,d) <- tns `zip` ds ]
       }
 where
  l = maximum ([segmentLen t1 | t1 <- tks] ++ [ segmentLen t1 + segmentLen t2 - 1
              | t1 <- tks
              , i <- segmentNexts t1
              , t2 <- tks
              , segmentId t2 == i
              ])

trainStep :: Solver -> System -> Term -> Term -> State -> State -> TrainId -> TrainState -> TrainState -> IO ()
trainStep s (routes,tks,tns) tm d s1 s2 i t1 t2 =
  do -- if front moves to a new piece, it was not already occupied
     -- BJLUT: removed this after adding alloaction logic
     ---    sequence_
     ---      [ do addClause s [ neg (front t2 .= j)
     ---                       , front t1 .= j
     ---                       , occ .= Nothing
     ---                       ]
     ---      | (t,(_,occ)) <- tks `zip` segments s1
     ---      , let j = segmentId t
     ---      ]
  
     -- if back moves to a new piece, it was already occupied by the same train, or the new front is there too
     ---   sequence_
     ---     [ do addClause s [ neg (back t2 .= j)
     ---                      , back t1 .= j
     ---                      , occ .= Just i
     ---                      , front t2 .= j
     ---                      ]
     ---     | (t,(_,occ)) <- tks `zip` segments s1
     ---     , let j = segmentId t
     ---     ]
  
     -- TODO: ONLY allocate through routes?
     -- is this necessary?
     -- No, it should not be necessary. If we reserve segments without route allocation,
     -- the movement authority will not be extended. (authSegment and authLength are fully constrained)
     -- If we reserve unneccesarily, the segments cannot be freed without running a train, so unneccessary
     -- reservation can only happen if the route allocation will happen later anyway, or the segment
     -- is not needed by any train for the rest of the time.
     --

     -- front and back move at most 1 segment, and an equal distance d
     sequence_
       [ do move t occ1 front frontOffset d
            move t occ1 back  backOffset  d
       | (t,(_,occ1)) <- tks `zip` (segments s1)
       ]

     -- Movement authority constraints:
     -- The authority is decreased by travel, increased by allocation
     --
     noAlloc <- isEqual s (authoritySegment t2) (authoritySegment t1)
     equalOr s [neg noAlloc] (authorityLength t2) (authorityLength t1 .-. d)

     allocs <- forM routes $ \route -> do
       -- If we allocated this route in this step,
       didAlloc <- andl s [neg noAlloc, (authoritySegment t2) .= (routeEndSegment route)]

       -- It must extend the current target (authority segment)
       addClause s [neg didAlloc, (authoritySegment t1) .= (routeStartSegment route)]

       -- And we must already be in a segment which has sight to the route
       addClause s (neg didAlloc : [front t1 .= allocSegmentId | allocSegmentId <- routeAllocatesFrom route])

       -- Adjust length of movement authority
       -- TODO: join to a single addition with [(routeLength route, didAlloc) | .. <- .. ] ?
       equalOr s [neg didAlloc] (authorityLength t2) (authorityLength t1 .+. (number (routeLength route)) .-. d)

       -- And allocate the path
       sequence_
         [ do addClause s [neg didAlloc, pathOcc .= Just i]
         | (t, (_, pathOcc)) <- tks `zip` (segments s2),
           (segmentId t) `elem` (routePath route)]
       return didAlloc

     addClause s (noAlloc : allocs)

     -- velocities, distance and time match
     let vdiff = trainVel t1 .-. trainVel t2
     vdiff2 <- multiply s vdiff vdiff
     tv1    <- multiply s tm (trainVel t1)
     tv2    <- multiply s tm (trainVel t2)
     
     -- check if v1 >= v2
     grt <- newLit s
     greaterThanEqualOr s [neg grt] vdiff (number 0)
     greaterThanEqualOr s [grt]     (number 0) vdiff
     
     -- d should be between two areas
     let p = 2*numerator maxDec
         q = denominator maxDec
     lessThanEqualOr s [neg grt] (p .* d) (p .* tv1 .-. (q .* vdiff2))
     lessThanEqualOr s [neg grt] (p .* tv2 .+. (q .* vdiff2)) (p .* d)

     let p = 2*numerator maxAcc
         q = denominator maxAcc
     lessThanEqualOr s [grt] (p .* d) (p .* tv2 .-. (q .* vdiff2))
     lessThanEqualOr s [grt] (p .* tv1 .+. (q .* vdiff2)) (p .* d)
     
     return ()
 where
  l = maximum [ segmentLen t | t <- tks ]
 
  move t occ1 whr off d =
    do addClause s ( neg (whr t1 .= (segmentId t))
                   : (whr t2 .= (segmentId t))
                   : [ whr t2 .= j | j <- segmentNexts t ]
                   )
       -- TODO: optimization: with known movement authority, we only need to allocate (mark)
       --       segments to trains at branches?

       -- move into allocated area
       -- Although the movement authority already stops the train from going too far,
       -- this constrant is needed to choose the correct path in a branch.
       addClause s [ neg (whr t2 .= (segmentId t)), -- if train front/back goes into t in next state, 
                    occ1 .= Just i] -- then the segment must already have been allocated to the train.

       -- TODO: some of this may be shared
       equalOr s [neg (whr t1 .= (segmentId t))]
                 (off t1 .+. d)
                 (off t2 .+. fromList [(segmentLen t,neg (whr t2 .= (segmentId t)))])

--------------------------------------------------------------------------------

maxAcc, maxDec :: Rational
maxAcc = 2
maxDec = 1

sys :: System
sys = ( [ Route 2 4 [2,3] (200) [1]
        , Route 2 6 [2,5,4] (300) [1]
        ]
      ,
        [ Segment 1 100 50 [2]
        , Segment 2 100 50 [3,5]
        , Segment 3 100 50 [4]
        , Segment 4 100 50 [6]
        , Segment 5 100 50 [4]
        , Segment 6 100 50 []
        ]
      , [ Train 1 50 50
        ]
      )

main :: IO ()
main =
  plan sys
       (mkState sys [(1,1,0,50,2)])
       4
       (mkState sys [(1,4,40,10,6)])

plan :: System -> State -> Int -> State -> IO ()
plan sys s0 n sz =
  withNewSolver $ \s ->
    do 
       -- solverVerbosity s 1000
       putStrLn "+++ creating..."
       states <- sequence [ newState s sys | i <- [1..n] ]
       let states' = s0 : states ++ [sz]
       steps <- sequence [ newStep s sys st st' | (st,st') <- states' `zip` tail states' ]
       
       -- arbitrary time constraint, just a test
       let total_time = (foldr1 (.+.) [ time stp | stp <- steps ])
       lessThanEqual s total_time (number 41)
       
       putStrLn "+++ solving..."
       b <- solve s []
       if b then
         do putStrLn "*** solution:"

            let printState stt =
                  do sequence_ [ printTrain i tst | (i,tst) <- trains stt ]

                printTrain i tst =
                  do putStr ("T" ++ show i ++ ": ")
                     fr  <- getValue s (front tst)
                     fro <- getValue s (frontOffset tst)
                     bk  <- getValue s (back tst)
                     bko <- getValue s (backOffset tst)
                     v   <- getValue s (trainVel tst)
                     target   <- getValue s (authoritySegment tst)
                     authlen   <- getValue s (authorityLength tst)
                     putStr ("S" ++ show bk ++ " +" ++ show bko ++ "m ~~ ")
                     putStr ("S" ++ show fr ++ " +" ++ show fro ++ "m ")
                     putStr ("@ " ++ show v ++ "m/s ")
                     putStr $ "target=" ++ (show target) ++ ", authlen=" ++ (show authlen)
                     putStrLn ""

                printStep stp =
                  do putStr "-- "
                     t <- getValue s (time stp)
                     putStr (show t ++ "s | ")
                     sequence_ [ do d <- getValue s dv
                                    putStr ("T" ++ show i ++ ": +" ++ show d ++ "m ")
                               | (i,dv) <- distances stp
                               ]
                     putStrLn "-->"

            sequence_ [ do printState stt
                           printStep stp
                      | (stt,stp) <- states' `zip` steps
                      ]
            printState (last states')
        else
         do putStrLn "*** no solution"


