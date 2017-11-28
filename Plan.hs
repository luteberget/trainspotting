module Plan where

import Data.Ratio

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

data Segment
  = Segment
  { segmentId     :: SegmentId
  , segmentLen    :: Integer
  , segmentMaxVel :: Integer
  , segmentNexts  :: [SegmentId]
  }
 deriving ( Eq, Ord, Show )

data Train
  = Train
  { trainId     :: TrainId
  , trainLen    :: Integer
  , trainMaxVel :: Integer
  }
 deriving ( Eq, Ord, Show )

type System = ([Segment],[Train])

--------------------------------------------------------------------------------

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
  }
 deriving ( Eq, Ord, Show )

mkState :: System -> [(TrainId,SegmentId)] -> State
mkState (tks,tns) tts =
  State
  { segments = [ (i, val (head $ [ Just j | (j,i') <- tts, i'==i ] ++ [Nothing]))
               | t <- tks
               , let i = segmentId t
               ]
  , trains   = [ (i, TrainState (val j) (number l)
                                (val j) (number 0) (number 0))
               | t <- tns
               , let i = trainId t
                     l = trainLen t
                     j = head [ j | (i',j) <- tts, i' == i ]
               ]
  }

newState :: Solver -> System -> IO State
newState s (tks,tns) =
  do xs <- sequence [ newVal s (Nothing : [ Just (trainId t) | t <- tns ])
                    | t <- tks
                    ]
     qs <- sequence [ newTrainState s tks t | t <- tns ]

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

newTrainState :: Solver -> [Segment] -> Train -> IO TrainState
newTrainState s tks t =
  do fr  <- newVal  s is
     fro <- newTerm s l
     bk  <- newVal  s is
     bko <- newTerm s l
     v   <- newTerm s (trainMaxVel t)
     
     -- offsets fit in current segments
     -- TODO: these can be aggregated for different segments of the same length
     sequence_
       [ do lessThanEqualOr s [neg (fr .= i)] fro (number l)
            lessThanEqualOr s [neg (bk .= i)] bko (number l)
       | t <- tks
       , let i = segmentId t
             l = segmentLen t
       ]
     
     return $ TrainState
       { front       = fr
       , frontOffset = fro
       , back        = bk
       , backOffset  = bko
       , trainVel    = v
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
newStep s (tks,tns) s1 s2 =
  do tm <- newTerm s 120 -- what should this be?
     ds <- sequence [ newTerm s l | t <- tns ]

     -- segment occupance
     -- TODO: WORK
     sequence_
       [ equalOr s [occ .= Nothing, occ' .= Nothing] occ occ'
       | ((_,occ),(_,occ')) <- segments s1 `zip` segments s2
       ]
     
     sequence_
       [ addClause s ( neg (occ' .= Nothing)
                     : occ .= Nothing
                     : [ back q .= i | (_,q) <- trains s1 ]
                     )
       | ((i,occ),(_,occ')) <- segments s1 `zip` segments s2
       ]
     
     -- train movement
     sequence_
       [ trainStep s (tks,tns) tm d s1 i t1 t2
       | (((i,t1),(_,t2)),d) <- (trains s1 `zip` trains s2) `zip` ds
       ]

     return $ Step
       { time      = tm
       , distances = [ (trainId t,d) | (t,d) <- tns `zip` ds ]
       }
 where
  l = maximum [ segmentLen t1 + segmentLen t2 - 1
              | t1 <- tks
              , i <- segmentNexts t1
              , t2 <- tks
              , segmentId t2 == i
              ]

trainStep :: Solver -> System -> Term -> Term -> State -> TrainId -> TrainState -> TrainState -> IO ()
trainStep s (tks,tns) tm d s1 i t1 t2 =
  do -- if front moves to a new piece, it was not already occupied
     sequence_
       [ do addClause s [ neg (front t2 .= j)
                        , front t1 .= j
                        , occ .= Nothing
                        ]
       | (t,(_,occ)) <- tks `zip` segments s1
       , let j = segmentId t
       ]
  
     -- if back moves to a new piece, it was already occupied by the same train, or the new front is there too
     sequence_
       [ do addClause s [ neg (back t2 .= j)
                        , back t1 .= j
                        , occ .= Just i
                        , front t2 .= j
                        ]
       | (t,(_,occ)) <- tks `zip` segments s1
       , let j = segmentId t
       ]
  
     -- front and back move at most 1 segment, and an equal distance d
     sequence_
       [ do move t front frontOffset d
            move t back  backOffset  d
       | t <- tks
       ]

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
 
  move t whr off d =
    do addClause s ( neg (whr t1 .= i)
                   : (whr t2 .= i)
                   : [ whr t2 .= j | j <- segmentNexts t ]
                   )
       -- TODO: some of this may be shared
       equalOr s [neg (whr t1 .= i)]
                 (off t1 .+. d)
                 (off t2 .+. fromList [(segmentLen t,neg (whr t2 .= i))])
   where
    i = segmentId t

--------------------------------------------------------------------------------

maxAcc, maxDec :: Rational
maxAcc = 2
maxDec = 1

sys :: System
sys = ( [ Segment 1 100 50 [3]
        , Segment 2 100 50 [3]
        , Segment 3 1   50 [4,5]
        , Segment 4 100 50 []
        , Segment 5 100 50 []
        ]
      , [ Train 1 50 50
        , Train 2 50 50
        ]
      )

main :: IO ()
main =
  plan sys
       (mkState sys [(1,1),(2,2)])
       3
       (mkState sys [(1,4),(2,2)])

plan :: System -> State -> Int -> State -> IO ()
plan sys s0 n sz =
  withNewSolver $ \s ->
    do putStrLn "+++ creating..."
       states <- sequence [ newState s sys | i <- [1..n] ]
       let states' = s0 : states ++ [sz]
       steps <- sequence [ newStep s sys st st' | (st,st') <- states' `zip` tail states' ]
       
       -- arbitrary time constraint, just a test
       lessThanEqual s (foldr1 (.+.) [ time stp | stp <- steps ]) (number 60)
       
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
                     putStr ("S" ++ show bk ++ " +" ++ show bko ++ "m ~~ ")
                     putStr ("S" ++ show fr ++ " +" ++ show fro ++ "m ")
                     putStrLn ("@ " ++ show v ++ "m/s")

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


