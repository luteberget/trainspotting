module Main where

import SAT
import SAT.Bool
import SAT.FloatTheory
import Control.Monad (forM_)

segmentLength = 100.0
maxVelocity = 1.00
maxTime = 5000.0
maxAccel = 0.02
maxBrake = 90.0

type Pos = FloatExpr
type Velocity = FloatExpr
type DeltaPos = FloatExpr
type DeltaTime = FloatExpr

type State = (Pos, Velocity)
type Transition = (DeltaPos, DeltaTime)

transition :: Double -> Solver -> FloatSolver -> State -> State -> IO Transition
transition maxT s fs s1 s2 = do
  dx <- newFloat fs 0.0 segmentLength
  dt <- newFloat fs 0.0 maxT
  let (v1,v2) = (snd s1, snd s2)


  let brakeDiff = (square (v1 .-. v2)) .*. (number (1.0/(2.0*maxBrake)))
  let accelDiff = (square (v1 .-. v2)) .*. (number (1.0/(2.0*maxAccel)))

  isBraking  <- constraint fs (v2 .<=. v1)
  braking1  <- constraint fs (((dt .*. v2) .+. brakeDiff) .<=. dx)
  braking2  <- constraint fs (dx .<=. ((dt .*. v1) .-. brakeDiff))

  isAcceling <- constraint fs (v2 .>=. v1)
  acceling1 <- constraint fs (((dt .*. v1) .+. accelDiff) .<=. dx)
  acceling2 <- constraint fs (dx .<=. ((dt .*. v2) .-. accelDiff))

  acc <- andl s [isAcceling, acceling1, acceling2]
  brk <- andl s [isBraking, braking1, braking2]
  addClause s [acc, brk]
  addClause s [neg acc, neg brk]

  assertConstraint fs ((fst s1) .+. dx .==. (fst s2))

  return (dx,dt)

run maxT = withNewSolver $ \s -> do
  fs <- newFloatSolver s
  
  let startState = (number 0.0, number 0.0)
  midStatePos <- newFloat fs 0.0 segmentLength -- 502.806 502.807--0.0 segmentLength
  midStateVelocity <- newFloat fs 0.0 maxVelocity
  let endState   = (number segmentLength, number 0.0)
  let states = [startState, (midStatePos, midStateVelocity), endState]
  transitions <- sequence [ transition maxT s fs s1 s2 
                          | (s1,s2) <- states `zip` tail states ]

  let sumT = foldl1 (.+.) (map snd transitions)
  constraint <- constraint fs (sumT .<=. (number maxT))
  addClause s [constraint]

  b <- solveMinimizeFloat fs sumT
  if b then do
    putStrLn "*** Solution:"
    forM_ (states `zip` transitions) $ \(st,tr) -> do
      putStrLn =<< stateToString fs st
      putStrLn =<< transitionToString fs tr
    putStrLn =<< stateToString fs (last states)
  else do
    putStrLn "*** No solution."

stateToString :: FloatSolver -> State -> IO String
stateToString fs (pos,vel) = do
  posVal <- SAT.FloatTheory.modelValue fs pos
  velVal <- SAT.FloatTheory.modelValue fs vel
  return $ " - State: x=" ++ (show posVal) ++ ", v=" ++ (show velVal)

transitionToString :: FloatSolver -> Transition -> IO String
transitionToString fs (dx, dt) = do 
  dxVal <- SAT.FloatTheory.modelValue fs dx
  dtVal <- SAT.FloatTheory.modelValue fs dt
  return $ " - Transition: dx=" ++ (show dxVal) ++ ", dt=" ++ (show dtVal)

main = run maxTime
