module Main where

import SAT
import SAT.FloatTheory
import Control.Monad (forM_)

segmentLength = 1000.0
maxVelocity = 10.00
maxTime = 5000.0
maxAccel = 2.0
maxBrake = 1.0

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

  brakingDecision <- newLit s 
  isBraking <- mkFloatConstraint fs (v2 .<=. v1)

  let brakeDiff = (square (v1 .-. v2)) .*. (floatConst (1.0/(2.0*maxBrake)))
  let accelDiff = (square (v1 .-. v2)) .*. (floatConst (1.0/(2.0*maxAccel)))

  addClause s [neg brakingDecision, isBraking]
  addClause s [brakingDecision, neg isBraking]
  
  braking1  <- mkFloatConstraint fs (((dt .*. v2) .+. brakeDiff) .<=. dx)
  braking2  <- mkFloatConstraint fs (dx .<=. ((dt .*. v1) .-. brakeDiff))
  addClause s [neg brakingDecision, braking1]
  addClause s [neg brakingDecision, braking2]

  acceling1 <- mkFloatConstraint fs (((dt .*. v1) .+. accelDiff) .<=. dx)
  acceling2 <- mkFloatConstraint fs (dx .<=. ((dt .*. v2) .-. accelDiff))
  addClause s [brakingDecision, acceling1]
  addClause s [brakingDecision, acceling2]

  travelDistance <- mkFloatConstraint fs ((fst s1) .+. dx .==. (fst s2))
  addClause s [travelDistance]

  return (dx,dt)

run maxT = withNewSolver $ \s -> do
  fs <- newFloatSolver s
  
  let startState = (floatConst 0.0, floatConst 0.0)
  midStatePos <- newFloat fs 0.0 segmentLength -- 502.806 502.807--0.0 segmentLength
  midStateVelocity <- newFloat fs 0.0 maxVelocity
  let endState   = (floatConst segmentLength, floatConst 0.0)
  let states = [startState, (midStatePos, midStateVelocity), endState]
  transitions <- sequence [ transition maxT s fs s1 s2 
                          | (s1,s2) <- states `zip` tail states ]

  let sumT = foldl1 (.+.) (map snd transitions)
  constraint <- mkFloatConstraint fs (sumT .<=. (floatConst maxT))
  addClause s [constraint]

  b <- solveWithFloat fs sumT
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
  posVal <- evalFloatExpr fs pos
  velVal <- evalFloatExpr fs vel
  return $ " - State: x=" ++ (show posVal) ++ ", v=" ++ (show velVal)

transitionToString :: FloatSolver -> Transition -> IO String
transitionToString fs (dx, dt) = do 
  dxVal <- evalFloatExpr fs dx
  dtVal <- evalFloatExpr fs dt
  return $ " - Transition: dx=" ++ (show dxVal) ++ ", dt=" ++ (show dtVal)

main = run maxTime
