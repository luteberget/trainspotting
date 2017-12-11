module LinespeedTrivial where

import SAT
import SAT.FloatTheory
import Control.Monad (forM_)

segmentLength = 1000.0
maxVelocity = 20.0
maxTime = 1000.0

type Pos = FloatExpr
type Velocity = FloatExpr
type DeltaPos = FloatExpr
type DeltaTime = FloatExpr

type State = (Pos, Velocity)
type Transition = (DeltaPos, DeltaTime)

transition :: FloatSolver -> State -> State -> IO Transition
transition fs s1 s2 = do
  dx <- newFloat fs 0.0 segmentLength
  dt <- newFloat fs 0.0 maxTime
  return (dx,dt)

main = withNewSolver $ \s -> do
  fs <- newFloatSolver s
  
  let startState = (floatConst 0.0, floatConst 0.0)
  midStatePos <- newFloat fs 0.0 segmentLength
  midStateVelocity <- newFloat fs 0.0 maxVelocity
  let endState   = (floatConst segmentLength, floatConst 0.0)
  let states = [startState, (midStatePos, midStateVelocity), endState]
  transitions <- sequence [ transition fs s1 s2 
                          | (s1,s2) <- states `zip` tail states ]

  b <- solveWithFloat fs
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
  posVal <- floatValue pos
  velVal <- floatValue vel
  return $ " - State: x=" ++ (show posVal) ++ ", v=" ++ (show velVal)

transitionToString :: FloatSolver -> Transition -> IO String
transitionToString fs (dx, dt) = do 
  dxVal <- floatValue dx
  dtVal <- floatValue dt
  return $ " - Transition: dx=" ++ (show dxVal) ++ ", dt=" ++ (show dtVal)

