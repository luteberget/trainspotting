{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified TrainPlan.Simulator as Sim
import qualified TrainPlan.Convert as Convert
import qualified TrainPlan.SolverUnique as Solver
import qualified TrainPlan.Parser as Parser
import qualified TrainPlan.Timing as Timing

import System.Console.CmdArgs

data RailPerfCheck 
  = RailPerfCheck 
  { infrastructureFile :: FilePath
  , routesFile :: FilePath
  , usageFile :: FilePath
  } deriving (Show, Data, Typeable)

optSpec = RailPerfCheck
  { infrastructureFile = def &= typ "INFRFILE" &= argPos 0
  , routesFile = def &= typ "ROUTESFILE" &= argPos 1
  , usageFile = def &= typ "USAGEFILE" &= argPos 2
  } &= summary "railperfcheck v0.1.0"

main = do
  opts <- cmdArgs optSpec

  Sim.withInfrastructureFile (infrastructureFile opts) $ \simInf -> do
  Sim.withRoutesFile simInf (routesFile opts) $ \simRoutes -> do

    putStrLn "hello sim"

    routes <- Parser.parseRoutesFile (routesFile opts)
    usage  <- Parser.parseUsageFile (usageFile opts)

    putStrLn "hello parse"

    let numTrains = 2 -- TODO
    let maxSteps = numTrains^2
    let solverInput = Convert.solverInput routes usage
    let run d = Sim.dispatchTiming simInf simRoutes d
    let eval h = Timing.evaluate usage h

    final <- Solver.plan maxSteps solverInput $ \plan -> do
      Sim.withDispatch (Convert.dispatchPlan plan) $ \dispatch -> do
        history <- run dispatch
        return (eval history)
        
      

    putStrLn "hello world"

