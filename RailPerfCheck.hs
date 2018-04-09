{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified TrainPlan.Simulator as Sim
import qualified TrainPlan.Convert as Convert
import qualified TrainPlan.SolverUnique as Solver
import qualified TrainPlan.Parser as Parser
import qualified TrainPlan.Timing as Timing
import qualified TrainPlan.UsagePattern as Usage

import System.Console.CmdArgs
import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure,exitSuccess)

logmsg = hPutStrLn stderr
output = putStrLn

data RailPerfCheck 
  = RailPerfCheck 
  { infrastructureFile :: FilePath
  , routesFile :: FilePath
  , usageFile :: FilePath
  , planoutput :: Maybe FilePath
  , resultjsonoutput :: Maybe FilePath
  } deriving (Show, Data, Typeable)

optSpec = RailPerfCheck
  { infrastructureFile = def &= typ "INFRFILE" &= argPos 0
  , routesFile = def &= typ "ROUTESFILE" &= argPos 1
  , usageFile = def &= typ "USAGEFILE" &= argPos 2
  , planoutput = def &= typFile &= help "Output dispatch of successful plan"
  , resultjsonoutput = def &= typFile &= help "Output constraint checklist in JSON format"
  } &= summary "railperfcheck v0.1.0"

main = do
  opts <- cmdArgs optSpec

  Sim.withInfrastructureFile (infrastructureFile opts) $ \simInf -> do
  Sim.withRoutesFile simInf (routesFile opts) $ \simRoutes -> do

    routes <- Parser.parseRoutesFile (routesFile opts)
    case routes of
      Left err -> do 
                  putStrLn err
                  exitFailure
      Right routes -> do
          usage  <- Parser.parseUsageFile (usageFile opts)
          case usage of
            Left err -> do 
                        putStrLn err
                        exitFailure
            Right usage -> do
              putStrLn (show routes)
              putStrLn (show usage)

              let numTrains = length (Usage.movements usage) -- TODO
              let maxSteps = numTrains^2 +3

              let solverInput = Convert.solverInput routes usage
              putStrLn "ROUTES"
              sequence_ [ putStrLn $ " *> " ++ (show r) | r <- (\(a,_,_,_) -> a) solverInput ]
              putStrLn $ "PARTIAL ROUTES" ++ (show ((\(_,b,_,_) -> b) solverInput))

              let run d = Sim.dispatchTiming simInf simRoutes d
              let eval h = Timing.evaluate usage h

              putStrLn (show solverInput)
              final <- Solver.plan maxSteps solverInput $ \plan -> do
                let dispatchString = Convert.dispatchPlan solverInput plan
                putStrLn dispatchString
                Sim.withDispatch dispatchString $ \dispatch -> do
                  history <- run dispatch
                  return (eval history)

              case final of
                Just plan -> do
                  case (planoutput opts) of
                    Just planfile -> do
                      let dispatchString = Convert.dispatchPlan solverInput plan
                      writeFile planfile (dispatchString ++ "\n")
                    Nothing -> return ()
                  putStrLn "sat"
                Nothing -> do
                  case (planoutput opts) of
                    Just planfile -> do
                      writeFile planfile ""
                    Nothing -> return ()
                  putStrLn "unsat"

              return ()
