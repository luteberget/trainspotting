{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified TrainPlan.Simulator as Sim
import qualified TrainPlan.Convert as Convert
import qualified TrainPlan.SolverUnique as Solver
import qualified TrainPlan.SolverInput as SolverInput
import qualified TrainPlan.Parser as Parser
import qualified TrainPlan.Timing as Timing
import qualified TrainPlan.UsagePattern as Usage
import qualified TrainPlan.LoopCheck as LoopCheck

import System.Console.CmdArgs
import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure,exitSuccess)
import System.CPUTime
import Data.IORef
import Data.Maybe (fromMaybe)

logmsg = hPutStrLn stderr
output = putStrLn

data RailPerfCheck 
  = RailPerfCheck 
  { infrastructureFile :: FilePath
  , routesFile :: FilePath
  , usageFile :: FilePath
  , planoutput :: Maybe FilePath
  , resultjsonoutput :: Maybe FilePath
  , dontsimulate :: Bool
  } deriving (Show, Data, Typeable)

optSpec = RailPerfCheck
  { infrastructureFile = def &= typ "INFRFILE" &= argPos 0
  , routesFile = def &= typ "ROUTESFILE" &= argPos 1
  , usageFile = def &= typ "USAGEFILE" &= argPos 2
  , planoutput = def &= typFile &= help "Output dispatch of successful plan"
  , resultjsonoutput = def &= typFile &= help "Output constraint checklist in JSON format"
  , dontsimulate = def &= help "Don't check timing constraints using simulator"
  } &= summary "railperfcheck v0.1.0"

time :: IO t -> IO (Integer, t)
time f = do
  start <- getCPUTime
  v <- f
  end <- getCPUTime
  return (end-start, v)

conv_time :: Integer -> Double
conv_time x = (fromIntegral x) / (10^12)

main = do
  opts <- cmdArgs optSpec
  let useSimulator = not (dontsimulate opts)
  putStrLn $ "Simulate : " ++ (show useSimulator)

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
              -- putStrLn (show routes)
              -- putStrLn (show usage)

              let numTrains = length (Usage.movements usage) -- TODO
              let maxminSteps = numTrains +3
              let maxFailedSteps = 3

              let solverInput = Convert.solverInput routes usage
              -- putStrLn "ROUTES"
              -- sequence_ [ putStrLn $ " *> " ++ (show r) | r <- (\(a,_,_,_) -> a) solverInput ]
              -- putStrLn $ "PARTIAL ROUTES" ++ (show ((\(_,b,_,_) -> b) solverInput))

              let run d = Sim.dispatchTiming simInf simRoutes d
              let eval h = Timing.evaluate usage h

              -- putStrLn (show solverInput)
              sat_timer <- newIORef =<< getCPUTime
              sat_timer_sum <- newIORef 0
              des_timer_sum <- newIORef 0
              des_counter <- newIORef 0
              plan_counter <- newIORef 0

              final <- Solver.plan maxminSteps maxFailedSteps solverInput $ \plan -> do
 
                t <- getCPUTime
                lastt <- readIORef sat_timer
                modifyIORef sat_timer_sum ((+) (t-lastt))
                writeIORef sat_timer t
                let dispatchString = Convert.dispatchPlan solverInput plan
                putStrLn $ "Converting dispatch string" ++ (show plan)
                putStrLn dispatchString
                putStrLn "Converting dispatch string DONE"
                modifyIORef plan_counter (+ 1)
                if useSimulator then do
                  Sim.withDispatch dispatchString $ \dispatch -> do
                    (sim_t, history) <- time (run dispatch)
                    modifyIORef des_timer_sum ((+) sim_t)
                    modifyIORef des_counter (+ 1)
                    return (eval history)
                else return False

              t <- getCPUTime
              lastt <- readIORef sat_timer
              modifyIORef sat_timer_sum ((+) (t-lastt))

              sat_time <- readIORef sat_timer_sum
              des_time <- readIORef des_timer_sum
              n_des <- readIORef des_counter
              n_plans <- readIORef plan_counter
              putStrLn $ "Number of routes: " ++ (show (length ((\(_,p,_,_) -> p) solverInput)))
              putStrLn $ "Number of partial routes: " ++ (show (length ((\(r,_,_,_) -> r) solverInput)))
              putStrLn $ "Time SAT: " ++ (show (conv_time sat_time))
              putStrLn $ "Time DES: " ++ (show (conv_time des_time))
              putStrLn $ "Number of simulations: " ++ (show n_des)
              putStrLn $ "Number of plans: " ++ (show n_plans)

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
