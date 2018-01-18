module Main where

import qualified TrainPlan.Parser
import qualified TrainSim.Builder
import qualified TrainPlan.Convert
import qualified TrainPlan.Solver

import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure,exitSuccess)

logmsg = hPutStrLn stderr
output = putStrLn

type TrainId = Int
type RouteId = Int
type Plan = [PlanItem]
data PlanItem = PlanEntry RouteId TrainId | PlanRoute RouteId

main = do
  input <- TrainPlan.Parser.parseStdin
  case input of
    Left err -> do
      logmsg $ "Parse error: " ++ err
      exitFailure
    Right (infrastructure,usagepattern,_) -> do
      logmsg (show infrastructure)
      let (planRoutes,planTrains) = TrainPlan.Convert.convert infrastructure usagepattern
      logmsg "PLAN ROUTES"
      logmsg $ show planRoutes
      logmsg "PLAN TRAINS"
      logmsg $ show planTrains
      TrainSim.Builder.withInfrastructureSimulator infrastructure $ \sim -> do
        logmsg "planning"
        final <- TrainPlan.Solver.plan planRoutes planTrains $ \plan -> do
          logmsg "FOUND PLAN"
          logmsg "simulating"
          return False
        logmsg "quitting"
        return ()
        --let simPlan = TrainSim.Convert.convertPlan infrastructure usagepattern plan
        -- TrainSim.Simulator.test sim simPlan
