module Main where

import qualified TrainPlan.Parser
import qualified TrainSim.Builder
import qualified TrainSim.ConvertInput
import qualified TrainPlan.Convert
import qualified TrainPlan.Solver
import Control.Monad (forM, forM_)

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
      -- logmsg "PLAN ROUTES"
      -- sequence_ [ logmsg $ show r | r <- planRoutes ]
      -- logmsg "PLAN TRAINS"
      -- logmsg $ show planTrains
      TrainSim.Builder.withInfrastructureSimulator infrastructure $ \sim -> do
        logmsg "planning"
        final <- TrainPlan.Solver.plan planRoutes planTrains $ \plan -> do
          logmsg "FOUND PLAN"
          logmsg $ show plan
          let simPlan = TrainSim.ConvertInput.convertPlan infrastructure usagepattern plan
          planObj <- TrainSim.Builder.add_plan simPlan
          v <- doSimulation
          logmsg $ show simPlan
          logmsg "simulating"
          return False
        logmsg "quitting"
        return ()
        -- TrainSim.Simulator.test sim simPlan
