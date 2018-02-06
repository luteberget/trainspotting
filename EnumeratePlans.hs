module Main where

import qualified TrainPlan.Parser
import qualified TrainPlan.Convert
import qualified TrainPlan.SolverUnique
import Control.Monad (forM, forM_)

import Data.Map (Map)
import qualified Data.Map as Map

import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure,exitSuccess)

logmsg = hPutStrLn stderr
output = putStrLn

main = do
  input <- TrainPlan.Parser.parseStdin
  case input of
    Left err -> do
      logmsg $ "Parse error: " ++ err
      exitFailure
    Right (infrastructure,usagepattern,_) -> do
      logmsg (show infrastructure)
      let (planRoutes,planTrains) = TrainPlan.Convert.convert infrastructure usagepattern
      logmsg "*-> ROUTES"
      sequence_ [ logmsg $ show r | r <- planRoutes ]
      logmsg "*-> TRAINS"
      logmsg $ show planTrains
      final <- TrainPlan.SolverUnique.plan 10 planRoutes planTrains $ \plan -> do
        return False
      exitSuccess
        

      -- TrainSim.Builder.withInfrastructureSimulator infrastructure $ \sim objmap -> do
      --   logmsg "OBJMAP"
      --   forM_ (Map.toList objmap) $ \(k,v) -> do logmsg $ "k=" ++ (show k) ++ ", v=" ++ (show v)
      --   logmsg "planning"
      --     logmsg "FOUND PLAN"
      --     logmsg $ show plan
      --     let simPlan = TrainSim.ConvertInput.convertPlan infrastructure usagepattern plan objmap
      --     logmsg $ show simPlan
      --     TrainSim.Builder.withPlan simPlan $ \plan -> do
      --       logmsg "simulating"
      --       perf <-  TrainSim.Builder.testPlan sim plan
      --       logmsg ("sim finished: " ++ (show perf))
      --     return False
      --   logmsg "quitting"
      --   return ()
