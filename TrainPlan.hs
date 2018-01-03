module Main where

import TrainPlan.Infrastructure
import TrainPlan.UsagePattern
import TrainPlan.Schedule
import qualified TrainPlan.Parser
import qualified TrainPlan.Solver as Solver

import Data.Maybe (listToMaybe, fromMaybe)

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
      schedule <- solve infrastructure usagepattern
      case schedule of
        Nothing -> output "No plan found"
        Just schedule -> do
          output "Plan found"
          output (formatSchedule schedule)

solverInput  :: Infrastructure -> UsagePattern ->  ([Solver.Route],[Solver.Segment],[Solver.Train])
solverInput is up = (undefined,undefined,fmap train (zip [0..] $ movements up))
  where
    train :: (Int, MovementSpec) -> Solver.Train
    train (i,(MovementSpec typeRef visits)) = Solver.Train i 
        (round $ vehicleLength t) (round $ vehicleMaxVelocity t) -- TODO pass max acc/brake
      where t = getTrainType typeRef

    getTrainType :: VehicleRef -> Vehicle
    getTrainType ref = fromMaybe (error $ "unknown vehicle: " ++ ref) $ listToMaybe 
      [ v | v <- (vehicles up), ref == vehicleName v]

solve :: Infrastructure -> UsagePattern -> IO (Maybe Schedule)
solve is up = do 
  let (_,_,x) = solverInput is up
  output (show x)
  undefined

formatSchedule :: Schedule -> String
formatSchedule = undefined
