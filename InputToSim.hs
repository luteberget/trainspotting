module Main where

import qualified TrainPlan.Parser
import qualified TrainSim.ConvertInput


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
      TrainSim.ConvertInput.toISGraphFormat infrastructure output
      exitSuccess
