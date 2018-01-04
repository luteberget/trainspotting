module Main where

import TrainPlan.Infrastructure
import TrainPlan.UsagePattern
import TrainPlan.Schedule
import TrainPlan.DefaultRoutes
import qualified TrainPlan.Parser
import qualified TrainPlan.Solver as Solver

import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad.State

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

rationalAccel :: Double -> Rational
rationalAccel a = (toRational $ round (a*(fromIntegral precision)))
                    /(fromIntegral precision)
  where 
    precision = 10 :: Integer

solverInput  :: Infrastructure -> UsagePattern -> ([Solver.Route],[Solver.Segment],[Solver.Train])
solverInput is up = (routes,segments,trains)
  where
    (segments,signals) = mkSegments is
    routes = mkRoutes segments signals
    trains = fmap train (zip [0..] (movements up))

    train :: (Int, MovementSpec) -> Solver.Train
    train (i,(MovementSpec typeRef enter visits exit)) = Solver.Train i 
        (round $ vehicleLength t) (round $ vehicleMaxVelocity t) -- TODO pass max acc/brake
      where t = getTrainType typeRef

    getTrainType :: VehicleRef -> Vehicle
    getTrainType ref = fromMaybe (error $ "unknown vehicle: " ++ ref) $ 
                         listToMaybe [ v | v <- (vehicles up), ref == vehicleName v]

fresh :: State Int Int
fresh = do
  i <- get
  modify' (+1)
  return i

mkSegments :: Infrastructure -> ([Solver.Segment], [SignalLocation])
mkSegments is = evalState go 0
  where 
    go :: State Int ([Solver.Segment], [SignalLocation])
    go = do 
      numberedTracks <- sequence [ do i <- fresh
                                      return (i,t)
                                 | t <- tracks is]
      segmentsAndSignals <- sequence [ mkTrackSegments d | d <- trackData numberedTracks]

      let segments = join (map fst segmentsAndSignals)
      let signals  = join (map snd segmentsAndSignals)

      return (segments, signals)

    trackData :: [(Int,Track)] -> [(Int, Track, [Int], [Component])]
    trackData ts = [(i,t,nexts t, tcomponents t) | (i,t) <- ts]
      where
        nexts t = [ fst (trackById ts nt) | (Node from to) <- nodes is
                                          , (trackId t) `elem` from
                                          , nt <- to ]

        tcomponents t = (tsignals t) ++ (tdetectors t)
        tsignals t   = [ s | s@(Signal _ ((Location tref _),_)) <- components is
                           , tref == (trackId t) ]
        tdetectors t = [ d | d@(Detector (Location tref _)) <- components is
                           , tref == (trackId t) ]

    trackById :: [(Int,Track)] -> TrackRef -> (Int, Track)
    trackById ts id = fromMaybe (error $ "Track not found " ++ (show id)) $
                    listToMaybe [ (i,x) | (i,x) <- ts, (trackId x) == id]
    
    mkTrackSegments :: (Int, Track, [Int], [Component]) 
                 -> State Int ([Solver.Segment], [(Int,Int)])
    mkTrackSegments (id,track,trackNextIds,components) = do
      let orderedComponents = sortBy (comparing componentLocation) components
      splitIds <- sequence [ fresh | _ <- orderedComponents ]

      let ids = id : splitIds
      let nextIds = (fmap (\x -> [x]) splitIds) ++ [trackNextIds]
      -- TODO fix allow components on borders?
      let locations = fmap (posLength.componentLocation) orderedComponents
      let sgLengths = [ n - p | (p,n) <- succPairs ((0:locations) 
                                           ++ [trackLength track])]

      let maxVelocity = 20 -- TODO
      let segments = [ Solver.Segment sid (round slen) maxVelocity snexts
                     | (sid,snexts,slen) <- zip3 ids nextIds sgLengths ]
      
      let signals = [ (s1,s2) 
                    | ((s1,s2),(Signal _ _)) <- 
                        zip (succPairs ids) orderedComponents ]

      return (segments, signals)

componentLocation :: Component -> Location
componentLocation (Signal _ (loc,_)) = loc
componentLocation (Detector loc) = loc

succPairs :: [a] -> [(a,a)]
succPairs [] = []
succPairs [x] = []
succPairs xs = zip xs (tail xs)

solve :: Infrastructure -> UsagePattern -> IO (Maybe Schedule)
solve is up = do 
  logmsg "INFRASTRUCTURE"
  logmsg (show is)
  logmsg "USAGE PATTERN"
  logmsg (show up)
  let (routes,segments,trains) = solverInput is up
  logmsg "CONVERTED"
  logmsg "ROUTES"
  logmsg (show routes)
  logmsg "SEGMENTS"
  logmsg (show segments)
  logmsg "TRAINS"
  logmsg (show trains)
  undefined

formatSchedule :: Schedule -> String
formatSchedule = undefined
