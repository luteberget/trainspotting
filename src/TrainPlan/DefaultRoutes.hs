module TrainPlan.DefaultRoutes where

--
-- Guess train routes from layout
--
--

import TrainPlan.Solver
import Data.Maybe (fromMaybe, listToMaybe)

-- Signals are located at (non-branching) segment borders
type SignalLocation = (SegmentId,SegmentId) 

mkRoutes :: [Segment] -> [SignalLocation] -> [Route]
mkRoutes segments sigLocs = fmap solverRoute routes
  where
    solverRoute :: (SignalLocation, SignalLocation, [Segment]) -> Route
    solverRoute (start,end,path) = Route 
                                     (snd start)
                                     (snd end)
                                     (fmap segmentId path)
                                     (sum (fmap segmentLen path))
                                     [] -- TODO: allocation constraints

    routes :: [(SignalLocation, SignalLocation, [Segment])]
    routes = [(start,end,path) | start <- sigLocs
                               , (end,path) <- adjacentSignals [] (snd start) ]

    signalsByPrevSegment s = listToMaybe $ filter 
      (\x -> (fst x) == (segmentId s)) sigLocs

    adjacentSignals :: [Segment] -> SegmentId -> [(SignalLocation, [Segment])]
    adjacentSignals basePath sgId = 
      case signalsByPrevSegment sg of
        Just goal -> [(goal,path)]
        Nothing -> concat [adjacentSignals path next
                          | next <- segmentNexts sg ]
      where
        path = basePath ++ [sg]
        sg = segmentById segments sgId
    
segmentById :: [Segment] -> SegmentId -> Segment
segmentById segments id = fromMaybe (error $ "Segment not found " ++ (show id)) $
                            listToMaybe [ x | x <- segments, (segmentId x) == id]


