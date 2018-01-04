module TrainPlan.DefaultRoutes where

--
-- Guess train routes from layout
--
--

import TrainPlan.Solver
import Data.Maybe (fromMaybe, listToMaybe)

-- Signals are located at (non-branching) segment borders
type SignalLocation = (Segment,Segment) 

mkRoutes :: [Segment] -> [SignalLocation] -> [Route]
mkRoutes segments sigLocs = fmap solverRoute routes
  where
    solverRoute :: (SignalLocation, SignalLocation, [Segment]) -> Route
    solverRoute (start,end,path) = Route 
                                     (segmentId $ snd start)
                                     (segmentId $ snd end)
                                     (fmap segmentId path)
                                     (sum (fmap segmentLen path))
                                     [] -- TODO: allocation constraints

    routes :: [(SignalLocation, SignalLocation, [Segment])]
    routes = [(start,end,path) | start <- sigLocs
                               , (end,path) <- adjacentSignals [] (snd start) ]

    signalsByPrevSegment s = listToMaybe $ filter 
      (\x -> (segmentId (fst x)) == (segmentId s)) sigLocs

    adjacentSignals :: [Segment] -> Segment -> [(SignalLocation, [Segment])]
    adjacentSignals basePath sg = 
      case signalsByPrevSegment sg of
        Just goal -> [(goal,path)]
        Nothing -> concat [adjacentSignals path next
                          | next <- fmap (segmentById segments) (segmentNexts sg) ]
      where
        path = basePath ++ [sg]
    
segmentById :: [Segment] -> SegmentId -> Segment
segmentById segments id = fromMaybe (error $ "Segment not found " ++ (show id)) $
                            listToMaybe [ x | x <- segments, (segmentId x) == id]


