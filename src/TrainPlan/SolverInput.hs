module TrainPlan.SolverInput where

import SAT.Val

type SignalId = String 
type RouteId = Int 
type TrainId = Int 
 
 -- data RouteEntry = NoEntry | SourceNode | SignalEntry SignalId 
 -- data RouteExit  = NoExit  | SinkNode   | SignalExit  SignalId 
  
data Route 
  = Route 
  { routeId        :: RouteId 
  , routeEntry     :: Maybe SignalId 
  , routeExit      :: Maybe SignalId 
  , routeConflicts :: [RouteId] 
  , routeLength    :: Double
  } deriving (Eq, Ord, Show)
  
data Train
  = Train 
  { trainId     :: TrainId
  , trainLength :: Double
  , trainVisits :: [RouteId]
  } deriving (Eq, Ord, Show)

type OrderingConstraint = ((TrainId,RouteId),(TrainId,RouteId))
type Problem = ([Route],[Train],[Ordering])

type RoutePlan = [[(RouteId, Maybe TrainId)]]

