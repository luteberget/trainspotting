module TrainPlan.SolverInput where
import TrainPlan.UsagePattern

type NodeRef = String
type SignalRef = String 
type RoutePartId = (String,Int)
type TrainName = String
type VisitId = Int
 
 -- data RouteEntry = NoEntry | SourceNode | SignalEntry SignalRef 
 -- data RouteExit  = NoExit  | SinkNode   | SignalExit  SignalRef 
  
data RoutePart
  = RoutePart
  { routePartName      :: (String, Int)
  , routePartEntry     :: Maybe SignalRef 
  , routePartExit      :: Maybe SignalRef 
  , routePartConflicts :: [RoutePartId] 
  , routePartContains  :: [NodeRef]
  , routePartLength    :: Double
  } deriving (Eq, Ord, Show)

type ElementaryRoutes = [[RoutePartId]]
  
data Train
  = Train 
  { trainName   :: TrainName
  , trainLength :: Double
  , trainVisits :: [[NodeRef]]
  , trainVehicle :: Vehicle
  } deriving (Eq, Ord, Show)

type TrainOrd = ((TrainName,VisitId),(TrainName,VisitId))

type Problem = ([RoutePart],ElementaryRoutes,[Train],[TrainOrd])

type RoutePlan = [[(RoutePartId, Maybe TrainName)]]


