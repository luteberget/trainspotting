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
  , routePartConflicts :: [[(RoutePartId,Int)]] -- Set of alternative sets of conflicting routes
                                                -- these sets can be switched between.
  , routePartWaitConflict :: Maybe Int  -- A conflict set which cannot be directly used.
                                        -- (used to model overlap timeout)
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


