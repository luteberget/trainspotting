module TrainPlan.Infrastructure where

type DirectionalLocation = (Location, Direction)
type ConstVelocity = Double

type TrackRef = String
type NodeRef = String
type SignalRef = String
type DetectorRef = String

data RoutePoint = RoutePointBoundary NodeRef
                | RoutePointSignal SignalRef
                | RoutePointTrackEnd 
  deriving (Show, Eq, Ord)

isBoundary :: RoutePoint -> Bool
isBoundary (RoutePointBoundary _) = True
isBoundary _ = False

routePointRef :: RoutePoint -> String
routePointRef (RoutePointBoundary s) = s
routePointRef (RoutePointSignal s) = s

maybeRoutePointRef :: RoutePoint -> Maybe String
maybeRoutePointRef (RoutePointBoundary s) = Just s
maybeRoutePointRef (RoutePointSignal s) = Just s
maybeRoutePointRef _ = Nothing

type ResourceRef = String -- TVD or switch reference
type TVDRef = String

data Direction = Up | Down
  deriving (Ord, Eq, Show)
data SwitchPosition = SwLeft | SwRight | SwUnknown
  deriving (Ord, Eq, Show)

-- Infrastructure model
data Infrastructure = Infrastructure {
      tracks :: [Track],
      nodes :: [Node],
      components :: [Component],
      routes :: [Route]
    } 
    deriving (Show)

data Route = Route {
  routeName :: String,
  entry :: RoutePoint,
  exit :: RoutePoint,
  tvds :: [TVDRef],
  switchPos :: [(NodeRef, SwitchPosition)],
  -- TODO add overlap to grammar
  -- overlap :: Maybe ([TVDRef], Double), -- Resource and timeout 
  length :: Double,
  releases :: [ReleaseSpec],
  routeDir :: Direction
} deriving (Show)

data ReleaseSpec
  = ReleaseSpec
  { releaseLength :: Double
  , trigger :: TVDRef
  , resources :: [ResourceRef]
  } deriving (Show)

data Track = Track {
    trackId :: String,
    trackLength :: Double 
}
    deriving (Show)

data NodeData = BoundaryNode | ConnectionNode [TrackRef] deriving (Show)
data Node = Node {
  nodeId :: NodeRef,
  nodesFrom :: NodeData,
  nodesTo :: NodeData
}
    deriving (Show)

data Component = 
    Signal String DirectionalLocation DetectorRef
  | Detector String Location (Maybe TVDRef) (Maybe TVDRef)
  | Sight Location SignalRef Double
  | TVD String
    deriving (Show)

data Location = Location {
  posTrackRef :: TrackRef,
  posLength :: Double 
}
    deriving (Show, Ord, Eq)

