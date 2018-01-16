module TrainPlan.Infrastructure where

type DirectionalLocation = (Location, Direction)
type ConstVelocity = Double

type TrackRef = String
type NodeRef = String
type RouteEnd = String -- Boundary node name or signal name
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
  entry :: Maybe RouteEnd,
  exit :: Maybe RouteEnd,
  tvds :: [TVDRef],
  switchPos :: [(NodeRef, SwitchPosition)],
  length :: Double,
  releases :: [ReleaseSpec]
} deriving (Show)

data ReleaseSpec
  = ReleaseSpec
  { trigger :: TVDRef
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
    Signal String DirectionalLocation
  | Detector Location (Maybe TVDRef) (Maybe TVDRef)
  | TVD String
    deriving (Show)

data Location = Location {
  posTrackRef :: TrackRef,
  posLength :: Double 
}
    deriving (Show, Ord, Eq)

