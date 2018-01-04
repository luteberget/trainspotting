module TrainPlan.Infrastructure where

type DirectionalLocation = (Location, Direction)
type ConstVelocity = Double

type TrackRef = String
data Direction = Up | Down
  deriving (Show)

-- Infrastructure model
data Infrastructure = Infrastructure {
      tracks :: [Track],
      nodes :: [Node],
      components :: [Component]
    } 
    deriving (Show)

data Track = Track {
    trackId :: String,
    length :: Double 
}
    deriving (Show)

data Node = Node {
  nodesFrom :: [TrackRef],
  nodesTo :: [TrackRef]
}
    deriving (Show)

data Component = 
    Signal String DirectionalLocation
  | Detector Location
    deriving (Show)

data Location = Location {
  posTrackRef :: TrackRef,
  posLength :: Double 
}
    deriving (Show)

