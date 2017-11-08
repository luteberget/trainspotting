{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson

-- Infrastructure model
data Infrastructure = Infrastructure {
      tracks :: [Track],
      trackNodes :: [TrackNode],
      tracksideObjects :: [TracksideObject]
    } 
    deriving (Generic, Show)
instance ToJSON Infrastructure
instance FromJSON Infrastructure

data Track = Track {
    trackId :: String,
    length :: Float
}
    deriving (Generic, Show)
instance ToJSON Track
instance FromJSON Track

data TrackNode = TrackNode {
  nodesFrom :: [String],
  nodesTo :: [String]
}
    deriving (Generic, Show)
instance ToJSON TrackNode
instance FromJSON TrackNode

data TracksideObject = 
    Signal String Position 
  | Detector Position
    deriving (Generic, Show)
instance ToJSON TracksideObject
instance FromJSON TracksideObject

data Position = Position{
  posTrackRef :: String,
  posLength :: Float
}
    deriving (Generic, Show)
instance ToJSON Position
instance FromJSON Position

