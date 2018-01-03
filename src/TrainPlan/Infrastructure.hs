{-# LANGUAGE DeriveGeneric #-}
module TrainPlan.Infrastructure where

import GHC.Generics
import Data.Aeson

type TrackRef = String

-- Infrastructure model
data Infrastructure = Infrastructure {
      tracks :: [Track],
      nodes :: [Node],
      components :: [Component]
    } 
    deriving (Generic, Show)
instance ToJSON Infrastructure
instance FromJSON Infrastructure

data Track = Track {
    trackId :: String,
    length :: Double 
}
    deriving (Generic, Show)
instance ToJSON Track
instance FromJSON Track

data Node = Node {
  nodesFrom :: [TrackRef],
  nodesTo :: [TrackRef]
}
    deriving (Generic, Show)
instance ToJSON Node
instance FromJSON Node

data Component = 
    Signal String Location 
  | Detector Location
    deriving (Generic, Show)
instance ToJSON Component 
instance FromJSON Component

data Location = Location {
  posTrackRef :: TrackRef,
  posLength :: Double 
}
    deriving (Generic, Show)
instance ToJSON Location
instance FromJSON Location

