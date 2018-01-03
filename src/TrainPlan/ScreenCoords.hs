{-# LANGUAGE DeriveGeneric #-}
module TrainPlan.ScreenCoords where

import GHC.Generics
import TrainPlan.Infrastructure

data ScreenCoords = ScreenCoords {
  loc :: Location,
  xy :: (Double, Double)
} deriving (Generic, Show)
