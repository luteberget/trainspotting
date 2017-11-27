{-# LANGUAGE DeriveGeneric #-}
module ScreenCoords where

import GHC.Generics
import Infrastructure

data ScreenCoords = ScreenCoords {
  loc :: Location,
  xy :: (Double, Double)
} deriving (Generic, Show)
