module TrainPlan.ScreenCoords where
import TrainPlan.Infrastructure

data ScreenCoords = ScreenCoords {
  loc :: Location,
  xy :: (Double, Double)
} deriving (Show)
