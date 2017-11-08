{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson

-- Usage pattern for railway lines
data UsagePattern = UsagePattern {
  movements :: [MovementSpec],
  timings :: [TimingSpec]
}
    deriving (Generic, Show)
instance ToJSON UsagePattern
instance FromJSON UsagePattern

data MovementSpec = MovementSpec {
  trainTypeRef :: String,
  visits :: [[String]] -- Signal references
}
    deriving (Generic, Show)
instance ToJSON MovementSpec
instance FromJSON MovementSpec

data TimingSpec = TimingSpec {
  visitA :: (Int, Int),
  visitB :: (Int, Int),
  timingDiff :: Float
}
    deriving (Generic, Show)
instance ToJSON TimingSpec
instance FromJSON TimingSpec

data TrainType = TrainType {
  name :: String,
  length :: Float,
  maxAccel :: Float,
  maxBrake :: Float,
  maxVelocity :: Float
}
    deriving (Generic, Show)
instance ToJSON TrainType
instance FromJSON TrainType

