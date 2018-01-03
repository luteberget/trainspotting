{-# LANGUAGE DeriveGeneric #-}

module TrainPlan.UsagePattern where

import GHC.Generics
import Data.Aeson

type VehicleRef = String
type SignalRef = String
type EventRef = String

-- Usage pattern for railway lines
data UsagePattern = UsagePattern {
  vehicles :: [Vehicle],
  movements :: [MovementSpec],
  timings :: [TimingSpec]
}
    deriving (Generic, Show)
instance ToJSON UsagePattern
instance FromJSON UsagePattern

data MovementSpec = MovementSpec {
  vehicleRef :: VehicleRef,
  visits :: [(Maybe String, [SignalRef])] -- Signal references
}
    deriving (Generic, Show)
instance ToJSON MovementSpec
instance FromJSON MovementSpec

data TimingSpec = TimingSpec {
  visitA :: EventRef,
  visitB :: EventRef,
  timingDiff :: Double
}
    deriving (Generic, Show)
instance ToJSON TimingSpec
instance FromJSON TimingSpec

data Vehicle = Vehicle {
  vehicleName :: String,
  vehicleLength :: Double,
  vehicleMaxAccel :: Double,
  vehicleMaxBrake :: Double,
  vehicleMaxVelocity :: Double
}
    deriving (Generic, Show)
instance ToJSON Vehicle
instance FromJSON Vehicle

