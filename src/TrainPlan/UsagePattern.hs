module TrainPlan.UsagePattern where

type VehicleRef = String
type SignalRef = String
type EventRef = String

-- Usage pattern for railway lines
data UsagePattern = UsagePattern {
  vehicles :: [Vehicle],
  movements :: [MovementSpec],
  timings :: [TimingSpec]
}
    deriving (Show)

data MovementSpec = MovementSpec {
  vehicleRef :: VehicleRef,
  visits :: [(Maybe String, [SignalRef])] -- Signal references
}
    deriving (Show)

data TimingSpec = TimingSpec {
  visitA :: EventRef,
  visitB :: EventRef,
  timingDiff :: Double
}
    deriving (Show)

data Vehicle = Vehicle {
  vehicleName :: String,
  vehicleLength :: Double,
  vehicleMaxAccel :: Double,
  vehicleMaxBrake :: Double,
  vehicleMaxVelocity :: Double
}
    deriving (Show)

