module TrainPlan.UsagePattern where
import TrainPlan.Infrastructure

type VehicleRef = String
type SignalRef = String
type EventRef = String

type WaitTime = Double

-- Usage pattern for railway lines
data UsagePattern = UsagePattern {
  vehicles :: [Vehicle],
  movements :: [MovementSpec],
  timings :: [TimingSpec]
}
    deriving (Show)

data MovementSpec = MovementSpec {
  vehicleRef :: VehicleRef,
  enter :: ([NodeRef], Maybe ConstVelocity),
  visits :: [(Maybe String, [DirectionalLocation], Maybe WaitTime)],
  exit :: ([NodeRef], Maybe ConstVelocity)
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

