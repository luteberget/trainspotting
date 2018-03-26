module TrainPlan.UsagePattern where
import TrainPlan.Routes

type VehicleRef = String
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
  visits :: [(Maybe String, [NodeRef], Maybe WaitTime)]
} deriving (Show)

data TimingSpec = TimingSpec {
  visitA :: EventRef,
  visitB :: EventRef,
  timingDiff :: Maybe Double
}
    deriving (Show)

data Vehicle = Vehicle {
  vehicleName :: String,
  vehicleLength :: Double,
  vehicleMaxAccel :: Double,
  vehicleMaxBrake :: Double,
  vehicleMaxVelocity :: Double
}
    deriving (Show, Ord, Eq)

