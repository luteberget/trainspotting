module TrainPlan.Timing 
 ( evaluate ) where

import TrainPlan.Simulator
import TrainPlan.UsagePattern

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromJust, isJust)
import Data.List (sortOn)
import Control.Monad (forM, forM_)
import Control.Monad.State
import Debug.Trace


-- data Event
--   = Event
--   { train :: String
--   , time :: Double
--   , node :: String }
--   deriving (Show, Eq, Ord)

-- data UsagePattern = UsagePattern {
--   vehicles :: [Vehicle],
--   movements :: [MovementSpec],
--   timings :: [TimingSpec]
-- }
--     deriving (Show)
-- 
-- data MovementSpec = MovementSpec {
--   vehicleRef :: VehicleRef,
--   visits :: [(Maybe String, [NodeRef], Maybe WaitTime)]
-- } deriving (Show)
-- 
-- data TimingSpec = TimingSpec {
--   visitA :: EventRef,
--   visitB :: EventRef,
--   timingDiff :: Double
-- }
--     deriving (Show)
-- 
-- data Vehicle = Vehicle {
--   vehicleName :: String,
--   vehicleLength :: Double,
--   vehicleMaxAccel :: Double,
--   vehicleMaxBrake :: Double,
--   vehicleMaxVelocity :: Double
-- }
--     deriving (Show, Ord, Eq)


type TrainName = String
type NodeRef = String
-- type Triggers  = Map (TrainName, NodeRef) (Set Int)
-- type Constraints = Map Int Status
data Status = Untriggered | Triggered Double | Resolved deriving (Show, Ord, Eq)

-- pass through a list of constraints in chronological order
-- create requirements which are checked off by events
evaluate :: UsagePattern -> [Event] -> Bool
evaluate usage history = all (\x -> x == Resolved) (traceShowId final)
      
  where
    final = execState go (Map.fromList [(i,Untriggered) 
                           | i <- [0..((length (timings usage))-1)] ])
    go = do
      forM (sortOn (\(Event _ t _) -> t) (traceShowId history)) $ \(Event trainName time node) -> do
        case Map.lookup (trainName, node) (traceShowId startTriggers) of
          Just ids -> forM_ ids $ \id -> do
                        status <- gets (Map.! id)
                        case status of
                          Untriggered -> do
                                         modify (Map.insert id (Triggered time))
                                         return ()
                          _ -> return ()
                        return ()
          Nothing -> return ()

        case Map.lookup (trainName, node) (traceShowId endTriggers) of
          Just ids -> forM_ ids $ \id -> do
                        let (TimingSpec visitA visitB dt) = timingFromId Map.! id
                        status <- gets (Map.! id)
                        case status of
                          Triggered prev_t -> do
                            case dt of
                              Just lim -> if time - prev_t < lim then do
                                              modify (Map.insert id (Resolved))
                                          else return ()
                              Nothing -> do
                                         modify (Map.insert id (Resolved))
                                         return ()
                          _ -> return ()
          Nothing -> return ()
        return ()

    startTriggers :: Map (TrainName, NodeRef) (Set Int)
    startTriggers = Map.fromListWith Set.union [ ((tName,nName),Set.singleton i)
                                              | (i,(TimingSpec visitA visitB dt)) <- zip [0..] (timings usage)
                                              , let (tName,nodes,dt) = visitMap Map.! visitA
                                              , nName <- nodes ]

    endTriggers :: Map (TrainName, NodeRef) (Set Int)
    endTriggers = Map.fromListWith Set.union [ ((tName,nName),Set.singleton i)
                                              | (i,(TimingSpec visitA visitB dt)) <- zip [0..] (timings usage)
                                              , let (tName,nodes,dt) = visitMap Map.! visitB
                                              , nName <- nodes ]
                                              
    visitMap :: Map String (String, [NodeRef], Maybe Double)
    visitMap = Map.fromList [ (fromJust visitName, (trainName, nodes, dt))
                            | (trainName,movement) <- zip (fmap (\x -> "t" ++ (show x)) [1..]) (movements usage)
                            , (visitName, nodes, dt) <- visits movement, isJust visitName ]

    timingFromId :: Map Int TimingSpec
    timingFromId = Map.fromList (zip [0..] (timings usage))
 
