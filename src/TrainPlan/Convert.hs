module TrainPlan.Convert (
  convert
  ) where

import qualified TrainPlan.Infrastructure as IS
import qualified TrainPlan.UsagePattern as UP
import qualified TrainPlan.Solver as S
import TrainPlan.Infrastructure (isBoundary, routePointRef, maybeRoutePointRef)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

convert :: IS.Infrastructure -> UP.UsagePattern -> ([S.Route], [S.Train])
convert is up = (reverse outRoutes, outTrains)
  where
    outRoutes = fmap convertRoute (zip [0..] (IS.routes is))
    outTrains = fmap convertTrain (zip [0..] (UP.movements up))

    convertTrain :: (Int, UP.MovementSpec) -> S.Train
    convertTrain (i,m) = S.Train i length visits
      where
        length = head [ UP.vehicleLength v | v <- UP.vehicles up
                                           , UP.vehicleName v == UP.vehicleRef m]
        visits = [enter] ++ internal ++ [exit]
        enterBoundary = head (fst (UP.enter m))
        exitBoundary = head (fst (UP.exit m))
        enter = head [ i | (i,r) <- zip [0..] (IS.routes is)
                         , maybeRoutePointRef (IS.entry r) == Just enterBoundary ]
        exit = head [ i | (i,r) <- zip [0..] (IS.routes is)
                        , maybeRoutePointRef (IS.exit r) == Just exitBoundary ]
        internal = [] -- TODO

    convertRoute :: (Int, IS.Route) -> S.Route
    convertRoute (i,r) = S.Route i entry exit (conflicts (i,r)) length
      where
        entry = (routePointConv i (IS.entry r))
        exit = (routePointConv i (IS.exit r))
        length = IS.length r

    routePointConv :: Int -> IS.RoutePoint -> Maybe String
    routePointConv i (IS.RoutePointBoundary b) = Nothing
    routePointConv i (IS.RoutePointSignal s) = Just s
    routePointConv i (IS.RoutePointTrackEnd) = Just ("r"++(show i)++"trackend")

    conflicts :: (Int, IS.Route) -> [Int]
    conflicts (self_id,r) = Set.toList (Set.delete self_id (Set.unions
                  [resourceToRouteMap Map.! res | res <- routeResources r]))

    resourceToRouteMap :: Map String (Set Int)
    resourceToRouteMap = Map.fromListWith Set.union 
                           [(res,Set.singleton id) | (id,r) <- zip [0..] (IS.routes is) 
                                                   , res <- routeResources r]

    -- tvds and switches
    -- not including overlap resources, because the route planner ignores waitable conflicts
    routeResources :: IS.Route -> [String]
    routeResources r = (IS.tvds r) ++ (fmap fst (IS.switchPos r)) 
                                  

