module TrainPlan.Routes where

type NodeRef = String
type SignalRef = String
type ResourceRef = String

data RoutePoint = RoutePointBoundary NodeRef
                | RoutePointSignal SignalRef
                | RoutePointTrackEnd
  deriving (Show, Eq, Ord)

maybeRoutePointRef :: RoutePoint -> Maybe String
maybeRoutePointRef (RoutePointBoundary s) = Just s
maybeRoutePointRef (RoutePointSignal s) = Just s
maybeRoutePointRef _ = Nothing

isBoundary :: RoutePoint -> Bool
isBoundary (RoutePointBoundary _) = True
isBoundary _ = False

routePointRef :: RoutePoint -> String
routePointRef (RoutePointBoundary s) = s
routePointRef (RoutePointSignal s) = s


data Route
 = Route
 { routeName :: String
 , entry :: RoutePoint
 , exit :: RoutePoint
 , length :: Double
 , releases :: [Release]
 } deriving (Show, Eq, Ord)

data Release
 = Release
 { releaseLength :: Double
 , resources :: [ResourceRef]
 } deriving (Show, Eq, Ord)

