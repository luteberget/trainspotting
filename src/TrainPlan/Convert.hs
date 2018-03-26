module TrainPlan.Convert (
  solverInput,
  dispatchPlan
  ) where

import TrainPlan.Routes
import TrainPlan.UsagePattern
import qualified TrainPlan.SolverInput as Solver

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (join)
import Data.List (find)
import Data.Maybe (catMaybes)

succPairs x = zip x (tail x)

routePointConv :: RoutePoint -> Maybe String
routePointConv (RoutePointBoundary b) = Nothing
routePointConv (RoutePointSignal s) = Just s

bdry :: Route -> Maybe String
bdry r = case routeEntry r of
  RoutePointBoundary x -> Just x
  _ -> case routeExit r of 
    RoutePointBoundary x -> Just x
    _ -> Nothing

data SplitRoute
 = SplitRoute
 { splitName :: (String, Int)
 , splitEntry :: Maybe SignalRef
 , splitExit :: Maybe SignalRef
 , splitLength :: Double
 , splitResources :: [String]
 , splitNodes :: [NodeRef]
 } deriving (Show, Eq, Ord)

splitRoutes :: Route -> [SplitRoute]
splitRoutes route =  [ SplitRoute (routeName route, i) entry exit l res nodes
                     | (i, (entry,exit), (Release l res)) <-
                            zip3 [0..] (succPairs signals) (routeReleases route)]
  where
    n = length (routeReleases route)
    signals = [routePointConv (routeEntry route)] ++ 
            (fmap (\j -> Just ((routeName route) ++ "__i" ++ (show j))) [1..(n-1)]) ++ 
            [routePointConv (routeExit route)]
    nodes = (routeContains (route)) ++ (catMaybes [bdry route])

resolveConflicts :: [SplitRoute] -> [Solver.RoutePart]
resolveConflicts rs = [ Solver.RoutePart name en ex (conflicts name res) nodes l
                      | (SplitRoute name en ex l res nodes) <- rs ]
  where
    conflicts :: (String,Int) -> [String] -> [(String,Int)]
    conflicts self res = Set.toList (Set.delete self 
                                    (Set.unions [resMap Map.! r | r <- res]))
    
    resMap :: Map String (Set (String,Int))
    resMap = Map.fromListWith Set.union 
               [(resource, Set.singleton name) 
               | (SplitRoute name _ _ _ resources _) <- rs
               , resource <- resources ]

convertRoutes :: [Route] -> ([Solver.RoutePart], Solver.ElementaryRoutes)
convertRoutes rs = (resolveConflicts (join routeParts), (fmap.fmap) splitName routeParts)
  where routeParts = fmap splitRoutes rs

convertTiming :: UsagePattern -> [Solver.TrainOrd]
convertTiming usage = [ (namedVisits Map.! a, namedVisits Map.! b)
                      | (TimingSpec a b _dt) <- timings usage]
  where 
    namedVisits :: Map String (Solver.TrainName, Solver.VisitId)
    namedVisits = Map.fromList (join (fmap trainNamedVisits (zip [0..] (movements usage))))

    trainNamedVisits :: (Int, MovementSpec) -> [(String, (Solver.TrainName, Solver.VisitId))]
    trainNamedVisits (i, movement) = catMaybes [ fmap (\vn -> (vn,("t" ++ (show i), j))) visitName
                                               | (j,(visitName, nodes, _wait)) <- zip [0..] (visits movement)]
    
-- TODO optionally given train names (for recognizing in the output)
convertTrain :: UsagePattern -> (Int,MovementSpec) -> (Solver.Train, [Solver.TrainOrd])
convertTrain usage (i,m) = (Solver.Train name length (fmap (\(_,nodes,_) -> nodes) (visits m)), ords)
  where
    name = "t" ++ (show i)
    length = head [ vehicleLength v | v <- vehicles usage
                                       , vehicleName v == vehicleRef m]
    ords = fmap (\(v1,v2) -> ((name,v1),(name,v2))) (succPairs (fmap fst (zip [0..] (visits m))))

solverInput :: [Route] -> UsagePattern -> ([Solver.RoutePart], Solver.ElementaryRoutes, [Solver.Train], [Solver.TrainOrd])
solverInput routes usage = (routeParts, elementaryRoutes, trains, ordConstraints)
   where
     (routeParts, elementaryRoutes) = convertRoutes routes
     trainConv = fmap (convertTrain usage) (zip [1..] (movements usage))
     (trains, trainOrd) = (fmap fst trainConv, fmap snd trainConv)
     ordConstraints = (join trainOrd) ++ (convertTiming usage)

dispatchPlan = undefined
