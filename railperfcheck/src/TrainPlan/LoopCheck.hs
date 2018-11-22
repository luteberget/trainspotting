module TrainPlan.LoopCheck
 ( check ) where

import TrainPlan.SolverInput

import Data.Maybe
import Data.Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Tree as Tree
import Data.Set (Set)
import Control.Monad (join)

import Debug.Trace

check :: [RoutePart] -> [TrainName] -> RoutePlan -> Maybe (Int, TrainName, [RoutePartId])
check routes trains states = listToMaybe (catMaybes (fmap ch (zip [0..] (prod trains states))))
  where ch (n,(train,state)) = fmap (\res -> (n,train, res)) (findLoop routes train state)

prod :: [a] -> [b] -> [(a,b)]
prod x y = do
  x1 <- x
  y1 <- y
  return (x1,y1)


data GraphNode = GNSignal SignalRef | GNRoutePart RoutePartId
  deriving (Ord, Show, Eq)

findLoop :: [RoutePart] -> TrainName -> [(RoutePartId, Maybe TrainName)] -> Maybe [RoutePartId]
findLoop routes train state = fmap report loop
  where (graph,nodeFromVertex,vertexFromKey) = graphFromEdges (fmap (\(a,b) -> ((),a,Set.toList b)) (Map.toList graphMap))
        edges = join [ [ (GNSignal entry, GNRoutePart rid),
                         (GNRoutePart rid, GNSignal exit) ]
                     | (rid,Just t) <- state, t == train, 
                       let r = findRoute routes rid, 
                       isJust (routePartEntry r), isJust (routePartExit r),
                       let (Just entry) = routePartEntry r, 
                       let (Just exit) = routePartExit r ]
        graphMap = Map.fromListWith Set.union (fmap (\(a,b) -> (a, Set.singleton b)) edges)
        loop = listToMaybe (filter (\x -> length x > 1) (fmap Tree.flatten (scc graph)))
        report l = catMaybes $ fmap (\v -> (\(_,n,_) -> gnRoutePart n) (nodeFromVertex v)) l 

gnRoutePart :: GraphNode -> Maybe RoutePartId
gnRoutePart (GNRoutePart id) = Just id
gnRoutePart _ = Nothing

findRoute :: [RoutePart] -> RoutePartId -> RoutePart
findRoute routes id = head [ r | r <- routes, routePartName r == id ]
