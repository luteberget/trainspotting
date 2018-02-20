module TrainPlan.Convert (
  convert
  ) where

import qualified TrainPlan.Infrastructure as IS
import qualified TrainPlan.UsagePattern as UP
import qualified TrainPlan.SolverInput as S
import TrainPlan.Infrastructure (RoutePoint(..), isBoundary, routePointRef, maybeRoutePointRef)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (join)
import Data.List (find)
import Data.Maybe (catMaybes)

succPairs x = zip x (tail x)

data RouteResources 
  = RouteResources
  { rrId :: Int
  , rrEntry :: Maybe String
  , rrExit :: Maybe String
  , rrResources :: [String]
  , rrLength :: Double
  , rrBoundary :: Maybe String
  } deriving (Ord, Eq, Show)

convert :: IS.Infrastructure -> UP.UsagePattern -> ([S.Route], S.PartialRoutes, [S.Train], [S.TrainOrd])
convert is up = (reverse outRoutes, partialRoutes, outTrains, outOrd)
  where
    routesByResource = fmap convertRoute (zip [0..] (IS.routes is))
    convertedTrains = fmap convertTrain (zip [0..] (UP.movements up))
    partialRoutes = (fmap.fmap) rrId routesByResource
    outTrains = fmap fst convertedTrains
    outRoutes = resolveConflicts (join routesByResource)
    outOrd = trainInternalOrd ++ timingOrd
    timingOrd = fmap convertTiming (UP.timings up)
    trainInternalOrd = join (fmap snd convertedTrains)

    namedVisits :: Map String (S.TrainId, S.VisitId)
    namedVisits = Map.fromList (join (fmap trainNamedVisits (zip [0..] (UP.movements up))))

    trainNamedVisits :: (Int, UP.MovementSpec) -> [(String,(S.TrainId,S.VisitId))]
    trainNamedVisits (i,movement) = catMaybes maybeNamedVisits
      where
        maybeNamedVisits = fmap (\(j,s) -> fmap (\s -> (s,(i,j))) s) numberedVisits
        numberedVisits = zip [0..] names
        names = [fst3 (UP.enter movement)] ++
                [] ++ -- (fmap fst3 (UP.visits movement)) ++ -- TODO
                [(fst3 (UP.exit movement))]
        fst3 (x,_,_) = x

    convertTiming :: UP.TimingSpec -> S.TrainOrd
    convertTiming (UP.TimingSpec a b _dt) = (namedVisits Map.! a, namedVisits Map.! b)

    convertTrain :: (Int, UP.MovementSpec) -> (S.Train, [S.TrainOrd])
    convertTrain (i,m) = (S.Train i length visits, ords)
      where
        ords = fmap (\(v1,v2) -> ((i,v1),(i,v2))) (succPairs (fmap fst (zip [0..] visits)))
        length = head [ UP.vehicleLength v | v <- UP.vehicles up
                                           , UP.vehicleName v == UP.vehicleRef m]
        visits = [enter] ++ internal ++ [exit]
        enterBoundary = head ((\(_,nodes,_) -> nodes) (UP.enter m))
        exitBoundary  = head ((\(_,nodes,_) -> nodes) (UP.exit m))
        enter = head [ i | (RouteResources i _ _ _ _ b) <- join routesByResource
                         , b == Just enterBoundary]
        exit = head [ i | (RouteResources i _ _ _ _ b) <- join routesByResource
                         , b == Just exitBoundary]
        internal = [] -- TODO

    convertRoute :: (Int, IS.Route) -> [RouteResources]
    convertRoute (i,r) = [ RouteResources (100*i+j) na nb res l bdry
                         | (j, (l,res),(na,nb)) <- zip3 [1..] pts (succPairs names) ]
      where
        pts = if null (IS.releases r) then [(IS.length r, routeResources r)]
              else fmap (\(IS.ReleaseSpec l _ res) -> (l,res)) (IS.releases r)
        names = [entry] ++ 
                fmap (\j -> Just ("r_" ++ (show i) ++ "_" ++ (show j))) 
                  (take (length (IS.releases r) - 1) [(100*i+1)..]) ++ 
                [exit]
        entry = (routePointConv i (IS.entry r))
        exit = (routePointConv i (IS.exit r))

        bdry :: Maybe String
        bdry = case IS.entry r of
          RoutePointBoundary x -> Just x
          _ -> case IS.exit r of 
            RoutePointBoundary x -> Just x
            _ -> Nothing

    resolveConflicts :: [RouteResources] -> [S.Route]
    resolveConflicts rs = [ S.Route i na nb (conflicts i res) l
                          | (RouteResources i na nb res l _) <- rs ]
      where
        conflicts :: Int -> [String] -> [Int]
        conflicts self res = Set.toList (Set.delete self 
                                 (Set.unions [resMap Map.! r | r <- res]))
        
        resMap :: Map String (Set Int)
        resMap = Map.fromListWith Set.union 
                   [(resource, Set.singleton route) 
                   | (RouteResources route _ _ resources _ _) <- rs
                   , resource <- resources ]


    routePointConv :: Int -> IS.RoutePoint -> Maybe String
    routePointConv i (IS.RoutePointBoundary b) = Nothing
    routePointConv i (IS.RoutePointSignal s) = Just s
    routePointConv i (IS.RoutePointTrackEnd) = Just ("r"++(show i)++"trackend")

    -- tvds and switches
    -- not including overlap resources, because the route planner ignores waitable conflicts
    routeResources :: IS.Route -> [String]
    routeResources r = (IS.tvds r) ++ (fmap fst (IS.switchPos r)) 


