use rolling::input::staticinfrastructure::{NodeId, RouteEntryExit};
use std::collections::{HashMap, HashSet};


// Plan interface

pub struct Config {
    pub n_before: usize,
    pub n_after: usize,
}

pub type PartialRouteId = (usize,usize); // Index into Problem.partial_routes
pub type OverlapId = usize; // index into PartialRoute.conflicts
pub type TrainId = usize; // Index into Problem.trains
pub type SignalId = usize; // Arbitrary identification for signals
pub type VisitId = usize; // Index into Train.visits

pub struct PartialRoute {
    pub entry: Option<SignalId>,
    pub exit: Option<SignalId>,
    pub conflicts: Vec<HashSet<(PartialRouteId, usize)>>, // ??
    pub wait_conflict :Option<OverlapId>,
    pub contains_nodes :HashSet<NodeId>,
    pub length: f32,
}

type ElementaryRoute = HashSet<PartialRouteId>;

pub struct Train {
    pub length: f32,
    pub visits: Vec<HashSet<NodeId>>,
    pub vehicle: Vehicle, // TODO only length/reference is needed?
                          // Could instead parameterize VehicleId for library consumers
                          //  (same with SignalId).
}

pub struct Vehicle {
    pub name :String,
    pub length :f32,
    pub max_accel :f32,
    pub max_brake :f32,
    pub max_velocity :f32,
}

pub struct TrainOrd {
    pub a :(TrainId, VisitId),
    pub b :(TrainId, VisitId),
}

pub struct Problem {
    pub partial_routes: HashMap<PartialRouteId, PartialRoute>,
    pub elementary_routes: Vec<ElementaryRoute>,
    pub trains: HashMap<TrainId,Train>,
    pub train_ord: Vec<TrainOrd>,
}


// OUTPUT: route plan

pub type RoutePlan = Vec<Vec<(PartialRouteId, Option<TrainId>)>>;

