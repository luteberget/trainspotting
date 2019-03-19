use rolling::input::staticinfrastructure::{NodeId, RouteEntryExit};
use crate::movement::*;


// Plan interface

pub struct Config {
    n_before: usize,
    n_after: usize,
}

type RoutePartId = (usize,usize); // Route index and partial index into route
type OverlapId = usize;
type TrainId = usize;

pub struct RoutePart {
    name :RoutePartId,
    entry: RouteEntryExit,
    exit: RouteEntryExit,
    conflicts: Vec<Vec<(RoutePartId, usize)>>, // ??
    wait_conflict :Option<OverlapId>,
    contains_nodes :Vec<NodeId>,
    length: f32,
}

type ElementaryRoute = Vec<RoutePartId>;

pub struct Train {
    name :String,
    length: f32,
    visits: Vec<Vec<NodeId>>,
    vehicle: Vehicle,
}

pub struct TrainOrd {
    a :(TrainId, VisitId),
    b :(TrainId, VisitId),
}

pub struct Problem {
    routes: Vec<RoutePart>,
    elementary_routes: Vec<ElementaryRoute>,
    trains: Vec<Train>,
    train_ord: Vec<TrainOrd>,
}


// OUTPUT: route plan

pub type RoutePlan = Vec<Vec<(RoutePartId, Option<TrainId>)>>;

