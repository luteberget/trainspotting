use input::staticinfrastructure::{SwitchPosition};
use railway::dynamics::{DriverAction, DistanceVelocity};

pub struct History {
    inf: Vec<InfrastructureEvent>,
    trains: Vec<(String, Vec<TrainEvent>)>,
}

pub enum InfrastructureEvent {
    Wait(f64),
    
    // Routes
    RoutePending(usize),
    RouteActive(usize),
    RouteReleased(usize),

    // Objects
    Authority(usize, Option<f64>),
    Reserved(usize, bool),
    Occupied(usize, bool),
    Position(usize, SwitchPosition),
}

pub enum TrainEvent {
    Wait(f64),
    Node(usize),
    Sight(usize, bool),
    Move(DriverAction, DistanceVelocity),
}
