use input::staticinfrastructure::SwitchPosition;
use railway::dynamics::{DriverAction, DistanceVelocity};

#[derive(Debug)]
pub struct History {
    pub inf: Vec<InfrastructureLogEvent>,
    pub trains: Vec<(String, Vec<TrainLogEvent>)>,
}

#[derive(Debug)]
pub enum InfrastructureLogEvent {
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

#[derive(Debug)]
pub enum TrainLogEvent {
    Wait(f64),
    Node(usize, Option<usize>),
    Sight(usize, bool),
    Move(f64, DriverAction, DistanceVelocity),
}
