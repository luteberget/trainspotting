use input::staticinfrastructure::{SwitchPosition};
use railway::dynamics::{DriverAction, DistanceVelocity};

pub struct History {
    pub inf: Vec<InfrastructureLogEvent>,
    pub trains: Vec<(String, Vec<TrainLogEvent>)>,
}

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

pub enum TrainLogEvent {
    Wait(f64),
    Node(usize),
    Sight(usize, bool),
    Move(DriverAction, DistanceVelocity),
}
