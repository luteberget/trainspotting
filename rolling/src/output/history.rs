use input::staticinfrastructure::{StaticInfrastructure, SwitchPosition};
use railway::dynamics::{DriverAction, DistanceVelocity, TrainParams};
use failure;

#[derive(Debug)]
pub struct History {
    pub inf: Vec<InfrastructureLogEvent>,
    pub trains: Vec<(String, TrainParams, Vec<TrainLogEvent>)>,
}

#[derive(Debug, Copy, Clone)]
pub enum RouteStatus {
    Pending, Active, Released,
}

#[derive(Debug)]
pub enum InfrastructureLogEvent {
    Wait(f64),
    Route(usize,RouteStatus),
    Authority(usize, Option<f64>),
    Reserved(usize, bool),
    Occupied(usize, bool),
    Position(usize, SwitchPosition),
}

#[derive(Debug)]
pub enum TrainLogEvent {
    Wait(f64),
    Node(usize),
    Edge(usize, Option<usize>),
    Sight(usize, bool),
    Move(f64, DriverAction, DistanceVelocity),
}

/// Print one train node visits per line on the following format:
/// `trainname time nodename`.
pub fn visits(inf :&StaticInfrastructure, h: &History) -> Result<String,failure::Error> {
    use std::fmt::Write;
    let mut s = String::new();
    for &(ref train_name, ref _params, ref events) in &h.trains {
        let mut t = 0.0;
        for ev in events {
            use self::TrainLogEvent::*;
            match *ev {
                Wait(dt) => t += dt,
                Move(dt,_,_) => t += dt,

                Node(x) => {
                    let node_name = inf.node_names.
                        iter().find(|&(_k,v)| v == &x).expect("unknown node").0;
                    write!(s, "{} {} {}\n", train_name, t, node_name)?;
                },

                _ => {},
            }
        }
    }
    Ok(s)
}
