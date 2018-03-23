extern crate smallvec;
extern crate ordered_float;
extern crate regex;
extern crate failure;
#[macro_use] extern crate failure_derive;


pub mod input;
pub mod output;
pub mod eventsim;
pub mod railway;

pub mod ffi;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use output::history::InfrastructureLogEvent;
pub struct Log(Rc<RefCell<Vec<InfrastructureLogEvent>>>);
impl eventsim::TimeLogger for Log {
    fn advance_time(&mut self, t: f64) { 
        self.0.borrow_mut().push(InfrastructureLogEvent::Wait(t)); 
    }
}

impl railway::infrastructure::Logger for Log {
    fn output(&mut self, msg :InfrastructureLogEvent) {
        self.0.borrow_mut().push(msg);
    }
}

pub fn evaluate_plan(staticinfrastructure: &input::staticinfrastructure::StaticInfrastructure,
                     routes: &HashMap<String,input::staticinfrastructure::Route>,
                     dispatch: &input::dispatch::Dispatch) -> output::history::History {

    let mut scheduler = eventsim::Scheduler::new();
    let world = railway::infrastructure::Infrastructure::new(&mut scheduler, staticinfrastructure); 

    let mut sim = eventsim::Simulation::new_with_scheduler(world, scheduler);

    let mut train_logs = Vec::new();
    let inf_log = Rc::new(RefCell::new(Vec::new()));
    sim.set_logger(Log(inf_log.clone()));

    for action in dispatch.actions.iter() {
        use input::dispatch::DispatchAction::*;
        match *action {
            Wait(t) => sim.advance_by(t),
            Route(ref route_name) => match routes.get(route_name) {
                Some(route) => {
                    sim.start_process(Box::new(
                            railway::route::ActivateRoute::new(route.clone(), inf_log.clone())));
                },
                None => panic!("Unknown route \"{}\"", route_name),
            },
            Train(ref name, ref params, (ref node, auth_dist)) =>  {
                let node_idx = staticinfrastructure.node_names[node];
                let log = Rc::new(RefCell::new(Vec::new()));
                train_logs.push((name.clone(), log.clone()));
                let driver = Box::new(
                    railway::driver::Driver::new(&mut sim, node_idx, auth_dist, 
                                                          params.clone(), log));
                sim.start_process(driver);
            }
        }
    }

    let log = sim.take_logger().unwrap();

    output::history::History {
        inf: log.0.replace(Vec::new()),
        trains: train_logs.into_iter().map(|(n,v)| (n, v.replace(Vec::new()))).collect()
    }
}
