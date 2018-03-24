#![allow(redundant_field_names)]
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

pub fn evaluate_plan(staticinfrastructure: &input::staticinfrastructure::StaticInfrastructure,
                     routes: &HashMap<String,input::staticinfrastructure::Route>,
                     dispatch: &input::dispatch::Dispatch) -> output::history::History {


    let mut train_logs = Vec::new();
    let inf_log = Rc::new(RefCell::new(Vec::new()));
    let time_log = {
        let log = inf_log.clone();
        Box::new(move |t| log.borrow_mut().push(InfrastructureLogEvent::Wait(t)))
    };

    let mut scheduler = eventsim::Scheduler::new();
    let world_log = inf_log.clone();
    let world = railway::infrastructure::Infrastructure::new(
        &mut scheduler, staticinfrastructure, 
        Box::new(move |i| world_log.borrow_mut().push(i))); 
    let mut sim = eventsim::Simulation::new_with_scheduler(world, scheduler);
    sim.set_time_log(time_log);

    for action in &dispatch.actions {
        use input::dispatch::DispatchAction::*;
        match *action {
            Wait(t) => sim.advance_by(t),
            Route(ref route_name) => match routes.get(route_name) {
                Some(&input::staticinfrastructure::Route::TrainRoute(ref route)) => {
                    sim.start_process(Box::new(
                        railway::route::ActivateRoute::new(route.clone())));
                },
                _ => panic!("Unknown route \"{}\"", route_name),
            },
            Train(ref name, ref params, ref route_name) =>  {
                let (node_idx, auth_dist) = match routes.get(route_name) {
                    Some(&input::staticinfrastructure::Route::EntryRoute(ref r)) => {
                        (r.boundary, r.length)
                    },
                    _ => panic!("Unknoown route \"{}\"", route_name),
                };

                let train_log = Rc::new(RefCell::new(Vec::new()));
                train_logs.push((name.clone(), train_log.clone()));
                let logger = Box::new(move |i| {
                    println!(" --- {:?}", i);
                    train_log.borrow_mut().push(i);
                });
                let driver = Box::new(
                    railway::driver::Driver::new(&mut sim, node_idx, auth_dist, 
                          *params, logger));
                sim.start_process(driver);
            }
        }
    }

    sim.run();

    output::history::History {
        inf: inf_log.replace(Vec::new()),
        trains: train_logs.into_iter().map(|(n,v)| (n, v.replace(Vec::new()))).collect()
    }
}
