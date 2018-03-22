extern crate smallvec;
extern crate ordered_float;
extern crate regex;

pub mod input;
pub mod output;
pub mod eventsim;
pub mod railway;

pub mod ffi;

pub fn evaluate_plan(staticinfrastructure: &input::staticinfrastructure::StaticInfrastructure,
                     routes: &Vec<input::staticinfrastructure::Route>,
                     dispatch: &input::dispatch::Dispatch) -> output::history::History {

    let history = output::history::History {
        inf: Vec::new(),
        trains: Vec::new(),
    };

    let mut scheduler = eventsim::Scheduler::new();
    let world = railway::infrastructure::Infrastructure::new(&mut scheduler, staticinfrastructure); 

    //let sim = eventsim::Simulation::railway(world, scheduler);


    unimplemented!()
}
