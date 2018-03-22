extern crate smallvec;
extern crate ordered_float;
extern crate regex;

pub mod input;
pub mod output;
pub mod eventsim;
pub mod railway;

pub mod ffi;

pub fn evaluate_plan(infrastructure: &input::staticinfrastructure::StaticInfrastructure,
                     routes: &Vec<input::staticinfrastructure::Route>,
                     dispatch: &input::dispatch::Dispatch) -> output::history::History {
    unimplemented!()

}
