extern crate smallvec;
extern crate ordered_float;

mod simulation;
mod observable;
mod dynamics;
mod infrastructure;

mod staticinfrastructure;
mod staticinfrastructure_parser;

mod driver;
mod route;


// INFRASTRUCTURE
// DISPATCH
// -> HISTORY

// fn simulate(inf :StaticInfrastructure, dis: Dispatch) -> History { ... }

fn main() {
    let x: Option<infrastructure::Infrastructure> = None;
    println!("infrastructure");
}
