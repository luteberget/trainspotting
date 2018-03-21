extern crate smallvec;
extern crate ordered_float;

mod simulation;
mod observable;
mod dynamics;
mod infrastructure;
mod staticinfrastructure;

mod driver;
mod route;

fn main() {
    let x: Option<infrastructure::Infrastructure> = None;
    println!("infrastructure");
}
