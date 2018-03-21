extern crate smallvec;
extern crate ordered_float;

mod simulation;
mod observable;
mod dynamics;
mod railway;
mod staticinfrastructure;

mod driver;
//mod route;

fn main() {
    let x: Option<railway::Infrastructure> = None;
    println!("railway");
}
