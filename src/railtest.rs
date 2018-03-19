extern crate smallvec;
extern crate ordered_float;

mod simulation;
mod observable;
mod dynamics;
mod railway;
mod driver;

mod route;

fn main() {
    let x: Option<railway::Railway> = None;
    println!("railway");
}
