extern crate smallvec;
extern crate ordered_float;

mod simulation;
mod observable;
mod dgraph;
mod railway;
mod objects;

mod driver;

fn main() {
    let x : Option<railway::Railway> = None;
    println!("railway");
}
