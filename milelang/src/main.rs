extern crate milelang;
use milelang::*;
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();
    let r = convert_railway(&buffer).unwrap();
    println!("Railway: {:?}",r);
    let railml = to_railml(r).unwrap();
    println!("railml: {}", railml);
}
