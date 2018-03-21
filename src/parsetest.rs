extern crate smallvec;
extern crate ordered_float;

mod parser_utils;
mod staticinfrastructure;
mod staticinfrastructure_parser;
mod route_parser;

use std::path::Path;

pub fn main() {
    use std::env;
    let args: Vec<String> = env::args().collect();
    let inffile = &args[1];
    let routefile = &args[2];
    let (inf,objnames,nodenames) = staticinfrastructure_parser::parse_file(
        &Path::new(inffile)).unwrap();
    let routes = route_parser::parse_file(&Path::new(routefile), 
                                          &objnames, &nodenames).unwrap();

    println!("parse test ok ");
}
