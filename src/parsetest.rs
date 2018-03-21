extern crate smallvec;
extern crate ordered_float;

mod parser_utils;
mod staticinfrastructure;
mod staticinfrastructure_parser;
use staticinfrastructure_parser::*;

use std::path::Path;

pub fn main() {
        use std::env;
            let args: Vec<String> = env::args().collect();

                let filename = &args[1];
                    //let filename = &args[2];
                    let x = parse_file(&Path::new(filename));
}

