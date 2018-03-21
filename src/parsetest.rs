extern crate smallvec;
extern crate ordered_float;

mod staticinfrastructure;
mod staticinfrastructure_parser;
use staticinfrastructure_parser::*;

use std::path::Path;
pub fn parse_file(f :&Path) -> Result<staticinfrastructure::StaticInfrastructure,String> {
  use std::fs::File;
  use std::io::prelude::*;
use std::io::BufReader;

  let mut file = File::open(f).map_err(|e| format!("Could not open file: {}",e.to_string()))?;
  let mut file = BufReader::new(&file);
  let mut contents = String::new();
  file.read_to_string(&mut contents).map_err(|e| format!("Invalid UTF8: {}", e.to_string()))?;

  let lex = lexer(&mut contents.chars()).map_err(|e| format!("{:?}",e))?;
  let mut i = 0;
  for x in lex.iter() {
      println!(" * {}: {:?}", i, x);
      i += 1;
  }
  let stmts = parse(&lex).map_err(|e| format!("{:?}",e))?;
  for x in stmts.iter() {
      println!(" * {}: {:?}", i, x);
      i += 1;
  }
  let model = model_from_ast(&stmts).map_err(|e| format!("{:?}", e))?;
  let mut i = 0;
  for x in model.nodes.iter() {
      println!(" *n {}: {:?}", i, x);
      i += 1;
  }
  let mut i = 0;
  for x in model.objects.iter() {
      println!(" *o {}: {:?}", i, x);
      i += 1;
  }
  Ok(model)
}

pub fn main() {
        use std::env;
            let args: Vec<String> = env::args().collect();

                let filename = &args[1];
                    //let filename = &args[2];
                    let x = parse_file(&Path::new(filename));
}

