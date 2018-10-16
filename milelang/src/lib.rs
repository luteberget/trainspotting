extern crate rolling;
extern crate railml2dgraph;
#[macro_use] extern crate lalrpop_util;
mod ast;
lalrpop_mod!(pub grammar);

use std::path::Path;
use rolling::input::staticinfrastructure::*;

pub fn convert_dgraph(s :&str) -> Result<StaticInfrastructure, String> {
    let milelang = grammar::MilelangParser::new().parse(s).map_err(|e| format!("{:?}", e))?;
    println!("Parsed {:?}", milelang);

    // Just load a single railml file for now
    for stmt in milelang {
        use ast::*;
        match stmt {
            Statement::RailmlFile(ref file) => {
                // Convert xml to dgraph string
                let (doc,ns) = railml2dgraph::get_xml(&Path::new(file), true)?;
                let model = railml2dgraph::convert(&doc, &ns)?;
                let s = railml2dgraph::output::dgraph_string(&model)?;

                // Parse string to dgraph infrastructure model
                let inf = rolling::get_infrastructure_string(&s).map_err(|e| format!("{:?}",e))?;
                return Ok(inf);
            },
        }
    }
    Err(format!("Unknown error parsing milelang input."))
}
