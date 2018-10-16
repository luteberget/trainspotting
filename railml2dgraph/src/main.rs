extern crate minidom;
extern crate clap;
extern crate railml2dgraph;

use std::fs::File;
use clap::{Arg, App};
use std::path;

use railml2dgraph::*;

struct Opts<'a> {
    input_fn: &'a path::Path,
    infrastructure_fn: Option<&'a path::Path>,
    routes_fn: Option<&'a path::Path>,
    verbose: bool,
}

fn main() {
    let opts = App::new("railML 2.x to Rolling converter")
        .about("Convert railML 2.x files to rolling D-graph format")
        .arg(Arg::with_name("INPUT")
            .help("railML file")
            .required(true)
            .index(1))
        .arg(Arg::with_name("v")
            .short("v")
            .help("Level of verbosity"))
        .arg(Arg::with_name("infrastructure")
            .long("infrastructure")
            .short("o")
            .value_name("FILE")
            .help("Output rolling d-graph format infrastructure to file"))
        .arg(Arg::with_name("routes")
            .short("r")
            .long("routes")
            .value_name("FILE")
            .help("Output rolling routes to file"))
        .get_matches();

    let opts = Opts {
        input_fn: opts.value_of("INPUT").map(|x| path::Path::new(x)).unwrap(),
        infrastructure_fn: opts.value_of("infrastructure").map(|x| path::Path::new(x)),
        routes_fn: opts.value_of("routes").map(|x| path::Path::new(x)),
        verbose: opts.occurrences_of("v") > 0,
    };

    match run(&opts) {
        Ok(()) => {}
        Err(e) => {
            println!("Failed: {}", e);
            std::process::exit(1);
        }
    }
}

fn run(opts :&Opts) -> Result<(), String> {
    let (doc, ns) = get_xml(opts.input_fn, opts.verbose)?;
    let dgraph = convert(&doc,&ns)?;


    //println!("DGRAPH {:?}", dgraph);
    println!("DGRAPH nodes");
    for (i,n) in dgraph.nodes.iter().enumerate(){
        println!("n{}: {:?}", i, n);
    }
    println!("DGRAPH edges");
    for (i,n) in dgraph.edges.iter().enumerate(){
        println!("e{}: {:?}", i, n);
    }



    // 9. output infrastructure rolling dgraph format
    if let Some(f) = opts.infrastructure_fn {
        let mut buffer = File::create(f).map_err(|e| e.to_string())?;
        output::print_rolling(&mut buffer, &dgraph).map_err(|e| e.to_string())?;
    }

    // 10. output routes
    if let Some(f) = opts.routes_fn {
        let routes = routes::find_routes(&dgraph);

        let mut buffer = File::create(f).map_err(|e| e.to_string())?;
        output::print_routes(&mut buffer, &dgraph, &routes).map_err(|e| e.to_string())?;
    }

    Ok(())
}

