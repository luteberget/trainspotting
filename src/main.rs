extern crate rolling;
extern crate failure;

#[macro_use]
extern crate structopt;

use std::path::{PathBuf, Path};
use structopt::StructOpt;

/// Rolling -- simple railway simulation
#[derive(StructOpt, Debug)]
#[structopt(name="rolling")]
struct Opt {
    // A flag, true if used in the command line. Note doc comment will
    // be used for the help message of the flag.
    // /// Activate debug mode
    // #[structopt(short = "d", long = "debug")]
    // debug: bool,

    // // The number of occurences of the `v/verbose` flag
    /// Verbose mode (-v, -vv)
    #[structopt(short = "v", long = "verbose", parse(from_occurrences))]
    verbose: u8,

    /// Static infrastructure file in the Rolling D-Graph format
    #[structopt(short = "i", long = "infrastructure", parse(from_os_str))]
    infrastructure: PathBuf,

    /// Route file in the Rolling Route format
    #[structopt(short = "r", long = "routes", parse(from_os_str))]
    routes: PathBuf,
    
    /// Dispatch file in the Rolling Dispatch format
    #[structopt(short = "d", long = "dispatch", parse(from_os_str))]
    dispatch: PathBuf,

    /// Output format profile: full | timing
    #[structopt(short = "f", long = "format")]
    format: String,
}

pub type AppResult<T> = Result<T, failure::Error>;

pub fn read_file(f :&Path) -> AppResult<String> {
  use std::fs::File;
  use std::io::prelude::*;
  use std::io::BufReader;

  let file = File::open(f)?;
  let mut file = BufReader::new(&file);
  let mut contents = String::new();
  file.read_to_string(&mut contents)?;
  Ok(contents)
}

use rolling::input::staticinfrastructure;
use rolling::input::dispatch;
pub fn get_infrastructure(s :&Path) -> AppResult<staticinfrastructure::StaticInfrastructure> {
    use rolling::input::staticinfrastructure_parser::{lexer, parse, model_from_ast};
    let contents = read_file(s)?;
  let lex = lexer(&mut contents.chars())?;
  let stmts = parse(&lex)?;
  let model = model_from_ast(&stmts)?;
  Ok(model)
}

pub fn get_routes(s :&Path, inf :&staticinfrastructure::StaticInfrastructure) 
    -> AppResult<staticinfrastructure::Routes> {
    use rolling::input::route_parser::{parse, lexer};
    let contents = read_file(s)?;
    let lex = lexer(&mut contents.chars())?;
    let rs = parse(&lex, inf)?;
    Ok(rs)
}

fn get_dispatch(s :&Path) -> AppResult<dispatch::Dispatch> {
    let contents = read_file(s)?;
    let d = dispatch::parse_dispatch(&contents)?;
    Ok(d)
}

fn run(opt :&Opt) -> AppResult<()> {
    // 
    // Infrastructure
    let infrastructure = get_infrastructure(&opt.infrastructure)?;
    if opt.verbose >= 2 {
        println!("Infrastructure:");
        println!("  Nodes:");
        for x in &infrastructure.nodes {
            println!("    * {:?}", x);
        }
        println!("  Objects:");
        for x in &infrastructure.objects {
            println!("    * {:?}", x);
        }
    }

    // Routes
    let routes = get_routes(&opt.routes, &infrastructure)?;
    if opt.verbose >= 2 {
        println!("Routes:");
        for x in &routes { println!("  - {:?}", x); }
    }

    // Dispatch
    let dispatch = get_dispatch(&opt.dispatch)?;
    if opt.verbose >= 1 {
        println!("Dispatch:");
        for x in &dispatch.actions { println!("  - {:?}", x); }
        println!("");
    }

    // Eval -> history
    let history = rolling::evaluate_plan(&infrastructure, &routes, &dispatch);

    // Print
    println!("# Infrastructure history:");
    for x in &history.inf {
        println!("> {:?}", x);
    }
    for &(ref name,ref x) in &history.trains {
        println!("## Train \"{}\":", name);
        for x in x {
            println!("> {:?}", x);
        }
    }

    Ok(())
}

pub fn main() {
    let opt = Opt::from_args();
    println!("{:?}", opt);
    match run(&opt) {
        Ok(()) => {},
        Err(e) => println!("Error:\n{}", e.cause()),
    }
}
