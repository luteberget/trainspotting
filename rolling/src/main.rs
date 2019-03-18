extern crate rolling;
extern crate failure;
extern crate structopt;

use rolling::*;
use std::path::PathBuf;
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
    #[structopt(parse(from_os_str))]
    infrastructure: PathBuf,

    /// Route file in the Rolling Route format
    #[structopt(parse(from_os_str))]
    routes: PathBuf,
    
    /// Dispatch file in the Rolling Dispatch format
    #[structopt(parse(from_os_str))]
    dispatch: PathBuf,

    /// Output JSON history file
    #[structopt(short = "j", long = "json", parse(from_os_str))]
    json: Option<PathBuf>,

    /// Output JSON history as JavaScript
    #[structopt(short = "J", long = "javascript", parse(from_os_str))]
    javascript: Option<PathBuf>,

    /// Output format profile: full | timing
    #[structopt(short = "f", long = "format")]
    format: Option<String>,

    /// Output node visit times to file
    #[structopt(short = "n", long = "visits", parse(from_os_str))]
    visits: Option<PathBuf>,

    /// Output directed graph for graphical conversion
    #[structopt(short = "g", long = "graphical", parse(from_os_str))]
    graphical: Option<PathBuf>,

    /// Maximum time step 
    #[structopt(short = "d", long = "time-step")]
    timestep: Option<f64>,
}

fn run(opt :&Opt) -> AppResult<()> {
    // 
    // Infrastructure
    let (infrastructure,names) = get_infrastructure(&opt.infrastructure)?;
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

    if opt.graphical.is_none() {
        // Routes
        let routes = get_routes(&opt.routes, &names)?;
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
        let history = rolling::evaluate_plan(&infrastructure, &routes, &dispatch, opt.timestep);

        // Print
        println!("# Infrastructure history:");
        for x in &history.inf {
            println!("> {:?}", x);
        }
        for &(ref name,ref params, ref x) in &history.trains {
            println!("## Train \"{}\" {:?}:", name, params);
            for x in x {
                println!("> {:?}", x);
            }
        }

        if let Some(ref json) = opt.json {
            use std::fs::File;
            use std::io::BufWriter;
            let mut file = File::create(json)?;
            let mut writer = BufWriter::new(&file);
            rolling::output::json::json_history(&infrastructure, &names, &history, &mut writer)?;
        }

        if let Some(ref javascript) = opt.javascript {
            use std::fs::File;
            use std::io::BufWriter;
            let mut file = File::create(javascript)?;
            let mut writer = BufWriter::new(&file);
            rolling::output::json::javascript_history(&infrastructure, &names, &history, &mut writer)?;
        }
        
        if let Some(ref visits) = opt.visits {
            use std::fs::File;
            use std::io::BufWriter;
            let mut file = File::create(visits)?;
            let mut writer = BufWriter::new(&file);
            let string = rolling::output::history::visits(&names, &history)?;
            use std::io::Write;
            write!(writer,"{}",string)?;
        }
    }

    if let Some(ref graphical) = opt.graphical {
        use std::fs::File;
        use std::io::BufWriter;
        let string = rolling::output::graphical::graphical(&infrastructure, &names)?;
        use std::io::Write;
        let mut file = File::create(graphical)?;
        let mut writer = BufWriter::new(&file);
        write!(writer,"{}",string)?;
    }

    Ok(())
}

pub fn main() {
    let opt = Opt::from_args();
    println!("{:?}", opt);
    match run(&opt) {
        Ok(()) => {},
        Err(e) => {
            println!("Error:\n{}", e.as_fail());
            std::process::exit(1);
        },
    }
}
