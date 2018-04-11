extern crate minidom;
extern crate clap;

use std::fs::File;
use std::io::prelude::*;
use clap::{Arg, App};
use std::path::Path;

fn main() {
    let opts = App::new("railML 2.3 to Rolling converter")
        .about("Convert railML 2.3 files to rolling D-graph format")
        .arg(Arg::with_name("INPUT")
            .help("railML file")
            .required(true)
            .index(1))
        .arg(Arg::with_name("v")
            .short("v")
            .help("Level of verbosity"))
        .get_matches();

    let input_fn = opts.value_of("INPUT").unwrap();
    println!("Using input file: {}", input_fn);
    let verbose = opts.occurrences_of("v") > 0;

    match convert(Path::new(input_fn), verbose) {
        Ok(()) => println!("Success."),
        Err(e) => {
            println!("Failed: {}", e);
            std::process::exit(1);
        }
    }
}

fn convert(input_fn: &Path, verbose: bool) -> Result<(), String> {
    let mut f = File::open(input_fn).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let doc: minidom::Element = contents.parse().expect("XML parse error");
    let ns = doc.ns().ok_or("Missing XML namespace.")?;
    if verbose {
        println!("Namespace {:?}", ns);
    }

    if doc.name().to_lowercase() == "infrastructure" {
        convert_infrastructure(&doc, &ns, verbose)
    } else if doc.name().to_lowercase() == "railml" {
        let inf = doc.children()
            .filter(|x| x.name().to_lowercase() == "infrastructure")
            .nth(0)
            .ok_or("No infrastructure element found.".to_string())?;
        convert_infrastructure(inf, &ns, verbose)
    } else {
        Err("No infrastructure element found.".to_string())
    }
}

#[derive(Debug)]
pub struct TracksideObject {
}

#[derive(Debug)]
pub struct Switch {
    trailnode: String,
    leftnode: String,
    rightnode: String,
}

#[derive(Debug)]
pub struct LinearSection {
    endA: Option<String>,
    endB: Option<String>,
    objects: Vec<(f64, TracksideObject)>,
    length: f64,
}

fn convert_infrastructure(infrastructure: &minidom::Element,
                          ns: &str,
                          verbose: bool)
                          -> Result<(), String> {

    let tracks = infrastructure.children()
        .filter(|x| x.name().to_lowercase() == "tracks")
        .nth(0)
        .ok_or("No tracks found in infrastructure".to_string())?;


    for track in tracks.children().filter(|x| x.name().to_lowercase() == "track") {

        let mut sections = Vec::new();
        let mut sw_datas = Vec::new();

        let name = track.attr("name")
            .ok_or("No name for track".to_string())?;

        let topology = track.get_child("trackTopology", ns)
            .ok_or("No trackTopology found.".to_string())?;

        let trackLength = topology.get_child("trackEnd", ns)
            .ok_or("No trackEnd found.".to_string())?
            .attr("pos")
            .ok_or("No pos found on trackEnd.".to_string())?
            .parse::<f64>()
            .map_err(|e| format!("{:?}", e))?;

        let mut switches = topology.get_child("connections", ns)
            .map(|c| {
                c.children()
                    .filter(|x| x.name() == "switch")
                    .map(|sw| {
                        let pos = sw.attr("pos")
                            .expect("No pos found on switch.")
                            .parse::<f64>()
                            .unwrap();
                        (pos, sw)
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_else(|| Vec::new());

        switches.sort_by(|a, b| (a.0).partial_cmp(&b.0).expect("Switch position NaN"));

        println!("track {:?} with length {:?}", name, trackLength);

        let start_node = topology.get_child("trackBegin", ns)
            .ok_or("No trackBegin found.".to_string())?
            .get_child("connection", ns)
            .map(|c| c.attr("id").unwrap().to_string());

        let end_node = topology.get_child("trackEnd", ns)
            .ok_or("No trackBegin found.".to_string())?
            .get_child("connection", ns)
            .map(|c| c.attr("id").unwrap().to_string());

        println!("   - Start {:?}", start_node);
        println!("   - End {:?}", end_node);

        let mut start = 0.0;
        let mut node: Option<String> = start_node;

        for (i, sw) in switches.iter().enumerate() {
            let pos = (sw.1).attr("pos").unwrap().parse::<f64>().unwrap();

            let conn = (sw.1).children().filter(|x| x.name() == "connection").collect::<Vec<_>>();
            if conn.len() == 0 {
                return Err("No connection in switch.".to_string());
            }
            if conn.len() > 1 {
                return Err("Multiple connections in switch.".to_string());
            }

            let conn = conn[0];

            let conn_ref = conn.attr("ref").unwrap().to_string();
            let orientation = conn.attr("orientation").unwrap();
            let course = conn.attr("course").unwrap();

            let before_name = format!("spv-{}-{}-down", name, i);
            let after_name = format!("spv-{}-{}-up", name, i);

            sections.push(LinearSection {
                endA: node.clone(),
                endB: Some(before_name.clone()),
                objects: Vec::new(),
                length: pos - start,
            });

            let (trailnode,legnode) = match orientation {
                "outgoing" => (before_name.clone(), after_name.clone()),
                "incoming" => (after_name.clone(), before_name.clone()),
                _ => panic!("incoming outgoing error"),
            };

            let (leftnode, rightnode) = match course {
                "left" => (conn_ref.clone(), legnode),
                "right" => (legnode, conn_ref.clone()),
                _ => panic!("left right error"),
            };

            let sw_data = Switch { trailnode, leftnode, rightnode };
            sw_datas.push(sw_data);

            println!("  sw {:?} {:?} {:?} {:?} {:?}",
                     (sw.1).attr("name"),
                     pos,
                     conn_ref,
                     orientation,
                     course);

            start = pos;
            node = Some(after_name.clone());
        }

        sections.push(LinearSection {
            endA: node.clone(),
            endB: end_node.clone(),
            objects: Vec::new(),
            length: trackLength - start,
        });

        for x in &sections { println!("  sec {:?}", x); }
        for x in &sw_datas { println!("  sw {:?}", x); }
    }


    Ok(())
}
