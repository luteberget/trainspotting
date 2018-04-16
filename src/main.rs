extern crate minidom;
extern crate clap;

use std::fs::File;
use std::io::prelude::*;
use clap::{Arg, App};
use std::path::Path;

use std::collections::HashMap;

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
    //println!("Using input file: {}", input_fn);
    let verbose = opts.occurrences_of("v") > 0;

    match convert(Path::new(input_fn), verbose) {
        Ok(()) => {},//println!("Success."),
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

#[derive(Copy, Clone, Debug)]
pub enum Side { Left, Right }

#[derive(Copy, Clone, Debug)]
enum Direction { Up, Down }


#[derive(Debug)]
pub struct Switch {
    trailnode: String,
    leftnode: String,
    rightnode: String,
    side :Side,
}

#[derive(Debug,Clone)]
pub enum Connection {
    Stop,
    Boundary,
    Connection(String),
}

#[derive(Debug)]
pub struct LinearSection {
    end_a: Connection,
    end_b: Connection,
    objects: Vec<(f64, String, TracksideObject)>,
    length: f64,
}

enum RefSide { Id, Ref }

fn endpoint(refside :RefSide, node :&minidom::Element, ns :&str) -> Connection {
    let conn_attr = match refside {
        RefSide::Id => "id",
        RefSide::Ref => "ref",
    };

    if let Some(id) = node.get_child("connection",ns)
        .and_then(|c| c.attr(conn_attr)).map(|c| c.to_string()) {
            return Connection::Connection(id);
        }

    if let Some(_) = node.get_child("bufferStop", ns) {
        return Connection::Stop;
    }

    if let Some(_) = node.get_child("openEnd", ns) {
        return Connection::Boundary;
    }

    // TODO stderr
    //println!("Warning: track begin/end node has no connection, bufferStop or openEnd element. Defaulting to bufferStop.");
    return Connection::Stop;
}


#[derive(Clone, Debug)]
enum TracksideObject {
    Signal(Direction),
}


fn track_objects(track :&minidom::Element, ns: &str, _verbose: bool) -> Result<Vec<(f64,String,TracksideObject)>,String> {
    let signal_elements = track.get_child("ocsElements",ns)
        .and_then(|o| o.get_child("signals",ns))
        .map(|s| s.children().filter(|x| x.name() == "signal").collect())
        .unwrap_or_else(|| Vec::new());
    let signals = signal_elements.iter()
        .filter_map(|s| {
            let id = s.attr("id").unwrap();
            let _name = s.attr("name").unwrap();
            let name = id;
            let pos = s.attr("pos").unwrap().parse::<f64>().unwrap();
            let dir = match s.attr("dir") {
                Some("up") => Direction::Up,
                Some("down") => Direction::Down,
                Some(_) | None => {
                    //println!("Error: signal has no direction {:?} {:?}.", id, name);
                    return None
                }
            };
            let relevant_type = match s.attr("type") {
                Some("main") | Some("combined") => true,
                Some(_) => false,
                None => {
                    //println!("Warning: signal {:?} {:?} has no type, assuming main signal.", id, name);
                    true
                },
            };

            if relevant_type {
                Some((pos, name.to_string(), TracksideObject::Signal(dir)))
            } else {
                None
            }
        });

    Ok(signals.collect())
}

fn convert_infrastructure(infrastructure: &minidom::Element,
                          ns: &str,
                          verbose: bool)
                          -> Result<(), String> {

    let tracks = infrastructure.children()
        .filter(|x| x.name().to_lowercase() == "tracks")
        .nth(0)
        .ok_or("No tracks found in infrastructure".to_string())?;

    let mut sections = Vec::new();
    let mut sw_datas = Vec::new();

    for track in tracks.children().filter(|x| x.name().to_lowercase() == "track") {


        let name = track.attr("name")
            .ok_or("No name for track".to_string())?;

        let topology = track.get_child("trackTopology", ns)
            .ok_or("No trackTopology found.".to_string())?;

        let track_length = topology.get_child("trackEnd", ns)
            .ok_or("No trackEnd found.".to_string())?
            .attr("pos")
            .ok_or("No pos found on trackEnd.".to_string())?
            .parse::<f64>()
            .map_err(|e| format!("{:?}", e))?;

        let mut objects = track_objects(track, ns, verbose)?;
        //println!("Objects: {:?}", objects);

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

        objects.sort_by(|a, b| (a.0).partial_cmp(&b.0).expect("Object position NaN"));
        switches.sort_by(|a, b| (a.0).partial_cmp(&b.0).expect("Switch position NaN"));

        //println!("track {:?} with length {:?}", name, track_length);

        let track_begin = topology.get_child("trackBegin", ns)
            .ok_or("No trackBegin found.".to_string())?;
        let track_end = topology.get_child("trackEnd", ns)
            .ok_or("No trackBegin found.".to_string())?;
        let start_node = endpoint(RefSide::Id, track_begin, ns);
        let end_node = endpoint(RefSide::Ref, track_end, ns);

        //println!("   - Start {:?}", start_node);
        //println!("   - End {:?}", end_node);

        let mut start = 0.0;
        let mut node = start_node;

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
            let conn_id = conn.attr("id").unwrap().to_string();
            let orientation = conn.attr("orientation").unwrap();
            let course = conn.attr("course").unwrap();

            let before_name = format!("spv-{}-{}-down", name, i);
            let after_name = format!("spv-{}-{}-up", name, i);

            sections.push(LinearSection {
                end_a: node,
                end_b: Connection::Connection(before_name.clone()),
                objects: objects.iter().cloned().filter_map(
                    |(p,n,d)| if p >= start && p < pos { Some((p-start,n,d)) } else { None })
                    .collect(),
                length: pos - start,
            });

            let (conn_node, trailnode,legnode) = match orientation {
                "outgoing" => (conn_ref.clone(), before_name.clone(), after_name.clone()),
                "incoming" => (conn_id.clone(), after_name.clone(), before_name.clone()),
                _ => panic!("incoming outgoing error"),
            };

            let (side, leftnode, rightnode) = match course {
                "left" => (Side::Left, conn_node, legnode),
                "right" => (Side::Right, legnode, conn_node),
                _ => panic!("left right error"),
            };

            let sw_data = Switch { trailnode, leftnode, rightnode, side };
            sw_datas.push(sw_data);

            //println!("  sw {:?} {:?} {:?} {:?} {:?}",
            //         (sw.1).attr("name"),
            //         pos,
            //         conn_ref,
            //         orientation,
            //         course);

            start = pos;
            node = Connection::Connection(after_name.clone());
        }

        sections.push(LinearSection {
            end_a: node.clone(),
            end_b: end_node.clone(),
            objects: objects.iter().cloned().filter_map(
                |(p,n,d)| if p >= start { Some((p-start,n,d)) } else { None })
                .collect(),
            length: track_length - start,
        });

        //for x in &sections { println!("  sec {:?}", x); }
        //for x in &sw_datas { println!("  sw {:?}", x); }
    }

    let mut continuations = Vec::new();
    let mut conn_nodes = HashMap::new();
    let mut i = 1;
    for s in &mut sections {
        println!("node n{}a-n{}b -- {:?}", i,i,s.end_a);
        if let Connection::Connection(ref end_a) = s.end_a {
            if conn_nodes.get(end_a).is_some() {
                let other_node = conn_nodes.remove(end_a).unwrap();
                continuations.push((format!("n{}a",i), other_node));
            } else {
               conn_nodes.insert(end_a.clone(), format!("n{}a",i));
            }
        }
        if let Connection::Boundary = s.end_a {
            println!("boundary n{}a",i);
        }

        let mut last_b    = format!("n{}b",i);
        i += 1;

        let mut last_pos = 0.0;
        for &(ref obj_pos,ref obj_name,ref obj_data) in &s.objects {
            println!("linear {}-n{}a {}", last_b, i, obj_pos - last_pos);
            let (down_obj,up_obj) = match *obj_data {
                TracksideObject::Signal(Direction::Up) => ("".to_string(),format!("(signal {})",obj_name)),
                TracksideObject::Signal(Direction::Down) => (format!("(signal {})",obj_name),"".to_string()),
            };
            println!("node n{}a{}-n{}b{} -- {:?}", i, down_obj, i, up_obj, obj_name);

            last_b = format!("n{}b",i);
            i += 1;
        }

        println!("linear {}-n{}a {}", last_b, i, s.length - last_pos);

        println!("node n{}a-n{}b -- {:?}", i,i,s.end_b);
        if let Connection::Connection(ref end_b) = s.end_b {
            if conn_nodes.get(end_b).is_some() {
                let other_node = conn_nodes.remove(end_b).unwrap();
                continuations.push((format!("n{}b",i), other_node));
            } else {
               conn_nodes.insert(end_b.clone(), format!("n{}b",i));
            }
        }
        i += 1;
    }

    let mut i = 1;
    for sw in &mut sw_datas {
        let side = match sw.side {
            Side::Left => "left",
            Side::Right => "right",
        };
        // println!("KEYS {:?}", sw);
        let n1 = conn_nodes.remove(&sw.trailnode).unwrap();
        let n2 = conn_nodes.remove(&sw.leftnode).unwrap();
        let n3 = conn_nodes.remove(&sw.rightnode).unwrap();
        println!("switch sw{} {} {}-({} 0.0, {} 0.0)", i, side, 
                 n1,n2,n3);
        i += 1;
    }

    for (k,v) in conn_nodes.into_iter() {
        // TODO stderr
        //println!("Error: connection missing {:?}", (k,v));
    }

    for (a,b) in continuations.into_iter() {
        println!("linear {}-{} 0.0", a,b);
    }

    Ok(())
}
