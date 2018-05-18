extern crate minidom;
extern crate clap;
extern crate petgraph;

use std::fs::File;
use std::io::prelude::*;
use clap::{Arg, App};
use std::path;

use std::collections::HashMap;
use std::collections::HashSet;

struct Opts<'a> {
    input_fn: &'a path::Path,
    infrastructure_fn: Option<&'a path::Path>,
    routes_fn: Option<&'a path::Path>,
    verbose: bool,
}

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

// fn copy_objects(nodes :&mut Vec<DGraphNode>, a :PartNodeIdx, b:PartNodeIdx) {
// }
//
// fn join_nodes(model :&mut Model, tol :f64) {
//     use Edge::*;
//     let mut edges_to :HashMap<PartNodeIdx,()>= HashMap::new();
//     for e in &model.edges {
//         match *e {
//             Edge::Linear(a,(b,d)) => {
//             },
//             Edge::Switch(ref name, _side, n1, (nl,dl),(nr,dr)) => {
//             },
//             Edge::Boundary(_) => {},
//         }
//     }
//
//     let mut edges = &mut model.edges;
//     let mut nodes = &mut model.nodes;
//
//     edges.retain(|e| match *e {
//             Edge::Linear(a,(b,d)) => {
//                 if d < tol {
//                     copy_objects(&mut nodes, b, a);
//                     nodes[
//                 }
//                 true
//             },
//             Edge::Switch(ref name, _side, n1, (nl,dl),(nr,dr)) => {
//                 true
//             },
//             Edge::Boundary(_) => true,
//     });
// }

fn run(opts: &Opts) -> Result<(), String> {
    // 1. read xml
    let (doc, ns) = get_xml(opts.input_fn, opts.verbose)?;
    // 2. find infrastructur
    let infrastructure = get_infrastructure_element(&doc, &ns)?;
    // 3. convert infrastructure to dgraph
    let mut dgraph = convert_infrastructure(infrastructure, &ns, opts.verbose)?;
    // 4. find detection sections
    create_sections_from_detectors(&mut dgraph);
    // 7. calculate sight nodes
    // create_sight(&mut dgraph);

    // 5. Join zero-length edges
    //  ---> instead of removing zero-length edges,
    //  it could better be viewed as removing empty nodes, because they have no use.
    //
    // 6. Join small edges within tolerance? (Signal and detector in same node?)
    // join_nodes(&mut dgraph, 1.0); // 1m tolerance
    //
    // 8. create routes
    let routes = find_routes(&dgraph);

    // 9. output infrastructure rolling dgraph format
    if let Some(f) = opts.infrastructure_fn {
        let mut buffer = File::create(f).map_err(|e| e.to_string())?;
        print_rolling(&mut buffer, &dgraph).map_err(|e| e.to_string())?;
    }

    // 10. output routes
    if let Some(f) = opts.routes_fn {
        let mut buffer = File::create(f).map_err(|e| e.to_string())?;
        print_routes(&mut buffer, &dgraph, &routes).map_err(|e| e.to_string())?;
    }

    Ok(())
}


#[derive(Debug, Clone)]
pub enum RouteBoundary {
    ModelBoundary(PartNodeIdx),
    Signal(String),
}

#[derive(Debug, Clone)]
pub struct Route {
    entry: RouteBoundary,
    exit: RouteBoundary,
    sections: Vec<String>,
    switches: Vec<(String, Side)>,
    releases: Vec<(String, f64, Vec<String>)>,
    length: f64,
}

#[derive(Debug, Clone)]
pub struct RouteEntry {
    node: PartNodeIdx,
    entry: RouteBoundary,
    section: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Path {
    node: PartNodeIdx,
    length: f64,
    entered_sections: Vec<(String, f64)>,
    exited_sections: Vec<(String, f64, f64)>,
    switches: Vec<(String, f64, Side)>,
}

#[derive(Debug, Clone)]
pub enum DirEdge {
    Linear(Link),
    FacingSwitch(String, Link, Link),
    TrailingSwitch(String, Side, Link),
    Boundary,
}


fn switch_list(sw :&[(String, f64, Side)]) -> Vec<(String, Side)> {
    sw.iter().map(|x| (x.0.clone(), x.2)).collect()
}

pub fn convert_state_to_route(state: &Path, entry: RouteBoundary,  
                              exit: RouteBoundary) -> Option<Route> {

    let section_tolerance = 15.0;

    if state.length < section_tolerance {
        println!("Warning: route too short");
        return None;
    }

    let mut sections = state.exited_sections.clone();
    sections.extend(state.entered_sections.iter()
                    .map(|&(ref x,l)| (x.clone(), l, state.length)));
    sections.retain(|&(_,a,b)| (b-a) > section_tolerance);

    let mut cleared_length = 0.0;
    let mut releases = sections.iter().map(|&(ref x,start,end)| {
        let trigger = x.clone();
        let start = if cleared_length > start { cleared_length } else { start };
        let length = end-start;
        cleared_length += length;
        let mut resources = vec![trigger.clone()];
        for &(ref sw,pos,_side) in &state.switches {
            if start <= pos && pos < end {
                resources.push(sw.clone());
            }
        }
        (trigger,length,resources)
    }).collect::<Vec<_>>();

    let release_length = releases.iter().map(|&(_,l,_)| l).sum::<f64>();
    if releases.len() > 0 && release_length != state.length {
        println!("Release length and route length differ by {} {} {:?} {:?}", state.length, release_length, entry, exit);
        releases.last_mut().unwrap().1 += state.length - release_length;
    }

    let route = Route {
        length: state.length,
        entry: entry,
        exit: exit,
        sections: sections.iter().map(|&(ref x,_,_)| x.clone()).collect(),
        switches: state.switches.iter().map(|&(ref x,_,s)| (x.clone(), s)).collect(),
        releases: releases,
    };
    //println!("Route {:?}", route);

    Some(route)
}

fn convert_model(model  :&Model) -> HashMap<PartNodeIdx, DirEdge> {
    let mut dir_edges: HashMap<PartNodeIdx, DirEdge> = HashMap::new();
    for edge in &model.edges {
        match *edge {
            Edge::Linear(a, (b, d)) => {
                dir_edges.insert(a, DirEdge::Linear((b, d)));
                dir_edges.insert(b, DirEdge::Linear((a, d)));
            }
            Edge::Switch(ref name, _side, n1, (nl, dl), (nr, dr)) => {
                dir_edges.insert(n1, DirEdge::FacingSwitch(name.clone(), (nl, dl), (nr, dr)));
                dir_edges.insert(nl,
                                 DirEdge::TrailingSwitch(name.clone(), Side::Left, (n1, dl)));
                dir_edges.insert(nr,
                                 DirEdge::TrailingSwitch(name.clone(), Side::Right, (n1, dr)));
            }
            Edge::Boundary(n) => {
                dir_edges.insert(n, DirEdge::Boundary);
            }
        }
    }
    dir_edges
}

pub fn find_routes(model: &Model) -> Vec<Route> {
    let mut routes = Vec::new();
    let dir_edges = convert_model(&model);
    let boundary_nodes = model.edges.iter().filter_map(|x| if let Edge::Boundary(n) = *x {
        Some(n)
    } else {
        None
    });

    let mut entry_visited = HashSet::new();
    for boundary in boundary_nodes {
        println!("Boundary start {:?}", boundary);
        let mut entry_stack = Vec::new();
        entry_stack.push(RouteEntry {
            node: boundary.opposite(),
            entry: RouteBoundary::ModelBoundary(boundary),
            section: None,
        });
        entry_visited.insert(boundary.opposite());

        while entry_stack.len() > 0 {
            let entry = entry_stack.pop().unwrap();
            let mut search_stack = Vec::new();

            //println!("Entry from {:?}", entry);

            // A route path may only visit a given switch in a given direction
            // (trailing or facing) once, because using a given switch
            // several times would mean either:
            //  a. The path goes over the same switch in the same position twice,
            //     which means that there was no end point between the visits,
            //     and since all switches between these visits need to be in the
            //     same position.
            //  b. The path goes over the switch twice in different directions,
            //     which is not something a route should be able to provide.
            //     All movable elements must be locked in place to activate a route
            //     so it would not be possible to require a switch to be in
            //     two positions at once.

            let mut switches_path_visited = HashSet::new();

            search_stack.push(Path {
                node: entry.node,
                entered_sections: entry.section.iter().map(|x| (x.clone(), 0.0)).collect(),
                exited_sections: vec![],
                switches: vec![],
                length: 0.0,
            });

            while search_stack.len() > 0 {
                let mut curr_state = search_stack.pop().unwrap();

                loop {
                    let mut is_exit = false;
                    if curr_state.node != entry.node {

                        // Check what is in here
                        //
                        let node = model.nodes[curr_state.node.node_idx()]
                            .get_part(curr_state.node.node_part());

                        for obj in &node.objs {
                            use PartNodeObject::*;
                            match *obj {
                                Signal(ref x) => {

                                    if let Some(route) = convert_state_to_route(&curr_state, 
                                                            entry.entry.clone(), RouteBoundary::Signal(x.clone())) {
                                        routes.push(route);
                                    } else {
                                        println!("Warning: Route conversion failed");
                                    }

                                    if entry_visited.insert(curr_state.node) {
                                        entry_stack.push(RouteEntry {
                                            node: curr_state.node,
                                            entry: RouteBoundary::Signal(x.clone()),
                                            section: curr_state.entered_sections.iter().nth(0).map(|x| x.0.clone()),
                                        });
                                    }

                                    is_exit = true;
                                }
                                TVDEnter(ref x) => {
                                    curr_state.entered_sections.push((x.clone(), curr_state.length));
                                }
                                TVDExit(ref x) => {
                                    if let Some(i) = curr_state.entered_sections.iter().position(|y| y.0 == *x) {
                                        let e = curr_state.entered_sections.remove(i);
                                        curr_state.exited_sections.push((e.0, e.1, curr_state.length));
                                    } else {
                                        panic!("Exited unexpected section");
                                    }
                                }
                                Sight(_,_) => {}
                            }
                        }
                    }

                    if is_exit { break; }

                    match dir_edges.get(&curr_state.node).cloned() {
                        Some(DirEdge::Linear((other,d))) => {
                            curr_state.node = other.opposite();
                            curr_state.length += d;
                        },
                        Some(DirEdge::TrailingSwitch(sw,pos,(other,d))) => {
                            curr_state.node = other.opposite();
                            curr_state.length += d;
                            curr_state.switches.push((sw,curr_state.length,pos));
                            if !switches_path_visited.insert(switch_list(&curr_state.switches)) {
                                // We have been here before. Abort without adding a new route,
                                // to avoid having routes with loops.
                                break;
                            }
                        },
                        Some(DirEdge::FacingSwitch(sw, (other1, d1), (other2, d2))) => {
                            let mut right_state = curr_state.clone();

                            curr_state.node = other1.opposite();
                            curr_state.switches.push((sw.clone(), curr_state.length, Side::Left));
                            curr_state.length += d1;

                            right_state.node = other2.opposite();
                            right_state.switches.push((sw, curr_state.length, Side::Right));
                            right_state.length += d2;

                            if switches_path_visited.insert(switch_list(&right_state.switches)) {
                                search_stack.push(right_state);
                            }
                            if !switches_path_visited.insert(switch_list(&curr_state.switches)) {
                                break;
                            }
                        },
                        Some(DirEdge::Boundary) => {
                            if let Some(route) = convert_state_to_route(&curr_state, 
                                        entry.entry.clone(), RouteBoundary::ModelBoundary(curr_state.node)) {
                                routes.push(route);
                                break; 
                            } else {
                                panic!("Could not convert route");
                            }
                        },
                        None => {
                            break; 
                        }
                    }
                }
            }
        }
    }

    // Remove release of resources that were not aquired
    for r in &mut routes {
        let resources = r.sections.iter().chain(r.switches.iter().map(|&(ref sw,_)| sw)).collect::<Vec<_>>();
        for &mut (_,_,ref mut res) in &mut r.releases {
            res.retain(|x| resources.contains(&x));
        }
    }

    routes
}


pub fn repr_partnodeobject(obj: &PartNodeObject) -> String {
    use PartNodeObject::*;
    match *obj {
        Signal(ref x) => format!("signal {}", x),
        TVDEnter(ref x) => format!("enter {}", x),
        TVDExit(ref x) => format!("exit {}", x),
        Sight(ref x, l) => format!("sight {} {}",x,l),
    }
}

pub fn repr_partnode(objs: &[PartNodeObject]) -> String {
    if objs.len() > 0 {
        format!("({})",
                objs.iter()
                    .map(|o| repr_partnodeobject(o))
                    .collect::<Vec<_>>()
                    .join(", "))
    } else {
        "".to_string()
    }
}

pub fn print_resources<W: std::io::Write>(buf: &mut W,
                                          _model: &Model,
                                          route: &Route)
                                          -> std::io::Result<()> {
    writeln!(buf, "  sections [{}]", route.sections.join(", "))?;
    writeln!(buf,
             "  switches [{}]",
             route.switches
                 .iter()
                 .map(|&(ref sw, pos)| format!("{} {}", sw, pos.as_str()))
                 .collect::<Vec<_>>()
                 .join(", "))?;
    writeln!(buf, "  contains []")?;
    Ok(())
}

pub fn print_releases<W: std::io::Write>(buf: &mut W,
                                         _model: &Model,
                                         route :&Route)
                                        -> std::io::Result<()> {


    // Don't use partial release on exit routes.
    // Just omit printing here, so that consumers will 
    // create default release behavior.
    let exit_route = if let RouteBoundary::ModelBoundary(_) = route.exit { true } else { false };

    if !exit_route {
        for &(ref trigger,length,ref resources) in &route.releases {
            let reslist = resources.clone().join(", ");
            writeln!(buf, "  release {{ length {} trigger {} resources [{}] }}", 
                     length, trigger, reslist)?;
        }
    }
    Ok(())
}

pub fn print_routes<W: std::io::Write>(buf: &mut W,
                                       model: &Model,
                                       routes: &Vec<Route>)
                                       -> std::io::Result<()> {
    let mut i = 1;
    for r in routes {
        use RouteBoundary::*;
        match (&r.entry, &r.exit) {
            (&ModelBoundary(ref b), &Signal(ref s)) => {
                writeln!(buf,
                         "modelentry r{} from {} {{",
                         i,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name)?;
                writeln!(buf, "  exit {}", s)?;
                writeln!(buf, "  length {}", r.length)?;
                print_resources(buf, model, r)?;
                print_releases(buf, model, r)?;
                writeln!(buf, "}}")?;
            }
            (&Signal(ref s), &ModelBoundary(ref b)) => {
                writeln!(buf,
                         "modelexit r{} to {} {{",
                         i,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name)?;
                writeln!(buf, "  entry {}", s)?;
                if let Some(s) = r.sections.get(0) {
                    writeln!(buf, "  entrysection {}", s)?;
                }
                writeln!(buf, "  length {}", r.length + 1000.0)?;
                print_resources(buf, model, r)?;
                print_releases(buf, model, r)?;
                writeln!(buf, "}}")?;
            }
            (&Signal(ref s1), &Signal(ref s2)) => {
                writeln!(buf, "route r{} {{", i)?;
                writeln!(buf, "  entry {}", s1)?;
                writeln!(buf, "  exit {}", s2)?;
                if let Some(s) = r.sections.get(0) {
                    writeln!(buf, "  entrysection {}", s)?;
                }
                writeln!(buf, "  length {}", r.length)?;
                print_resources(buf, model, r)?;
                print_releases(buf, model, r)?;
                writeln!(buf, "}}")?;
            }
            (&ModelBoundary(ref b1), &ModelBoundary(ref b2)) => {
                println!("Warning: boundaries {:?} to {:?} are reachable without passing a \
                          signal.",
                         b1,
                         b2);
            }
        }

        i += 1;
    }
    Ok(())
}

pub fn print_rolling<W: std::io::Write>(buf: &mut W, model: &Model) -> std::io::Result<()> {
    for n in &model.nodes {
        writeln!(buf,
                 "node {}{}-{}{}",
                 n.a.name,
                 repr_partnode(&n.a.objs),
                 n.b.name,
                 repr_partnode(&n.b.objs))?;
    }
    for e in &model.edges {
        match *e {
            Edge::Linear(a, (b, d)) => {
                writeln!(buf,
                         "linear {}-{} {}",
                         model.nodes[a.node_idx()].get_part(a.node_part()).name,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name,
                         d)?
            }
            Edge::Switch(ref name, side, n1, (n2, d2), (n3, d3)) => {
                writeln!(buf,
                         "switch {} {} {}-({} {}, {} {})",
                         name,
                         match &side { &Some(ref s) => s.as_str(), 
                             &None => "unknown" },
                         model.nodes[n1.node_idx()].get_part(n1.node_part()).name,
                         model.nodes[n2.node_idx()].get_part(n2.node_part()).name,
                         d2,
                         model.nodes[n3.node_idx()].get_part(n3.node_part()).name,
                         d3)?
            }
            Edge::Boundary(n) => {
                writeln!(buf,
                         "boundary {}",
                         model.nodes[n.node_idx()].get_part(n.node_part()).name)?
            }

        }
    }

    Ok(())
}



fn get_xml(input_fn: &path::Path, verbose: bool) -> Result<(minidom::Element, String), String> {
    let mut f = File::open(input_fn).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let doc: minidom::Element = contents.parse().expect("XML parse error");
    let ns = doc.ns().ok_or("Missing XML namespace.")?.to_string();
    if verbose {
        println!("Namespace {:?}", ns);
    }

    Ok((doc, ns))
}

fn get_infrastructure_element<'a>(doc: &'a minidom::Element,
                                  _ns: &str)
                                  -> Result<&'a minidom::Element, String> {
    if doc.name().to_lowercase() == "infrastructure" {
        Ok(doc)
    } else if doc.name().to_lowercase() == "railml" {
        let inf = doc.children()
            .filter(|x| x.name().to_lowercase() == "infrastructure")
            .nth(0)
            .ok_or("No infrastructure element found.".to_string())?;
        Ok(inf)
    } else {
        Err("No infrastructure element found.".to_string())
    }
}

#[derive(Debug,Clone)]
pub struct Model {
    nodes: Vec<DGraphNode>,
    edges: Vec<Edge>,
}

pub type Link = (PartNodeIdx, f64);

#[derive(Debug,Clone)]
pub enum Edge {
    Linear(PartNodeIdx, Link),
    Switch(String, Option<Side>, PartNodeIdx, Link, Link),
    Boundary(PartNodeIdx),
}

#[derive(Debug,Clone)]
pub struct DGraphNode {
    name: Option<String>, //??
    a: PartNode,
    b: PartNode,
    both: Vec<TracksideObject>,
    removed: bool,
}

impl DGraphNode {
    fn get_part(&self, p: NodePart) -> &PartNode {
        match p {
            NodePart::A => &self.a,
            NodePart::B => &self.b,
        }
    }
}

#[derive(Debug,Clone)]
pub struct PartNode {
    name: String,
    objs: Vec<PartNodeObject>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Side {
    Left,
    Right,
}

impl Side {
    pub fn as_str(&self) -> &str {
        match *self {
            Side::Left => "left",
            Side::Right => "right",
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
}


#[derive(Debug)]
pub struct Switch {
    trailnode: String,
    leftnode: String,
    rightnode: String,
    side: Option<Side>,
}

#[derive(Debug,Clone)]
pub enum Connection {
    Stop,
    Boundary(Option<String>),
    Connection(String),
}

#[derive(Debug)]
pub struct LinearSection {
    end_a: Connection,
    end_b: Connection,
    objects: Vec<(f64, String, TracksideObject)>,
    length: f64,
}

enum RefSide {
    Id,
    Ref,
}

fn endpoint(refside: RefSide, node: &minidom::Element, ns: &str) -> Connection {
    let conn_attr = match refside {
        RefSide::Id => "id",
        RefSide::Ref => "ref",
    };

    if let Some(id) = node.get_child("connection", ns)
        .and_then(|c| c.attr(conn_attr))
        .map(|c| c.to_string()) {
        return Connection::Connection(id);
    }

    if let Some(_) = node.get_child("bufferStop", ns) {
        return Connection::Stop;
    }

    if let Some(e) = node.get_child("openEnd", ns) {
        let name = e.attr("id");
        return Connection::Boundary(name.map(|x| x.to_string()));
    }

    // TODO stderr
    // println!("Warning: track begin/end node has no connection,
    // bufferStop or openEnd element. Defaulting to bufferStop.");
    return Connection::Stop;
}


#[derive(Clone, Debug)]
enum TracksideObject {
    Signal(Direction, f64),
    Sight(Direction, String, f64),
    Detector,
}

#[derive(Clone, Debug)]
pub enum PartNodeObject {
    Signal(String),
    TVDEnter(String),
    TVDExit(String),
    Sight(String,f64),
}


fn track_objects(track: &minidom::Element,
                 ns: &str,
                 _verbose: bool)
                 -> Result<Vec<(f64, String, TracksideObject)>, String> {
    let signal_elements = track.get_child("ocsElements", ns)
        .and_then(|o| o.get_child("signals", ns))
        .map(|s| s.children().filter(|x| x.name() == "signal").collect())
        .unwrap_or_else(|| Vec::new());

    let signals = signal_elements.iter()
        .filter_map(|s| {
            let id = s.attr("id").expect("signal id missing");
            let _name = s.attr("name").expect("signal name missing");
            let name = id;
            let pos = s.attr("pos").expect("signal pos missing")
                .parse::<f64>().expect("signal pos invalid number");
            let sight = s.attr("sight")
                .map(|x| x.parse::<f64>().expect("signal sight invalid number"))
                .unwrap_or_else(|| {
                    println!("Warning: no sight info for signal {:?}", name);
                    200.0
                });
            let dir = match s.attr("dir") {
                Some("up") => Direction::Up,
                Some("down") => Direction::Down,
                Some(_) | None => {
                    // println!("Error: signal has no direction {:?} {:?}.", id, name);
                    return None;
                }
            };
            let relevant_type = match s.attr("type") {
                Some("main") | Some("combined") => true,
                Some(_) => false,
                None => {
                    // println!("Warning: signal {:?} {:?} has
                    // no type, assuming main signal.", id, name);
                    true
                }
            };

            if relevant_type {
                Some((pos, name.to_string(), TracksideObject::Signal(dir, sight)))
            } else {
                None
            }
        });

    let detector_elements = track.get_child("ocsElements", ns)
        .and_then(|o| o.get_child("trainDetectionElements", ns))
        .map(|s| {
            s.children()
                .filter(|x| x.name() == "trainDetector" || x.name() == "trackCircuitBorder")
                .collect()
        })
        .unwrap_or_else(|| Vec::new());

    let detectors = detector_elements.iter()
        .map(|d| {
            let id = d.attr("id").expect("detector id missing");
            //let _name = d.attr("name").expect("detector name missing");
            let name = id;
            let pos = d.attr("pos").expect("detector pos missing")
                .parse::<f64>().expect("detector pos number invalid");

            let _dir = match d.attr("dir") {
                Some("up") | Some("down") => {
                    println!("Warning: directional detector not supported ({:?}).", name);
                    ()
                }
                _ => (),
            };

            (pos, name.to_string(), TracksideObject::Detector)
        });

    Ok(signals.chain(detectors).collect())
}

type NodeIdx = usize;

#[derive(Copy,Clone,Debug,PartialEq,Eq,Hash)]
pub struct PartNodeIdx(usize);

#[derive(Copy,Clone,Debug)]
pub enum NodePart {
    A,
    B,
}

impl PartNodeIdx {
    pub fn from_node_part(n: NodeIdx, p: NodePart) -> PartNodeIdx {
        PartNodeIdx(2 * n + 1 +
                    match p {
            NodePart::A => 0,
            NodePart::B => 1,
        })
    }

    pub fn from_node(n: NodeIdx) -> (PartNodeIdx, PartNodeIdx) {
        (PartNodeIdx(2 * n + 1), PartNodeIdx(2 * n + 1 + 1))
    }

    pub fn to_usize(&self) -> usize {
        self.0
    }

    pub fn node_idx(&self) -> usize {
        (self.0 - 1) / 2
    }
    pub fn node_part(&self) -> NodePart {
        if (self.0 - 1) % 2 == 0 {
            NodePart::A
        } else {
            NodePart::B
        }
    }

    pub fn opposite(&self) -> PartNodeIdx {
        match self.node_part() {
            NodePart::A => PartNodeIdx::from_node_part(self.node_idx(), NodePart::B),
            NodePart::B => PartNodeIdx::from_node_part(self.node_idx(), NodePart::A),
        }
    }
}

fn create_sections_from_detectors(m: &mut Model) {

    let num_nodes = m.nodes.len() * 2 + 1;
    let is_boundary_idx = 0;
    let mut sets = petgraph::unionfind::UnionFind::new(num_nodes);

    for (node_idx, node) in m.nodes.iter().enumerate() {

        // Zero is the special boundary marker
        // 2n+1 is the A part, and 2n+2 is the B part of the node.
        let (a_idx, b_idx) = (2 * node_idx + 1, 2 * node_idx + 2);

        let has_detector = node.both.iter().any(|x| if let TracksideObject::Detector = *x {
            true
        } else {
            false
        });
        if has_detector {
            // println!("Detector at {:?}", node);
        } else {
            sets.union(a_idx, b_idx);
        }
    }

    for edge in &m.edges {
        use Edge::*;
        match *edge {
            Linear(n1, (n2, _)) => sets.union(n1.to_usize(), n2.to_usize()),
            Switch(_, _, n1, (n2, _), (n3, _)) => {
                sets.union(n1.to_usize(), n2.to_usize());
                sets.union(n1.to_usize(), n3.to_usize())
            }
            Boundary(n) => sets.union(is_boundary_idx, n.to_usize()),
        };
    }

    let mut sec_num_counter = 0;
    let mut sec_num_map = HashMap::new();

    // Go back to each node and insert tvd entry/exit
    for (node_idx, node) in m.nodes.iter_mut().enumerate() {
        let (a_idx, b_idx) = (2 * node_idx + 1, 2 * node_idx + 2);

        let has_detector = node.both.iter().any(|x| if let TracksideObject::Detector = *x {
            true
        } else {
            false
        });
        if has_detector {

            let a_section = sets.find(a_idx);
            let b_section = sets.find(b_idx);


            if a_section != sets.find(is_boundary_idx) {
                if sec_num_map.get(&a_section).is_none() {
                    sec_num_map.insert(a_section, sec_num_counter);
                    sec_num_counter += 1;
                }
                let a_section_name = format!("sec{}", sec_num_map[&a_section]);
                node.a.objs.push(PartNodeObject::TVDEnter(a_section_name.clone()));
                node.b.objs.push(PartNodeObject::TVDExit(a_section_name));
            } else {
                // println!("Side A of detector {:?} is boundary", node);
            }

            if b_section != sets.find(is_boundary_idx) {
                if sec_num_map.get(&b_section).is_none() {
                    sec_num_map.insert(b_section, sec_num_counter);
                    sec_num_counter += 1;
                }

                let b_section_name = format!("sec{}", sec_num_map[&b_section]);
                node.b.objs.push(PartNodeObject::TVDEnter(b_section_name.clone()));
                node.a.objs.push(PartNodeObject::TVDExit(b_section_name));
            } else {
                // println!("Side B of detector {:?} is boundary", node);
            }
        }
    }

    // println!("Union find is done: {:?}", sets);
}

fn empty_node(name: &str) -> DGraphNode {
    DGraphNode {
        removed: false,
        name: None,
        a: PartNode {
            name: format!("{}a", name),
            objs: vec![],
        },
        b: PartNode {
            name: format!("{}b", name),
            objs: vec![],
        },
        both: vec![],
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum SectionEnd {
    Begin, End,
}


fn mk_sight(sections: &mut [LinearSection], links: &HashMap<(usize,SectionEnd),Vec<(usize,SectionEnd)>>) {
    let mut signals = sections.iter()
        .enumerate()
        .flat_map(|(i, x)| {
            x.objects
                .iter()
                .filter_map(|&(pos, ref name, ref obj)| {
                    if let TracksideObject::Signal(dir, l) = *obj {
                        Some((i, pos, l,l, dir, name.clone()))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    while signals.len() > 0 {
        let (sec,pos,sight_dist,remaining_dist,dir,name) = signals.pop().unwrap();
        match dir {
            Direction::Up => {
                if pos - remaining_dist > 0.0  {
                    sections[sec].objects.push((pos- remaining_dist, "".to_string(), 
                        TracksideObject::Sight(dir,name, sight_dist)));
                } else {
                    match links.get(&(sec,SectionEnd::Begin)) {
                        Some(pts) => {
                            for pt in pts {
                                match pt {
                                    &(newsec, SectionEnd::End) => {
                                        signals.push((newsec, sections[newsec].length, sight_dist, remaining_dist - pos, dir, name.clone()));
                                    },
                                    &(newsec, SectionEnd::Begin) => {
                                        signals.push((newsec, 0.0, sight_dist, remaining_dist - pos, Direction::Up, name.clone()));
                                    },
                                }
                            }
                        },
                        None => {
                            let deficiency = remaining_dist - pos;
                            println!("Warning: sighting distance for \"{}\" truncated by {} from {} to {}", name, deficiency, sight_dist, sight_dist - deficiency);
                            sections[sec].objects.push((0.0, "".to_string(), 
                                TracksideObject::Sight(dir,name,sight_dist - deficiency)));
                        }
                    }
                }
            },
            Direction::Down => {
                if pos + remaining_dist < sections[sec].length {
                    sections[sec].objects.push((pos+remaining_dist, "".to_string(), 
                        TracksideObject::Sight(dir, name, sight_dist)));
                } else {
                    match links.get(&(sec, SectionEnd::End)) {
                        Some(pts) => {
                            for pt in pts {
                                match pt {
                                    &(newsec, SectionEnd::Begin) => {
                                        signals.push((newsec, 0.0, sight_dist, remaining_dist - (sections[sec].length - pos), dir, name.clone()));
                                    },
                                    &(newsec, SectionEnd::End) => {
                                        signals.push((newsec, sections[newsec].length, sight_dist, remaining_dist - (sections[sec].length - pos), Direction::Down, name.clone()));
                                    },
                                }
                            }
                        },
                        None => {
                            let deficiency = remaining_dist - (sections[sec].length - pos);
                            println!("Warning: sighting distance for \"{}\" truncated by {} from {} to {}", name, deficiency, sight_dist, sight_dist - deficiency);
                            sections[sec].objects.push((sections[sec].length, "".to_string(), 
                                TracksideObject::Sight(dir,name,sight_dist - deficiency)));
                        }
                    }
                }
            }
        }

    }
}

fn convert_infrastructure(infrastructure: &minidom::Element,
                          ns: &str,
                          verbose: bool)
                          -> Result<Model, String> {

    let tracks = infrastructure.children()
        .filter(|x| x.name().to_lowercase() == "tracks")
        .nth(0)
        .ok_or("No tracks found in infrastructure".to_string())?;

    let mut sections = Vec::new();
    let mut sw_datas = Vec::new();

    // map[Detector] = Node
    // name of detectore and index of its node
    // Set of nodes containing detectors (for splitting the graph)
    // let mut detector_nodes :HashSet<usize> = HashMap::new();

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
        // println!("Objects: {:?}", objects);

        let mut switchesandcrossings = topology.get_child("connections", ns)
            .map(|c| {
                c.children()
                    .filter(|x| x.name() == "switch" /*|| x.name() == "crossing"*/)
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

        // println!("OBJECTs on track: {:?}", objects);
        switchesandcrossings.sort_by( |a, b| (a.0).partial_cmp(&b.0).expect("Switch position NaN"));

        // println!("track {:?} with length {:?}", name, track_length);

        let track_begin = topology.get_child("trackBegin", ns)
            .ok_or("No trackBegin found.".to_string())?;
        let track_end = topology.get_child("trackEnd", ns)
            .ok_or("No trackBegin found.".to_string())?;
        let start_node = endpoint(RefSide::Id, track_begin, ns);
        let end_node = endpoint(RefSide::Ref, track_end, ns);

        // println!("   - Start {:?}", start_node);
        // println!("   - End {:?}", end_node);

        let mut start = 0.0;
        let mut node = start_node;

        for (i, sw) in switchesandcrossings.iter().enumerate() {
            let pos = (sw.1).attr("pos").unwrap().parse::<f64>().unwrap();

            let before_name = format!("spv-{}-{}-down", name, i);
            let after_name = format!("spv-{}-{}-up", name, i);

            if (sw.1).name() == "switch" {

                let conn = (sw.1)
                    .children()
                    .filter(|x| x.name() == "connection")
                    .collect::<Vec<_>>();
                if conn.len() == 0 {
                    return Err("No connection in switch.".to_string());
                }
                if conn.len() > 1 {
                    return Err("Multiple connections in switch.".to_string());
                }

                let conn = conn[0];

                let conn_ref = conn.attr("ref").expect("switch connection without reference").to_string();
                let conn_id = conn.attr("id").expect("switch connection without id").to_string();
                let orientation = conn.attr("orientation").expect("switch connection without orientation");
                let course = conn.attr("course");

                let (conn_node, trailnode, legnode) = match orientation {
                    "outgoing" => (conn_ref.clone(), before_name.clone(), after_name.clone()),
                    "incoming" => (conn_id.clone(), after_name.clone(), before_name.clone()),
                    _ => panic!("incoming outgoing error"),
                };

                let (side, leftnode, rightnode) = match course {
                    Some("left") => (Some(Side::Left), conn_node, legnode),
                    Some("right") => (Some(Side::Right), legnode, conn_node),
                    _ => (None, legnode, conn_node),
                };

                // let side = match (sw.1).attr("trackContinueCourse") {
                //     Some("left") => Some(Side::Left),
                //     Some("right") => Some(Side::Right),
                //     _ => side
                // };

                let sw_data = Switch {
                    trailnode: trailnode,
                    leftnode: leftnode,
                    rightnode: rightnode,
                    side: side,
                };
                sw_datas.push(sw_data);
            } else if (sw.1).name() == "crossing" {

            }

            sections.push(LinearSection {
                end_a: node,
                end_b: Connection::Connection(before_name.clone()),
                objects: objects.iter()
                    .cloned()
                    .filter_map(|(p, n, d)| if p >= start && p < pos {
                        Some((p - start, n, d))
                    } else {
                        None
                    })
                    .collect(),
                length: pos - start,
            });

            // println!("  sw {:?} {:?} {:?} {:?} {:?}",
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
            objects: objects.iter()
                .cloned()
                .filter_map(|(p, n, d)| if p >= start {
                    Some((p - start, n, d))
                } else {
                    None
                })
                .collect(),
            length: track_length - start,
        });

        // for x in &sections { println!("  sec {:?}", x); }
        // for x in &sw_datas { println!("  sw {:?}", x); }
    }

    let mut conn_nodes1 = HashMap::new();
    let mut links = HashMap::new();
    for (i,s) in sections.iter().enumerate() {
        if let Connection::Connection(ref end_a) = s.end_a {
            let this = (i,SectionEnd::Begin);
            if  conn_nodes1.get(end_a).is_some() {
                let other_node = conn_nodes1.remove(end_a).unwrap();
                links.insert(other_node,vec![this]);
                links.insert(this,vec![other_node]);
            } else  {
                conn_nodes1.insert(end_a.clone(), this);
            }
        }
        if let Connection::Connection(ref end_b) = s.end_b {
            let this = (i,SectionEnd::End);
            if  conn_nodes1.get(end_b).is_some() {
                let other = conn_nodes1.remove(end_b).unwrap();
                links.insert(other,vec![this]);
                links.insert(this,vec![other]);
            } else  {
                conn_nodes1.insert(end_b.clone(), this);
            }
        }
    }
    for sw in &sw_datas {
        let n1 = conn_nodes1.remove(&sw.trailnode).unwrap();
        let n2 = conn_nodes1.remove(&sw.leftnode).unwrap();
        let n3 = conn_nodes1.remove(&sw.rightnode).unwrap();
        links.insert(n1,vec![n2,n3]);
        // Don't go from legs to trunk because many signals
        // would be sighted from the same track, which would cause
        // confusion for driver.
    }

    mk_sight(&mut sections, &links);

    // SOrt objects by pos
    for s in &mut sections {
        s.objects.sort_by(|a, b| (a.0).partial_cmp(&b.0).expect("Object position NaN"));
    }


    let mut model = Model {
        nodes: Vec::new(),
        edges: Vec::new(),
    };
    let mut continuations = Vec::new();
    let mut conn_nodes = HashMap::new();
    let mut i = 0;
    for s in &mut sections {
        // println!("node n{}a-n{}b -- {:?}", i, i, s.end_a);

        model.nodes.push(empty_node(&format!("n{}", i)));

        if let Connection::Connection(ref end_a) = s.end_a {
            if conn_nodes.get(end_a).is_some() {
                let other_node = conn_nodes.remove(end_a).unwrap();
                continuations.push((PartNodeIdx::from_node_part(i, NodePart::A), other_node));
            } else {
                conn_nodes.insert(end_a.clone(), PartNodeIdx::from_node_part(i, NodePart::A));
            }
        }
        if let Connection::Boundary(ref name) = s.end_a {
            // println!("boundary n{}a", i);
            let n = PartNodeIdx::from_node_part(i, NodePart::A);
            model.edges.push(Edge::Boundary(n));

            if let Some(ref name) = *name {
                model.nodes[n.node_idx()].a.name = name.clone();
            }
        }

        let mut last_b = PartNodeIdx::from_node_part(i, NodePart::B);
        i += 1;

        let mut last_pos = 0.0;
        for &(ref obj_pos, ref obj_name, ref obj_data) in &s.objects {

            // println!("linear {}-n{}a {}", last_b, i, obj_pos - last_pos);
            model.edges.push(Edge::Linear(last_b,
                                          (PartNodeIdx::from_node_part(i, NodePart::A),
                                           obj_pos - last_pos)));

            let mut node = empty_node(&format!("n{}", i));

            match *obj_data {
                TracksideObject::Signal(Direction::Down, _) => 
                    node.a.objs.push(PartNodeObject::Signal(obj_name.to_string())),
                TracksideObject::Signal(Direction::Up, _) => 
                    node.b.objs.push(PartNodeObject::Signal(obj_name.to_string())),
                TracksideObject::Detector => node.both.push(TracksideObject::Detector),
                TracksideObject::Sight(Direction::Down, ref sig_name, l) => 
                    node.a.objs.push(PartNodeObject::Sight(sig_name.to_string(), l)),
                TracksideObject::Sight(Direction::Up, ref sig_name, l) => 
                    node.b.objs.push(PartNodeObject::Sight(sig_name.to_string(), l)),
            };

            model.nodes.push(node);
            // println!("node n{}a{}-n{}b{} -- {:?}",
            //         i,
            //         down_obj,
            //         i,
            //         up_obj,
            //         obj_name);

            last_b = PartNodeIdx::from_node_part(i, NodePart::B);
            last_pos = *obj_pos;
            i += 1;
        }

        // println!("linear {}-n{}a {}", last_b, i, s.length - last_pos);
        model.edges.push(Edge::Linear(last_b,
                                      (PartNodeIdx::from_node_part(i, NodePart::A),
                                       s.length - last_pos)));

        // println!("node n{}a-n{}b -- {:?}", i, i, s.end_b);
        model.nodes.push(empty_node(&format!("n{}", i)));


        if let Connection::Connection(ref end_b) = s.end_b {
            if conn_nodes.get(end_b).is_some() {
                let other_node = conn_nodes.remove(end_b).unwrap();
                continuations.push((PartNodeIdx::from_node_part(i, NodePart::B), other_node));
            } else {
                conn_nodes.insert(end_b.clone(), PartNodeIdx::from_node_part(i, NodePart::B));
            }
        }

        if let Connection::Boundary(ref name) = s.end_b {
            let n = PartNodeIdx::from_node_part(i, NodePart::B);
            model.edges.push(Edge::Boundary(n));

            if let Some(ref name) = *name {
                model.nodes[n.node_idx()].b.name = name.clone();
            }
        }

        i += 1;
    }

    let mut i = 1;
    for sw in &mut sw_datas {
        // println!("KEYS {:?}", sw);
        let n1 = conn_nodes.remove(&sw.trailnode).unwrap();
        let n2 = conn_nodes.remove(&sw.leftnode).unwrap();
        let n3 = conn_nodes.remove(&sw.rightnode).unwrap();
        // println!("switch sw{} {} {}-({} 0.0, {} 0.0)", i, side, n1, n2, n3);
        let name = format!("sw{}", i);
        model.edges.push(Edge::Switch(name, sw.side, n1, (n2, 0.0), (n3, 0.0)));
        i += 1;
    }

    // for (k, v) in conn_nodes.into_iter() {
    //    // TODO stderr
    //    // println!("Error: connection missing {:?}", (k,v));
    //

    for (a, b) in continuations.into_iter() {
        // println!("linear {}-{} 0.0", a, b);
        model.edges.push(Edge::Linear(a, (b, 0.0)));
    }

    Ok(model)
}
