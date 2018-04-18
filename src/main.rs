extern crate minidom;
extern crate clap;
extern crate petgraph;

use std::fs::File;
use std::io::prelude::*;
use clap::{Arg, App};
use std::path;

use std::collections::HashMap;
use std::collections::HashSet;

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
    let verbose = opts.occurrences_of("v") > 0;

    match run(path::Path::new(input_fn), verbose) {
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

fn run(input_fn: &path::Path, verbose: bool) -> Result<(), String> {
    // 1. read xml
    let (doc, ns) = get_xml(input_fn, verbose)?;
    // 2. find infrastructur
    let infrastructure = get_infrastructure_element(&doc, &ns)?;
    // 3. convert infrastructure to dgraph
    let mut dgraph = convert_infrastructure(infrastructure, &ns, verbose)?;
    // 4. find detection sections
    create_sections_from_detectors(&mut dgraph);
    // 7. calculate sight nodes
    // 5. Join zero-length edges
    // 6. Join small edges within tolerance? (Signal and detector in same node?)
    // join_nodes(&mut dgraph, 1.0); // 1m tolerance
    // 8. (create routes)
    let routes = find_routes(&dgraph);
    // 9. output infrastructure rolling dgraph format
    print_rolling(&dgraph);
    print_routes(&dgraph, &routes);

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
    section: Option<String>,
    sections: Vec<(String, f64)>,
    switches: Vec<(String, Side)>,
    length: f64,
}

#[derive(Debug, Clone)]
pub enum DirEdge {
    Linear(Link),
    FacingSwitch(String, Link, Link),
    TrailingSwitch(String, Side, Link),
    Boundary,
}

pub fn find_routes(model: &Model) -> Vec<Route> {

    let section_tolerance = 3.0;

    let mut routes = Vec::new();

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

    println!("DIR EDGES {:?}", dir_edges);

    let boundary_nodes = model.edges.iter().filter_map(|x| if let Edge::Boundary(n) = *x {
        Some(n)
    } else {
        None
    });

    for boundary in boundary_nodes {
        println!("Boundary start {:?}", boundary);
        let mut entry_stack = Vec::new();
        let mut entry_visited = HashSet::new();
        entry_stack.push(RouteEntry {
            node: boundary.opposite(),
            entry: RouteBoundary::ModelBoundary(boundary),
            section: None,
        });
        entry_visited.insert(boundary.opposite());

        while entry_stack.len() > 0 {
            let entry = entry_stack.pop().unwrap();
            let mut search_stack = Vec::new();

            println!("Entry from {:?}", entry);

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
                sections: entry.section.iter().map(|x| (x.clone(), 0.0)).collect(),
                section: entry.section,
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
                                    routes.push(Route {
                                        entry: entry.entry.clone(),
                                        exit: RouteBoundary::Signal(x.clone()),
                                        sections: curr_state.sections
                                            .iter()
                                            .filter_map(|&(ref x, d)| if d > section_tolerance {
                                                Some(x.clone())
                                            } else {
                                                None
                                            })
                                            .collect(),
                                        switches: curr_state.switches.clone(),
                                        length: curr_state.length,
                                    });

                                    if entry_visited.insert(curr_state.node) {
                                        entry_stack.push(RouteEntry {
                                            node: curr_state.node,
                                            entry: RouteBoundary::Signal(x.clone()),
                                            section: curr_state.section.clone(),
                                        });
                                    }

                                    is_exit = true;
                                    break; // Don't count TVDEnter on same node
                                }
                                TVDEnter(ref x) => {
                                    curr_state.section = Some(x.clone());
                                    curr_state.sections.push((x.clone(), 0.0));
                                }
                                TVDExit(ref x) => {
                                    if curr_state.section == Some(x.clone()) {
                                        curr_state.section = None;
                                    }
                                }
                            }
                        }
                    }

                    match (is_exit, dir_edges.get(&curr_state.node).cloned()) {
                        (false, Some(DirEdge::Linear((other, d)))) => {
                            curr_state.node = other.opposite();
                            curr_state.length += d;
                            if let Some(&mut (ref s, ref mut l)) =
                                curr_state.sections.iter_mut().last() {
                                if Some(s) == curr_state.section.as_ref() {
                                    *l += d;
                                }
                            }
                        }
                        (false, Some(DirEdge::TrailingSwitch(sw, pos, (other, d)))) => {
                            curr_state.node = other.opposite();
                            curr_state.length += d;
                            curr_state.switches.push((sw, pos));
                            if let Some(&mut (ref s, ref mut l)) =
                                curr_state.sections.iter_mut().last() {
                                if Some(s) == curr_state.section.as_ref() {
                                    *l += d;
                                }
                            }
                            if !switches_path_visited.insert(curr_state.switches.clone()) {
                                break;
                            }
                        }
                        (false, Some(DirEdge::FacingSwitch(sw, (other1, d1), (other2, d2)))) => {
                            let mut right_state = curr_state.clone();

                            curr_state.node = other1.opposite();
                            curr_state.length += d1;
                            if let Some(&mut (ref s, ref mut l)) =
                                curr_state.sections.iter_mut().last() {
                                if Some(s) == curr_state.section.as_ref() {
                                    *l += d1;
                                }
                            }
                            curr_state.switches.push((sw.clone(), Side::Left));

                            right_state.node = other2.opposite();
                            right_state.length += d2;
                            if let Some(&mut (ref s, ref mut l)) =
                                right_state.sections.iter_mut().last() {
                                if Some(s) == right_state.section.as_ref() {
                                    *l += d2;
                                }
                            }
                            right_state.switches.push((sw, Side::Right));

                            if switches_path_visited.insert(right_state.switches.clone()) {
                                search_stack.push(right_state);
                            }
                            if !switches_path_visited.insert(curr_state.switches.clone()) {
                                break;
                            }
                        }
                        (false, Some(DirEdge::Boundary)) => {
                            routes.push(Route {
                                entry: entry.entry.clone(),
                                exit: RouteBoundary::ModelBoundary(curr_state.node),
                                sections: curr_state.sections
                                    .iter()
                                    .filter_map(|&(ref x, d)| if d > section_tolerance {
                                        Some(x.clone())
                                    } else {
                                        None
                                    })
                                    .collect(),
                                switches: curr_state.switches.clone(),
                                length: curr_state.length,
                            });
                            break;
                        }
                        _ => break,
                    }
                }
            }
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

pub fn print_resources(_model: &Model, route: &Route) {
    println!("  sections [{}]", route.sections.join(", "));
    println!("  switches [{}]",
             route.switches
                 .iter()
                 .map(|&(ref sw, pos)| format!("{} {}", sw, pos.as_str()))
                 .collect::<Vec<_>>()
                 .join(", "));
    println!("  contains []");
}

pub fn print_routes(model: &Model, routes: &Vec<Route>) {
    let mut i = 1;
    for r in routes {
        use RouteBoundary::*;
        match (&r.entry, &r.exit) {
            (&ModelBoundary(ref b), &Signal(ref s)) => {
                println!("modelentry r{} from {} {{",
                         i,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name);
                println!("  exit {}", s);
                println!("  length {}", r.length);
                print_resources(model, r);
                println!("}}");
            }
            (&Signal(ref s), &ModelBoundary(ref b)) => {
                println!("modelexit r{} to {} {{",
                         i,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name);
                println!("  entry {}", s);
                if let Some(s) = r.sections.get(0) {
                    println!("  entrysection {}", s);
                }
                println!("  length {}", r.length);
                print_resources(model, r);
                println!("}}");
            }
            (&Signal(ref s1), &Signal(ref s2)) => {
                println!("route r{} {{", i);
                println!("  entry {}", s1);
                println!("  length {}", r.length);
                if let Some(s) = r.sections.get(0) {
                    println!("  entrysection {}", s);
                }
                println!("  exit {}", s2);
                print_resources(model, r);
                println!("}}");
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
}

pub fn print_rolling(model: &Model) {
    for n in &model.nodes {
        println!("node {}{}-{}{}",
                 n.a.name,
                 repr_partnode(&n.a.objs),
                 n.b.name,
                 repr_partnode(&n.b.objs));
    }
    for e in &model.edges {
        match *e {
            Edge::Linear(a, (b, d)) => {
                println!("linear {}-{} {}",
                         model.nodes[a.node_idx()].get_part(a.node_part()).name,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name,
                         d)
            }
            Edge::Switch(ref name, side, n1, (n2, d2), (n3, d3)) => {
                println!("switch {} {} {}-({} {}, {} {})",
                         name,
                         side.as_str(),
                         model.nodes[n1.node_idx()].get_part(n1.node_part()).name,
                         model.nodes[n2.node_idx()].get_part(n2.node_part()).name,
                         d2,
                         model.nodes[n3.node_idx()].get_part(n3.node_part()).name,
                         d3)
            }
            Edge::Boundary(n) => {
                println!("boundary {}",
                         model.nodes[n.node_idx()].get_part(n.node_part()).name)
            }

        }
    }
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
    Switch(String, Side, PartNodeIdx, Link, Link),
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

#[derive(Copy, Clone, Debug)]
enum Direction {
    Up,
    Down,
}


#[derive(Debug)]
pub struct Switch {
    trailnode: String,
    leftnode: String,
    rightnode: String,
    side: Side,
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

    if let Some(_) = node.get_child("openEnd", ns) {
        return Connection::Boundary;
    }

    // TODO stderr
    // println!("Warning: track begin/end node has no connection,
    // bufferStop or openEnd element. Defaulting to bufferStop.");
    return Connection::Stop;
}


#[derive(Clone, Debug)]
enum TracksideObject {
    Signal(Direction),
    Detector,
}

#[derive(Clone, Debug)]
pub enum PartNodeObject {
    Signal(String),
    TVDEnter(String),
    TVDExit(String),
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
            let id = s.attr("id").unwrap();
            let _name = s.attr("name").unwrap();
            let name = id;
            let pos = s.attr("pos").unwrap().parse::<f64>().unwrap();
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
                Some((pos, name.to_string(), TracksideObject::Signal(dir)))
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
            let id = d.attr("id").unwrap();
            let _name = d.attr("name").unwrap();
            let name = id;
            let pos = d.attr("pos").unwrap().parse::<f64>().unwrap();

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
    let mut model = Model {
        nodes: Vec::new(),
        edges: Vec::new(),
    };

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
        // println!("OBJECTs on track: {:?}", objects);
        switches.sort_by(|a, b| (a.0).partial_cmp(&b.0).expect("Switch position NaN"));

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

        for (i, sw) in switches.iter().enumerate() {
            let pos = (sw.1).attr("pos").unwrap().parse::<f64>().unwrap();

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

            let conn_ref = conn.attr("ref").unwrap().to_string();
            let conn_id = conn.attr("id").unwrap().to_string();
            let orientation = conn.attr("orientation").unwrap();
            let course = conn.attr("course").unwrap();

            let before_name = format!("spv-{}-{}-down", name, i);
            let after_name = format!("spv-{}-{}-up", name, i);

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

            let (conn_node, trailnode, legnode) = match orientation {
                "outgoing" => (conn_ref.clone(), before_name.clone(), after_name.clone()),
                "incoming" => (conn_id.clone(), after_name.clone(), before_name.clone()),
                _ => panic!("incoming outgoing error"),
            };

            let (side, leftnode, rightnode) = match course {
                "left" => (Side::Left, conn_node, legnode),
                "right" => (Side::Right, legnode, conn_node),
                _ => panic!("left right error"),
            };

            let sw_data = Switch {
                trailnode: trailnode,
                leftnode: leftnode,
                rightnode: rightnode,
                side: side,
            };
            sw_datas.push(sw_data);

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
        if let Connection::Boundary = s.end_a {
            // println!("boundary n{}a", i);
            model.edges.push(Edge::Boundary(PartNodeIdx::from_node_part(i, NodePart::A)));
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
                TracksideObject::Signal(Direction::Down) => {
                    node.a.objs.push(PartNodeObject::Signal(obj_name.to_string()))
                }
                TracksideObject::Signal(Direction::Up) => {
                    node.b.objs.push(PartNodeObject::Signal(obj_name.to_string()))
                }
                TracksideObject::Detector => node.both.push(TracksideObject::Detector),
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

        if let Connection::Boundary = s.end_b {
            model.edges.push(Edge::Boundary(PartNodeIdx::from_node_part(i, NodePart::B)));
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
