use std::collections::{HashMap, HashSet};
use base::*;
use dgraph::*;

// TODO 
// don't allow switches in two different positions? Happens on the balloon loop
// with entry signal only.
//

#[derive(Debug, Clone)]
pub enum RouteBoundary {
    ModelBoundary(PartNodeIdx),
    Signal(String),
}

#[derive(Debug, Clone)]
pub struct Route {
    pub entry: RouteBoundary,
    pub exit: RouteBoundary,
    pub sections: Vec<String>,
    pub switches: Vec<(String, Side)>,
    pub releases: Vec<(String, f64, Vec<String>)>,
    pub length: f64,
}

#[derive(Debug, Clone)]
pub struct RouteEntry {
    pub node: PartNodeIdx,
    pub entry: RouteBoundary,
    pub section: Option<String>,
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

fn convert_model(model  :&DGraphModel) -> HashMap<PartNodeIdx, DirEdge> {
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

pub fn find_routes(model: &DGraphModel) -> Vec<Route> {
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
                            use dgraph::PartNodeObject::*;
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
                                println!("Warning: Could not convert route");
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

