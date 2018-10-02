use std::collections::HashMap;
use base::*;
use branching::*;

#[derive(Debug,Clone)]
pub struct DGraphModel {
    pub nodes: Vec<DGraphNode>,
    pub edges: Vec<Edge>,
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
    pub name: Option<String>,
    pub a: PartNode,
    pub b: PartNode,
    pub has_detector: bool,
}

impl DGraphNode {
    pub fn get_part(&self, p: NodePart) -> &PartNode {
        match p {
            NodePart::A => &self.a,
            NodePart::B => &self.b,
        }
    }
}

#[derive(Debug,Clone)]
pub struct PartNode {
    pub name: String,
    pub objs: Vec<PartNodeObject>,
}

#[derive(Clone, Debug)]
pub enum PartNodeObject {
    Signal(String),
    TVDEnter(String),
    TVDExit(String),
    Sight(String,f64),
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


fn empty_node(name :&str) -> DGraphNode {
    DGraphNode {
        name: None,
        a: PartNode {
            name: format!("{}a", name),
            objs: vec![],
        },
        b: PartNode {
            name: format!("{}b", name),
            objs: vec![],
        },
		has_detector: false,
    }
}


fn new_node(ns :&mut Vec<DGraphNode>) -> (PartNodeIdx, PartNodeIdx) {
    let i = ns.len();
    let name = format!("n{}",i);
    ns.push(empty_node(&name));
    PartNodeIdx::from_node(i)
}

pub fn convert(bm :BranchingModel) -> Result<DGraphModel, String> {
    let mut model = DGraphModel {
        nodes: Vec::new(),
        edges: Vec::new(),
    };

    let mut named_connections = HashMap::new();
    for mut t in bm.tracks {
        let (mut na, mut nb) = new_node(&mut model.nodes);
        match t.begin {
            BrTrackEnd::Stop => {},
            BrTrackEnd::Boundary(_name) => { model.edges.push(Edge::Boundary(na)); },
            BrTrackEnd::Connection((a,b)) => { named_connections.insert(a,(b,na)); },
        };

        let mut last_node = nb;
        let mut last_pos = 0.0;

        t.objs.sort_by(|a, b| (a.pos).partial_cmp(&b.pos).unwrap());
        for obj in t.objs {
            let (mut na, mut nb) = new_node(&mut model.nodes);
            use branching::BrObjectData::*;

            match obj.data {
                Switch { dir, side, conn: (cid,cref) } => {
                    let (na_branch, nb_branch) = new_node(&mut model.nodes);
                    let (na_straight, nb_straight) = new_node(&mut model.nodes);

                    model.edges.push(Edge::Switch(obj.name, Some(side),
                        if dir == Dir::Up { nb } else { na },
                        if side == Side::Left { (na_branch, 0.0) } else { (na_straight, 0.0 ) },
                        if side == Side::Right { (na_branch, 0.0) } else { (na_straight, 0.0 ) }));

                    named_connections.insert(cid, (cref, nb_branch));

                    if dir == Dir::Up { nb = nb_straight; }
                    else { na = nb_straight; }
                },
                Detector => {
                    model.nodes[na.node_idx()].has_detector = true;
                },
                Signal { dir, .. } => {
                    if dir == Dir::Up {
                        model.nodes[nb.node_idx()].b.objs.push(PartNodeObject::Signal(obj.name));
                    }  else {
                        model.nodes[na.node_idx()].a.objs.push(PartNodeObject::Signal(obj.name));
                    }
                },
                Sight { dir, signal, distance } => {
                    if dir == Dir::Up {
                        model.nodes[nb.node_idx()].b.objs.push(PartNodeObject::Sight(signal, distance));
                    }  else {
                        model.nodes[na.node_idx()].a.objs.push(PartNodeObject::Sight(signal, distance));
                    }
                },
            }

            model.edges.push(Edge::Linear(last_node, (na, obj.pos - last_pos)));
            last_pos = obj.pos;
            last_node = nb;
        }

        // Last node
        let (mut na, mut nb) = new_node(&mut model.nodes);
        model.edges.push(Edge::Linear(last_node, (na, t.length - last_pos)));
        match t.end {
            BrTrackEnd::Stop => {},
            BrTrackEnd::Boundary(_name) => { model.edges.push(Edge::Boundary(nb)); },
            BrTrackEnd::Connection((a,b)) => { named_connections.insert(a,(b,nb)); },
        };
    }

    // Resolve named connections
    while !named_connections.is_empty() {
        let id1 = named_connections.keys().next().unwrap().clone();
        let (ref1, node_a) = named_connections.remove(&id1).unwrap();
        let (ref2, node_b) = named_connections.remove(&ref1).unwrap(); // TODO err msg
        if ref2 != id1 { panic!("Inconsistent connections."); }
        model.edges.push(Edge::Linear(node_a, (node_b, 0.0)));
    }

    Ok(model)
}
