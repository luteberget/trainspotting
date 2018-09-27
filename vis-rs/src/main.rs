extern crate z3;

type EdgeRef = usize;
type NodeRef = usize;
#[derive(Copy,Clone,Debug)]
pub enum Side { Left, Right}
#[derive(Copy,Clone,Debug)]
pub enum Dir { Outgoing, Incoming }

pub enum NodeShape {
    Begin(EdgeRef), // index of edge
    End(EdgeRef),   // index of edge
    Switch {
        side: Side,
        dir :Dir,
        left :EdgeRef,
        right :EdgeRef,
        trunk :EdgeRef,
    }
}

pub struct Dirs<'c> {
    is_up : z3::Ast<'c>,
    is_straight : z3::Ast<'c>,
    is_down : z3::Ast<'c>,
}
impl<'c> Dirs<'c> {
    pub fn new(ctx: &'c z3::Context) -> Dirs<'c> {
        Dirs {
            is_up: ctx.from_bool(false),
            is_down: ctx.from_bool(false),
            is_straight: ctx.from_bool(false),
        }
    }
}

pub struct Node { 
    name :String,
    pos: f64,
    shape :NodeShape,
}

pub struct PortRef {
    node: usize,
    port: Port,
}

pub fn port(node: usize, port: Port) -> PortRef { PortRef { node, port } }

pub enum Port { Out, In, Left, Right, Trunk }

pub struct Edge {
    a : PortRef, // index of node
    b : PortRef, // index of node
}

//  node->edge lookup
//enum NodeEdgeRef {
//    A(usize),
//    B(usize),
//}

//type NodeLookupEdge = [(usize, 

// Solver-internal representation of node
struct NodeRepr<'ctx> {
    x : z3::Ast<'ctx>,
    y : z3::Ast<'ctx>,
}

// Solver-internal representation of edge
struct EdgeRepr<'ctx> {
    y         : z3::Ast<'ctx>,
    // TODO only y is really needed? 
    //       dy1dy2 are only definitions used in the for-loop over edges
    //       shortup,shortdown are variables, but only used in the for-loop over edges
    //       however, y needs to be stored to index from the less_than relation.
    dy1       : z3::Ast<'ctx>,
    dy2       : z3::Ast<'ctx>,
    shortup   : z3::Ast<'ctx>,
    shortdown : z3::Ast<'ctx>,
}

type EdgePair = (EdgeRef, EdgeRef);
fn less_than(nodes :&[Node], edges :&[Edge]) -> Vec<EdgePair> {
    use std::collections::HashSet;
    use std::collections::BinaryHeap;
    let mut lt : HashSet<(EdgeRef,EdgeRef)> = HashSet::new();

    let lr = |n:NodeRef,dir| {
        // BinaryHeap is max-heap, so negate for increasing  and unit for decreasing mileage
        let dirfactor = match dir { Dir::Outgoing => -1, Dir::Incoming => 1 };

        let top_edge = |n:NodeRef| match (dir,&nodes[n].shape) {
            (Dir::Outgoing, NodeShape::Switch { dir: Dir::Outgoing, left, .. }) => left,
            (Dir::Incoming, NodeShape::Switch { dir: Dir::Incoming, right, .. }) => right,
            _ => panic!(),
        };

        let bottom_edge = |n:NodeRef| match (dir, &nodes[n].shape) {
            (Dir::Outgoing, NodeShape::Switch { dir: Dir::Outgoing, right, .. }) => right,
            (Dir::Incoming, NodeShape::Switch { dir: Dir::Incoming, left, .. }) => left,
            _ => panic!(),
        };

        let edge_node = |e:EdgeRef| match dir {
            Dir::Outgoing => edges[e].b.node,
            Dir::Incoming => edges[e].a.node,
        };

        let next_edges = |n:NodeRef| match (dir, &nodes[n].shape) {
            // TODO get rid of allocation with smallvec? probably not worth much.
            (Dir::Outgoing, NodeShape::End(_)) => vec![],
            (Dir::Outgoing, NodeShape::Switch { dir: Dir::Outgoing, left, right, .. }) => vec![left, right],
            (Dir::Outgoing, NodeShape::Switch { dir: Dir::Incoming, trunk, .. }) => vec![trunk],
            (Dir::Incoming, NodeShape::Begin(_)) => vec![],
            (Dir::Incoming, NodeShape::Switch { dir: Dir::Incoming, left, right, .. }) => vec![left, right],
            (Dir::Incoming, NodeShape::Switch { dir: Dir::Outgoing, trunk, .. }) => vec![trunk],
             _ => panic!(),
        };

        let mut over_edges = HashSet::new();
        let mut over_nodes = vec![edge_node(*top_edge(n))].into_iter().collect::<HashSet<usize>>();
        let mut over_queue :BinaryHeap<(isize,usize)> = BinaryHeap::new();
        over_queue.push((dirfactor*edge_node(*top_edge(n)) as isize, *top_edge(n)));

        let mut under_edges = HashSet::new();
        let mut under_nodes = vec![edge_node(*bottom_edge(n))].into_iter().collect::<HashSet<usize>>();
        let mut under_queue :BinaryHeap<(isize,usize)>  = BinaryHeap::new();
        under_queue.push((dirfactor*edge_node(*bottom_edge(n)) as isize, *bottom_edge(n)));

        loop {
            match (over_queue.peek().cloned(), under_queue.peek().cloned()) {
                (Some((over_priority,_)), Some((under_priority,_))) => {
                    if over_priority > under_priority {
                        let (_,edge) = over_queue.pop().unwrap();
                        over_edges.insert(edge);
                        for edge_i in next_edges(edge_node(edge)) {
                            over_nodes.insert(edge_node(*edge_i));
                            over_queue.push((dirfactor*edge_node(*edge_i) as isize, *edge_i));
                        }
                    } else {
                        let (_,edge) = under_queue.pop().unwrap();
                        under_edges.insert(edge);
                        for edge_i in next_edges(edge_node(edge)) {
                            over_nodes.insert(edge_node(*edge_i));
                            over_queue.push((dirfactor*edge_node(*edge_i) as isize, *edge_i));
                        }
                    }

                    // If the over and under paths have joined, we are done
                    if !over_nodes.intersection(&under_nodes).next().is_some() {
                        break;
                    }
                }
                _ => break
            }
        }

        // left over edges in the queue are also included
        over_edges.extend(over_queue.into_iter().map(|(_,e)| e));
        under_edges.extend(under_queue.into_iter().map(|(_,e)| e));

        (over_edges.into_iter().collect::<Vec<_>>(), 
         under_edges.into_iter().collect::<Vec<_>>())
    };

    for (i,n) in nodes.iter().enumerate() {
        match &n.shape {
            NodeShape::Switch { dir, side, .. } => {
                let (l,r) = lr(i,*dir);
                for e_l in &l {
                    for e_r in &r {
                        lt.insert((*e_r,*e_l));
                    }
                }
            },
            _ => {},
        }
    }

    lt.into_iter().collect()
}

fn main() {
    let mut conf = z3::Config::new();
    conf.set_param_value("trace","true");
    let ctx = z3::Context::new(&conf);
    let opt = z3::Optimize::new(&ctx);

    let nodes = vec![
        Node { name: "n1".to_string(), shape: NodeShape::Begin(0), pos: 100.0 },
        Node { name: "sw".to_string(), shape: NodeShape::Switch{
            side: Side::Left, dir: Dir::Outgoing, 
            trunk: 0, left: 1, right: 2,
        }, pos: 200.0 },
        Node { name: "e1".to_string(), shape: NodeShape::End(1), pos: 300.0 },
        Node { name: "e2".to_string(), shape: NodeShape::End(2), pos: 400.0 },
    ];
    let edges = vec![
        Edge { a: port(0, Port::Out),   b: port(1, Port::Trunk) },
        Edge { a: port(1, Port::Left),  b: port(2, Port::In) },
        Edge { a: port(1, Port::Right), b: port(3, Port::In) },
    ];

    let edges_lt = less_than(&nodes, &edges);
    println!("LESS than relation: {:?}", edges_lt);

    let zero_int = ctx.from_i64(0);
    let zero_real = ctx.from_real(0,1); // 0.0
    let one_real  = ctx.from_real(1,1); // 1.0
    let two_real  = ctx.from_real(2,1); // 1.0
    let negone_real  = ctx.from_real(-1,1); // 1.0
    let true_bool = || ctx.from_bool(true);
    let false_bool = || ctx.from_bool(false);

    let mut node_data = Vec::new();
    for n in &nodes {
        let x = ctx.fresh_real_const("nx");
        opt.assert(&x.ge(&zero_real));
        let y = ctx.fresh_int_const("ny");
        opt.assert(&y.ge(&zero_int));

        node_data.push(NodeRepr { x, y });
    }

    let mut edge_data = Vec::new();
    for e in &edges {
        let y = ctx.fresh_int_const("ey");
        opt.assert(&y.ge(&zero_int));

        let na_y = &node_data[e.a.node].y;
        let nb_y = &node_data[e.b.node].y;
        let dy1 = y.sub(&[na_y]);
        let dy2 = nb_y.sub(&[&y]);

        let shortup = ctx.fresh_bool_const("e_bothup");
        let shortdown = ctx.fresh_bool_const("e_bothdown");
        opt.assert(&shortup.and(&[&shortdown]).not());

        edge_data.push(EdgeRepr {y, dy1, dy2, shortup, shortdown});
    }

    // Edge space node X
    for (i,e) in edges.iter().enumerate() {
        let node_a_x = &node_data[e.a.node].x;
        let node_b_x = &node_data[e.b.node].x;

        let dist = &one_real;
        opt.assert(&node_a_x.add(&[dist]).le(node_b_x));
    }

    // Node ordering X
    for ((i,na),nb) in node_data.iter().enumerate().zip(node_data.iter().skip(1)) {
        opt.assert(&na.x.le(&nb.x));
        // KM / MILEAGE DIFF
        //let posdiff = (nodes[i+1].pos - nodes[i].pos) as i32;
        //opt.assert(&na.x.add(&[&ctx.from_real(posdiff,1)]).le(&nb.x));
    }

    // Node shape
    let mut node_slanted :Vec<z3::Ast> = Vec::new();
    for (i,n) in nodes.iter().enumerate() {
        match n.shape {
            NodeShape::Switch {.. } => node_slanted.push(ctx.fresh_bool_const("slanted")),
            _ => node_slanted.push(false_bool()),
        }
    }

    let mut absys = Vec::new();
    for (i,e) in edges.iter().enumerate() {
        let node_a = &nodes[e.a.node];
        let node_b = &nodes[e.b.node];

        let (edge_out, edge_in, absdy1factor, absdy2factor) = {
            use NodeShape::*;
            use Port::*;
            let a_slanted = node_slanted[e.a.node].and(&[&true_bool()]);
            let a_straight = a_slanted.not();
            let b_slanted = node_slanted[e.b.node].and(&[&true_bool()]);
            let b_straight = a_slanted.not();

            let absdy1factor = match (&node_a.shape, &e.a.port) {
                (Switch { dir: Dir::Outgoing, ..  },                   Port::Right) => -1,
                (Switch { side: Side::Left,  dir: Dir::Incoming, .. }, Port::Trunk) => -1,
                _ => 1,
            };
            let absdy2factor = match (&node_b.shape, &e.b.port) {
                (Switch { dir: Dir::Incoming, ..  },                   Port::Right) => -1,
                (Switch { side: Side::Left,  dir: Dir::Outgoing, .. }, Port::Trunk) => -1,
                _ => 1,
            };

            let edge_out = match (&node_a.shape, &e.a.port) {

                (Begin(_), Port::Out) => 
                    Dirs { is_straight: true_bool(), .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Outgoing, .. }, Port::Left) => 
                    Dirs { is_up: a_straight, is_straight: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Outgoing, .. }, Port::Right) => 
                    Dirs { is_straight: a_straight, is_down: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Outgoing, .. }, Port::Right) => 
                    Dirs { is_down: a_straight, is_straight: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Outgoing, .. }, Port::Left) => 
                    Dirs { is_straight: a_straight, is_up: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Incoming, .. }, Port::Trunk) => 
                    Dirs { is_straight: a_straight, is_down: a_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Incoming, .. }, Port::Trunk) => 
                    Dirs { is_straight: a_straight, is_up: a_slanted, .. Dirs::new(&ctx) },

                _ => panic!("Bad combination"),
            };

            let edge_in = match (&node_b.shape, &e.b.port) {
                (End(_), In) => 
                    Dirs { is_straight: true_bool(), .. Dirs::new(&ctx) },

                    // Incoming
                (Switch { side: Side::Left, dir: Dir::Incoming, ..}, Port::Left) =>
                    Dirs { is_up: b_straight, is_straight: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Incoming, ..}, Port::Right) =>
                    Dirs { is_straight: b_straight, is_down: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Incoming, ..}, Port::Right) =>
                    Dirs { is_down: b_straight, is_straight: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Right, dir: Dir::Incoming, ..}, Port::Left) =>
                    Dirs { is_straight: b_straight, is_up: b_slanted, .. Dirs::new(&ctx) },

                    // Outgoing
                (Switch { side: Side::Right, dir: Dir::Outgoing, .. }, Port::Trunk) => 
                    Dirs { is_straight: b_straight, is_up: b_slanted, .. Dirs::new(&ctx) },
                (Switch { side: Side::Left, dir: Dir::Outgoing, .. }, Port::Trunk) => 
                    Dirs { is_straight: b_straight, is_down: b_slanted, .. Dirs::new(&ctx) },

                _ => panic!("Bad combo"),
            };


            (edge_out,edge_in, absdy1factor, absdy2factor)
        };

        let shortup = &edge_data[i].shortup;
        let shortdown = &edge_data[i].shortdown;

        opt.assert(&shortup.implies(&edge_out.is_up.and(&[ &edge_in .is_up])));
        opt.assert(&shortdown.implies(&edge_out.is_down.and(&[ &edge_in .is_down])));

        // if end is straihgt, dy = 0
        opt.assert(&edge_out.  is_straight.implies(&edge_data[i].dy1._eq(&zero_real)));
        opt.assert(&edge_in.  is_straight.implies(&edge_data[i].dy2._eq(&zero_real)));

        // if end is down, dy <= 0.0, it must also be <= -1.0, see below
        opt.assert(&edge_out.  is_down.implies(&edge_data[i].dy1.le(&zero_real)));
        opt.assert(&edge_in.  is_down.implies(&edge_data[i].dy2.le(&zero_real)));
        // Both down -> dy1+dy2 < -1
        opt.assert(&shortdown.implies(&edge_data[i].dy1.add(&[&edge_data[i].dy2]).le(&negone_real)));
        // Leftmost end down and not both -> dy1 < -1
        opt.assert(&edge_out.is_down.and(&[&shortdown.not()]).implies(&edge_data[i].dy1.le(&negone_real)));
        // Rightmost end down and not both -> dy2 < -1
        opt.assert(&edge_in .is_down.and(&[&shortdown.not()]).implies(&edge_data[i].dy2.le(&negone_real)));


        // if end is up, dy >= 0.0, it must also be >= 1.0, see below
        opt.assert(&edge_out.  is_up.implies(&edge_data[i].dy1.ge(&zero_real)));
        opt.assert(&edge_in.  is_up.implies(&edge_data[i].dy2.ge(&zero_real)));
        // Both up -> dy1+dy2 > 1
        opt.assert(&shortup.implies(&edge_data[i].dy1.add(&[&edge_data[i].dy2]).ge(&one_real)));
        // Left most up and not both -> dy1 > 1
        opt.assert(&edge_out.is_up.and(&[&shortup.not()]).implies(  &edge_data[i].dy1.ge(&one_real)));
        // Rightmost up and not both -> dy2 > 1
        opt.assert(&edge_in.is_up.and(&[&shortup.not()]).implies(  &edge_data[i].dy2.ge(&one_real)));


        // use sum of abs of dy1 and dy2 as min x-dist between nodes
        let x1 = &node_data[e.a.node].x;
        let x2 = &node_data[e.b.node].x;
        // TODO TODO 
        //let absdy1 = &zero_real; //edge_data[i].dy1
        //let absdy2 = &zero_real; //edge_data[i].dy1

        let absdy1 = edge_data[i].dy1.mul(&[&ctx.from_real(absdy1factor,1)]);
        let absdy2 = edge_data[i].dy2.mul(&[&ctx.from_real(absdy2factor,1)]);

        let absy = absdy1.add(&[&absdy2]);
        opt.assert(&x1.add(&[&absy]).le(x2));
        absys.push(absy);


        // X distnace is affected by short
        let is_short = shortdown.or(&[shortup]);
        opt.assert(&is_short.implies(&x1.add(&[&one_real]).ge(x2)));
        opt.assert(&is_short.not().implies(&x1.add(&[&two_real]).le(x2)));

    }

    // First minimize slant
    let absys_ref = absys.iter().collect::<Vec<_>>();
    opt.minimize(&zero_real.add(&absys_ref));

    // Then minimize size of drawing
    let node_xs = node_data.iter().map(|n| &n.x).collect::<Vec<&z3::Ast>>();
    let node_ys = node_data.iter().map(|n| &n.y).collect::<Vec<&z3::Ast>>();
    opt.minimize(&zero_real.add(&node_xs).add(&node_ys));

    // Then minimize height
    let edge_ys = edge_data.iter().map(|e| &e.y).collect::<Vec<&z3::Ast>>();
    opt.minimize(&zero_real.add(&edge_ys));


    let status = opt.check();
    println!("Status: {:?}", status);
    if !status { panic!("solver failed"); }
    let model = opt.get_model();

    for ((i,n),repr) in nodes.iter().enumerate().zip(node_data.iter()) {
        let x = model.eval(&repr.x).unwrap().as_real();
        let y = model.eval(&repr.y).unwrap().as_i64();
        println!("node {}: {:?} {:?}", i,x,y);
    }

    for ((i,n),repr) in edges.iter().enumerate().zip(edge_data.iter()) {
        let y = model.eval(&repr.y).unwrap().as_i64();
        println!("edge {}: {:?}", i,y);
    }

}
