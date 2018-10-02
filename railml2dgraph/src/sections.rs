use std::collections::HashMap;
use petgraph;
use dgraph::*;

pub fn create_sections_from_detectors(m: &mut DGraphModel) {

    let num_nodes = m.nodes.len() * 2 + 1;
    let is_boundary_idx = 0;
    let mut sets = petgraph::unionfind::UnionFind::new(num_nodes);

    for (node_idx, node) in m.nodes.iter().enumerate() {

        // Zero is the special boundary marker
        // 2n+1 is the A part, and 2n+2 is the B part of the node.
        let (a_idx, b_idx) = (2 * node_idx + 1, 2 * node_idx + 2);

        if node.has_detector {
            // println!("Detector at {:?}", node);
        } else {
            sets.union(a_idx, b_idx);
        }
    }

    for edge in &m.edges {
        use dgraph::Edge::*;
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

        if node.has_detector {

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




