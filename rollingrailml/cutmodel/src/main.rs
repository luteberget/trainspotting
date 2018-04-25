extern crate minidom;
extern crate clap;
extern crate petgraph;

use std::fs::File;
use std::io::prelude::*;
use clap::{Arg, App};
use std::path;
use std::collections::HashMap;
use std::collections::HashSet;
use petgraph::Undirected;

fn main() {
    let opts = App::new("railML model cut extractor")
        .about("Cut your model in pieces")
        .arg(Arg::with_name("INPUT")
             .help("railML file")
             .required(true)
             .index(1))
        .get_matches();

    let input_fn = opts.value_of("INPUT").map(|x| path::Path::new(x)).unwrap();

    match run(&input_fn) {
        Ok(()) => {},
        Err(e) => {
            println!("Failed: {}", e);
            std::process::exit(1);
        }
    }
}

fn run(input_fn :&path::Path) -> Result<(), String> {
	let except_connections = vec!["co23323", "co22226", "co22103", "co23444"];
	//let except_connections = vec![];
    let (doc,ns) = get_xml(input_fn, true)?;
    let infrastructure = get_infrastructure_element(&doc, &ns)?;
    let graph = build_graph(infrastructure, &ns, &except_connections)?;


    //for xx in 0..100  {
    //

    let traverse_from = "tr7";
    let tracks = connected_tracks_from(&graph, &traverse_from)?;
    println!("Keeping {}/{} tracks (from {}).", tracks.len(), graph.1.node_count(),traverse_from);

    //
    //}

    for x in tracks {
        println!("{:?}", x);
    }
    for y in graph.1.raw_nodes() {
        //println!(" NODE {:?}",y);
    }

    Ok(())
}

fn connected_tracks_from<'a>(&(ref tracks,ref graph) :&(HashMap<&'a str,petgraph::graph::NodeIndex>, petgraph::Graph<&'a str, (), Undirected>),
                                        from :&str) -> Result<HashSet<&'a str>, String> {

    let mut ts = HashSet::new();
    petgraph::visit::depth_first_search(&graph, Some(tracks[from]), |ev| {
        if let petgraph::visit::DfsEvent::Discover(v,_) = ev {
            ts.insert(*(graph.node_weight(v).unwrap()));
        }
        ()
    });
    Ok(ts)
}


fn get_track_connections<'a>(track :&'a minidom::Element, ns :&'a str) -> Result<Vec<(&'a str,&'a str)>,String> {
    let mut v = vec![];
    let topology = track.get_child("trackTopology", ns)
                    .ok_or("No trackTopology found.".to_string())?;

	let track_begin = topology.get_child("trackBegin", ns)
		.ok_or("No trackBegin found.".to_string())?;
    if let Some(conn) = track_begin.get_child("connection", ns) {
        v.push((conn.attr("id").unwrap(), conn.attr("ref").unwrap()));
	} else {
        if let Some(end) = track_begin.get_child("openEnd", ns) {
        } else {
        if let Some(end) = track_begin.get_child("bufferStop", ns) {
        } else {
        println!("NOT CONNECTION {:?}", track_begin);
        }
        }
    }

	let track_end = topology.get_child("trackEnd", ns)
		.ok_or("No trackBegin found.".to_string())?;
    if let Some(conn) = track_end.get_child("connection", ns) {
        v.push((conn.attr("id").unwrap(), conn.attr("ref").unwrap()));
	} else {
        if let Some(end) = track_end.get_child("openEnd", ns) {
        } else {
        if let Some(end) = track_end.get_child("bufferStop", ns) {
        } else {
        println!("NOT CONNECTION {:?}", track_end);
        }
        }
    }

    if let Some(c) = topology.get_child("connections", ns) {
        for e in c.children() {
            match e.name() {
                "switch" | "crossing" => {
                    for conn in e.children().filter(|x| x.name().to_lowercase() == "connection") {
                        v.push((conn.attr("id").unwrap(), conn.attr("ref").unwrap()));
                    }
                }
                _ => {
                    println!("Unknown connection type {:?}", e);
                }
            }
        }
    }

	Ok(v)

}

fn build_graph<'a>(inf :&'a minidom::Element, ns :&'a str, except_connections :&'a [&str]) -> Result<(HashMap<&'a str, petgraph::graph::NodeIndex>,petgraph::Graph<&'a str, (), Undirected>), String>{
    let mut graph = petgraph::Graph::new_undirected();
    let mut track_nodes = HashMap::new();
    let mut connections = HashMap::new();

    let tracks = inf.children().filter(|x| x.name().to_lowercase() == "tracks")
        .nth(0).ok_or("No tracks in infrastructure".to_string())?;

    for track in tracks.children().filter(|x| x.name().to_lowercase() == "track") {
        //let name = track.attr(name) .ok_or("No name for track".to_string());
        let tid = track.attr("id") .ok_or("No id for track".to_string())?;
        let cs = get_track_connections(track, ns)?;

        //println!("Track  {:?} has connections: ", tid);
        //for x in &cs { println!("   - {:?}", x); }
        for (cid,_ref) in cs.into_iter() {
            connections.insert(cid,tid);
        }
    }

    for track in tracks.children().filter(|x| x.name().to_lowercase() == "track") {
        //let name = track.attr(name) .ok_or("No name for track".to_string());
        let tid = track.attr("id") .ok_or("No id for track".to_string())?;
        let cs = get_track_connections(track, ns)?;
        for (cid,ref_) in cs.into_iter() {
            if except_connections.contains(&cid) || except_connections.contains(&ref_) {
                //println!("SKIPPING {:?}", (cid,ref_));
                //
            } else {
                let node = track_nodes.entry(tid)
                    .or_insert_with(|| graph.add_node(tid)).clone();
                let other_node = track_nodes.entry(connections[ref_])
                    .or_insert_with(|| graph.add_node(connections[ref_])).clone();
                //println!("ADDING {:?} {:?} {:?}", (cid,ref_), (tid,track_nodes[tid]),(connections[ref_], track_nodes[connections[ref_]]));
                graph.add_edge(node, other_node, ());
            }
        }
    }

    Ok((track_nodes,graph))
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

