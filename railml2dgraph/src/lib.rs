pub extern crate minidom;
extern crate petgraph;

pub mod base;
pub mod branching;
pub mod sight;
pub mod dgraph;
pub mod sections;
pub mod routes;
pub mod output;

use std::fs::File;
use std::io::prelude::*;
use std::path;
use std::collections::HashMap;


pub fn get_xml(input_fn: &path::Path, verbose: bool) -> Result<(minidom::Element, String), String> {
    let mut f = File::open(input_fn)
        .map_err(|e| format!("File not found: {:?}",e))?;
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .map_err(|e| format!("Could not read file: {:?}",e))?;
    get_xml_string(&contents, verbose)

}

pub fn get_xml_string(contents :&str, verbose: bool) -> Result<(minidom::Element, String), String> {
    let doc: minidom::Element = contents.parse()
        .map_err(|e| format!("Could not parse XML: {:?}",e))?;
    let ns = doc.ns().ok_or("Missing XML namespace.")?.to_string();
    if verbose {
        println!("Namespace {:?}", ns);
    }

    Ok((doc, ns))
}

pub fn convert(doc :&minidom::Element, ns :&str) -> Result<(dgraph::DGraphModel, HashMap<String,Vec<(String,String)>>), String> {
    // read branching model from xml
    let mut branching = branching::get_branching_model(&doc, &ns)?;
    // add sight to branching model
    sight::add_sight(&mut branching);
    // convert to d-graph representation
    let mut dgraph = dgraph::convert(branching)?;
    // convert detector locations to enter/exit objects on part-nodes
    let sections = sections::create_sections_from_detectors(&mut dgraph);
    let sections = sections.into_iter().map(|(k,v)| {
            let v = v.into_iter().map(|(pn1,pn2)| 
              (dgraph.nodes[pn1.node_idx()].get_part(pn1.node_part()).name.clone(),
               dgraph.nodes[pn2.node_idx()].get_part(pn2.node_part()).name.clone())
               ).collect();
                (k,v)
        }).collect();

    Ok((dgraph,sections))
}
