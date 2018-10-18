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

pub fn convert(doc :&minidom::Element, ns :&str) -> Result<dgraph::DGraphModel, String> {
    // read branching model from xml
    let mut branching = branching::get_branching_model(&doc, &ns)?;
    // add sight to branching model
    sight::add_sight(&mut branching);
    // convert to d-graph representation
    let mut dgraph = dgraph::convert(branching)?;
    // convert detector locations to enter/exit objects on part-nodes
    sections::create_sections_from_detectors(&mut dgraph);

    Ok(dgraph)
}
