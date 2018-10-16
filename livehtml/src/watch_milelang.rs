use ViewUpdate;
use std::sync::mpsc::Sender;
use watch;
use serde_json;
use std::path::Path;
use milelang;
use vis_rs;
use rolling::input::staticinfrastructure::*;

fn schematic_update(s :&str) -> Result<serde_json::Value, String> {
    let inf = milelang::convert_dgraph(s).map_err(|e| format!("{:?}",e))?;
    println!("Parsed infrastructure: {:?}", inf);
    let schematic = vis_rs::convert_dgraph(&inf)?;
    println!("Created schematic: {:?}", schematic);
    let json_schematic = vis_rs::convert_javascript(schematic)?;
    Ok(json_schematic)
}

pub fn forever(file :&Path, tx :Sender<ViewUpdate>) {
    watch::update_file_string(file, move |s| {
        println!("Input update.");
        match schematic_update(&s) {
            Ok(json_data) => {
                tx.send(ViewUpdate::Schematic { json_data }).unwrap();
            },
            Err(e) => {
                println!("Input error: {:?}", e);
                // TODO send error message to frontend?
                // tx.send(ViewUpdate::Error(format!("{:?}", e))).unwrap();
            }
        };

    });
}
