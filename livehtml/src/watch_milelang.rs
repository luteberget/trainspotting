use ViewUpdate;
use std::sync::mpsc::Sender;
use watch;
use serde_json;
use std::path::Path;

pub fn forever(file :&Path, tx :Sender<ViewUpdate>) {
    watch::update_file_string(file, move |s| {
        println!("Input update.");
        tx.send(ViewUpdate::Schematic {
            json_data: json!(s)
        }).unwrap();
    });
}
