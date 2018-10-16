#[macro_use]
extern crate serde_json;
extern crate webbrowser;
extern crate ws;
extern crate notify;
extern crate milelang;
extern crate vis_rs;
extern crate rolling;

mod listen_railcomplete;
mod watch_milelang;
mod frontend;
mod watch;
use std::collections::BTreeMap;

#[derive(Debug)]
pub enum ViewUpdate {
    Reload,
    Refresh,
    Schematic { json_data :serde_json::Value },
    Routes { json_data: serde_json::Value, },
    Scenario { name :String, json_data: serde_json::Value },
}

use std::sync::mpsc::*;
fn run(rc :Receiver<ViewUpdate>, broadcast :ws::Sender) -> Result<(),()> {
    println!("Waiting for internal update message");

    let mut current_schematic = json!([]);
    let mut current_routes = json!([]);
    let mut current_scenarios :BTreeMap<String,serde_json::Value> = BTreeMap::new();

    while let Ok(msg) = rc.recv() {
        println!("Sending view update {:?}.", msg);
        match msg {
            ViewUpdate::Reload => {
                broadcast.send(serde_json::to_string(&json!(
                            {"key": "reload"})).unwrap()).unwrap();
            },
            ViewUpdate::Refresh => {
                broadcast.send(serde_json::to_string(
                        &json!({"key":"schematic", "value": current_schematic}))
                    .unwrap()).unwrap();
            },
            ViewUpdate::Schematic { json_data } => {
                current_schematic = json_data;
                broadcast.send(serde_json::to_string(
                        &json!({"key":"schematic", "value": current_schematic}))
                    .unwrap()).unwrap();
            },
            _ => {
                // ...
            }
        }
    }

    println!("Internal update channel closed.");
    Ok(())
}

use std::path::PathBuf;
struct Opt {
    listen_railcomplete :Option<String>,
    watch_milelang :Option<PathBuf>,
    port :u16,
}

use std::{thread, time};
fn main() {
    let opt :Opt = Opt {
        listen_railcomplete: Some(format!("localhost:1409")),
        watch_milelang: Some("examples/input.mile".into()),
        port: 1409,
    };
    let port = opt.port.clone();

    let (ext_tx,ext_rx) = channel();

    if let Some(addr) = opt.listen_railcomplete {
        let tx = ext_tx.clone();
        thread::spawn(move || {
            listen_railcomplete::forever(&addr, tx);
        });
    }

    if let Some(file) = opt.watch_milelang {
        let tx = ext_tx.clone();
        thread::spawn(move || {
            watch_milelang::forever(&file, tx);
        });
    }

    // Start frontend
    let broadcast = frontend::serve(port, ext_tx);

    // Start web browser
    //
    thread::spawn(move || {
        let addr = format!("http://localhost:{}/",port);
        thread::sleep(time::Duration::from_millis(100));
        eprintln!("Info: Opening web browser, address {}", &addr);
        webbrowser::open(&addr).unwrap();
    });


    run(ext_rx, broadcast).unwrap();
}
