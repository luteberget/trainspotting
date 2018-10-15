use std::path::Path;
use std::sync::mpsc::channel;
use notify::Watcher;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::thread;
use std::time;
use notify;


pub fn update_file_bytes<F:FnMut(&[u8])+Send+'static>(f :&Path, mut update :F) {
    let f = f.to_path_buf();

        let (watcher_tx, watcher_rx) = channel();
        let mut watcher = notify::watcher(watcher_tx, time::Duration::from_millis(100)).unwrap();
        let canonical = fs::canonicalize(f).unwrap();
        let path = canonical.parent().unwrap().to_path_buf();
    thread::spawn(move || {
        watcher.watch(&path, notify::RecursiveMode::Recursive).unwrap();

        loop {
            match watcher_rx.recv() {
                Ok(event) => {
                    match event {
                        notify::DebouncedEvent::Create(x)
                            | notify::DebouncedEvent::Write(x)
                            | notify::DebouncedEvent::Rename(_,x) => {

                            if x == canonical {
                                eprintln!("Info: File changed event");
                                let mut buf = Vec::new();
                                let mut f = File::open(x).expect("Could not open file.");
                                f.read_to_end(&mut buf).expect("Could not read file.");
                                update(&buf);
                            }
                        }
                        _ => {},
                    }
                },
                Err(e) => {
                    eprintln!("Warning: file watch error: {:?}", e);
                }
            }
        }
    });
}

pub fn update_file_string<F:FnMut(&str)+Send+'static>(f :&Path, mut update :F) {
    let f = f.to_path_buf();

        let (watcher_tx, watcher_rx) = channel();
        let mut watcher = notify::watcher(watcher_tx, time::Duration::from_millis(100)).unwrap();
        let canonical = fs::canonicalize(f).unwrap();
        let path = canonical.parent().unwrap().to_path_buf();
    thread::spawn(move || {
        watcher.watch(&path, notify::RecursiveMode::Recursive).unwrap();

        loop {
            match watcher_rx.recv() {
                Ok(event) => {
                    match event {
                        notify::DebouncedEvent::Create(x)
                            | notify::DebouncedEvent::Write(x)
                            | notify::DebouncedEvent::Rename(_,x) => {

                            if x == canonical {
                                eprintln!("Info: File changed event");
                                let mut buf = String::new();
                                let mut f = File::open(x).expect("Could not open file.");
                                f.read_to_string(&mut buf).expect("Could not read file.");
                                update(&buf);
                            }
                        }
                        _ => {},
                    }
                },
                Err(e) => {
                    eprintln!("Warning: file watch error: {:?}", e);
                }
            }
        }
    });
}
