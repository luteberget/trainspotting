use ViewUpdate;
use std::sync::mpsc::Sender;
use std::{thread,time};
pub fn forever(addr :&str, tx :Sender<ViewUpdate>) {
    thread::sleep(time::Duration::from_millis(1000));

    //let test = railml2dgraph::convert_from_string("");

    println!("Exiting listen_railcomplete");
}
