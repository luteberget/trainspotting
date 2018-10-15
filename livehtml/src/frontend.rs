use ws;
use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex};
use ViewUpdate;
use watch;
use std::thread;
use std::path::Path;

struct ServerThread {
    index: Arc<Mutex<Vec<u8>>>,
}

static D3_JS :&'static [u8] = include_bytes!("d3/d3.js");

impl ws::Handler for ServerThread {
    fn on_message(&mut self, _msg :ws::Message) -> ws::Result<()> { Ok(()) }
    fn on_request(&mut self, req :&ws::Request) -> ws::Result<ws::Response> {
        match req.resource() {
            "/ws" => ws::Response::from_request(req),
            "/" => Ok(ws::Response::new(200, "OK", self.index.lock().unwrap().clone())),
            "/d3.js" => Ok(ws::Response::new(200,"OK",D3_JS.to_vec())),
            _ => Ok(ws::Response::new(404, "Not Found", b"404 - Not Found".to_vec())),
        }
    }
}

pub fn serve(port :u16, view_tx :Sender<ViewUpdate>) -> ws::Sender {
    let tx2 = view_tx.clone();
    let index = Arc::new(Mutex::new(Vec::new()));

    let index2 = index.clone();
    watch::update_file_bytes(Path::new("./index.html"), move |buf|  {
        *index2.lock().unwrap() = buf.to_vec();
        tx2.send(ViewUpdate::Reload).unwrap();
    });

    let http = ws::WebSocket::new(move |out:ws::Sender| {
        view_tx.send(ViewUpdate::Refresh).unwrap();
        ServerThread { index: index.clone() }
    }).unwrap();
    let broadcaster = http.broadcaster();

    thread::spawn(move || {
        let addr = format!("localhost:{}",port);
        http.listen(&addr).unwrap();
    });

    broadcaster
}
