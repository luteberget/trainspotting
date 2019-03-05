extern crate quick_xml;
#[macro_use]
extern crate structopt;

#[derive(Clone, Debug)]
struct Connection {
    id :String,
    ref_ :String,
}

#[derive(Clone, Debug)]
enum Node {
    Connection(Connection),
    Boundary(String),
}

#[derive(Copy, Clone, Debug)]
enum Side { Left, Right }

#[derive(Copy, Clone, Debug)]
enum Direction { Up, Down }

#[derive(Clone, Debug)]
struct Track {
    begin :Node,
    end :Node,
    length: f64,
    signals :Vec<f64>,
    detectors :Vec<f64>,
    switches :Vec<(f64, Side, Direction, Connection)>,
}


fn mk_connection(i :&mut usize) -> (Connection,Connection) {
    let ida = format!("conn{}-a", i);
    let idb = format!("conn{}-b", i);
    *i += 1;
    (Connection { id: ida.clone(), ref_: idb.clone() },
     Connection { id: idb,         ref_: ida         })
}

fn mk_station_track(tracks :&mut Vec<Track>, spec :&ModelSpec, 
                    conn_i :&mut usize, num :usize, 
                    conn_in: Connection, conn_out :Connection) {

        let length = 2.0*((num+2) as f64) *spec.switch_length + 
            spec.effective_station_track_length;

        let mut station_track = Track {
            begin: Node::Connection(conn_in),
            end: Node::Connection(conn_out),
            length: length,
            signals: vec![length - 2.0*spec.switch_length -20.0],
            switches: vec![],
            detectors: vec![spec.switch_length - 5.0, length - 2.0*spec.switch_length - 20.0],
        };

        if num > 1 {
            // Add switches and another track.
            
            let (sw1ca,sw1cb) = mk_connection(conn_i);
            let (sw2ca,sw2cb) = mk_connection(conn_i);
            station_track.switches.push((spec.switch_length, Side::Left, Direction::Up, sw1ca));
            station_track.switches.push((length - spec.switch_length, Side::Right, Direction::Down, sw2ca));

            mk_station_track(tracks, spec, conn_i, num-1, sw1cb, sw2cb);
        }

        tracks.push(station_track);
}

fn create_model(spec :&ModelSpec) -> Vec<Track> {
    let mut tracks = Vec::new();
    let mut conn_i = 0;

    let (c, mut last_connection) = mk_connection(&mut conn_i);
    tracks.push(Track {
        begin: Node::Boundary("in".to_string()),
        end: Node::Connection(c),
        length: 350.0,
        signals :vec![100.0],
        detectors: vec![50.0, 100.0, 340.0],
        switches: vec![],
    });

    for s in 0..(spec.num_serial) {
        let (c1,c2) = mk_connection(&mut conn_i); 
        mk_station_track(&mut tracks, &spec, &mut conn_i, spec.num_parallel, last_connection, c1);
        last_connection = c2;
    }

    tracks.push(Track {
        begin: Node::Connection(last_connection),
        end: Node::Boundary("out".to_string()),
        length: 500.0,
        signals: vec![400.0],
        detectors: vec![50.0, 400.0, 450.0],
        switches: vec![],
    });

    tracks
}

fn open_elem<'a, W: std::io::Write>(w :&mut quick_xml::Writer<W>, name :Vec<u8>, attrs :Vec<(&'a str, &'a str)>) {
    use quick_xml::Writer;
    use quick_xml::events::{Event, BytesEnd, BytesStart};
    use quick_xml::Reader;
    let len = name.len();
    let mut elem = BytesStart::owned(name, len);
    for &(ref a,ref b) in &attrs {
        elem.push_attribute((*a,*b));
    }
    w.write_event(Event::Start(elem)).unwrap();
}

fn empty_elem<'a, W: std::io::Write>(w :&mut quick_xml::Writer<W>, name :Vec<u8>, attrs :Vec<(&'a str, &'a str)>) {
    use quick_xml::Writer;
    use quick_xml::events::{Event, BytesEnd, BytesStart};
    use quick_xml::Reader;
    let len = name.len();
    let mut elem = BytesStart::owned(name, len);
    for &(ref a,ref b) in &attrs {
        elem.push_attribute((*a,*b));
    }
    w.write_event(Event::Empty(elem)).unwrap();
}

fn close_elem<'a, W: std::io::Write>(w :&mut quick_xml::Writer<W>, name :Vec<u8>) {
    use quick_xml::Writer;
    use quick_xml::events::{Event, BytesEnd, BytesStart};
    use quick_xml::Reader;
    let mut elem = BytesEnd::owned(name);
    w.write_event(Event::End(elem)).unwrap();
}

fn output_xml(model :&Vec<Track>) -> String{
    use quick_xml::Writer;
    use quick_xml::events::{Event, BytesEnd, BytesStart};
    use quick_xml::Reader;
    use std::io::Cursor;
    use std::iter;

    let mut w = Writer::new_with_indent(Cursor::new(Vec::new()), b' ', 4);

    let mut signal_i = 0;
    let mut track_i = 0;
    let mut detector_i = 0;
    let mut switch_i = 0;

    for track in model {
        open_elem(&mut w, b"track".to_vec(),vec![("name",format!("track{}",track_i).as_str())]);
        open_elem(&mut w, b"trackTopology".to_vec(), vec![]);
        open_elem(&mut w, b"trackBegin".to_vec(), vec![]);
        match &track.begin {
            &Node::Connection(ref c) => empty_elem(&mut w, b"connection".to_vec(),
                vec![("id",&c.id), ("ref", &c.ref_)]),
            &Node::Boundary(ref n) => empty_elem(&mut w, b"openEnd".to_vec(),
                vec![("id", n)]),
        };
        close_elem(&mut w, b"trackBegin".to_vec());
        open_elem(&mut w, b"trackEnd".to_vec(), vec![("pos", &format!("{}", track.length))]);
        match &track.end {
            &Node::Connection(ref c) => empty_elem(&mut w, b"connection".to_vec(),
                vec![("id",&c.id), ("ref", &c.ref_)]),
            &Node::Boundary(ref n) => empty_elem(&mut w, b"openEnd".to_vec(),
                vec![("id", n)]),
        };
        close_elem(&mut w, b"trackEnd".to_vec());

        open_elem(&mut w, b"connections".to_vec(), vec![]);
        for &(pos,side,dir,ref conn) in &track.switches {
            open_elem(&mut w, b"switch".to_vec(), 
                      vec![("id", &format!("sw{}", switch_i)),
                           ("pos",&format!("{}", pos))]);
            switch_i += 1;

            empty_elem(&mut w, b"connection".to_vec(),
                       vec![("id", &conn.id), ("ref", &conn.ref_), 
                       ("course", match side {
                           Side::Left => "left",
                           Side::Right => "right" }),
                       ("orientation", match dir {
                           Direction::Up => "outgoing",
                           Direction::Down => "incoming",
                       })]);

            close_elem(&mut w, b"switch".to_vec());
        }
        close_elem(&mut w, b"connections".to_vec());
        close_elem(&mut w, b"trackTopology".to_vec());


        open_elem(&mut w, b"ocsElements".to_vec(), vec![]);

        open_elem(&mut w, b"signals".to_vec(), vec![]);
        for pos in &track.signals {
            empty_elem(&mut w, b"signal".to_vec(),
            vec![
            ("id", &format!("sig{}", signal_i)),
            ("name", &format!("sig{}", signal_i)),
                 ("pos", &format!("{}", pos)),
                 ("type", "main"),
                 ("dir", "up")]);
            signal_i += 1;
        }
        close_elem(&mut w, b"signals".to_vec());


        open_elem(&mut w, b"trainDetectionElements".to_vec(), vec![]);
        for pos in &track.detectors {
            empty_elem(&mut w, b"trainDetector".to_vec(),
            vec![
            ("id", &format!("det{}", detector_i)),
            ("name", &format!("det{}", detector_i)),
                 ("pos", &format!("{}", pos)),
                 ("dir","both")]);
            detector_i += 1;
        }
        close_elem(&mut w, b"trainDetectionElements".to_vec());

        close_elem(&mut w, b"ocsElements".to_vec());

        close_elem(&mut w, b"track".to_vec());

        track_i += 1;
    }

    let result = w.into_inner().into_inner();
    String::from_utf8(result).unwrap()
}

/// Generate railML example models
#[derive(StructOpt, Clone, Debug)]
#[structopt(name="genmodel")]
struct ModelSpec {

    /// Number of stations in serial connection
    #[structopt(short="n", long="num", default_value="2")]
    num_serial :usize,

    /// Number of parallel tracks on each station
    #[structopt(short="t", long="tracks", default_value="3")]
    num_parallel :usize,

    /// Length between stations
    #[structopt(short="s", long="sep", default_value="500.0")]
    station_sep_length :f64,

    /// Length of switch area
    #[structopt(short="x", long="switch-length", default_value="50.0")]
    switch_length :f64,

    /// Effective stations track lengths
    #[structopt(short="l", long="track-length", default_value="250.0")]
    effective_station_track_length :f64
}


fn main() {

    use structopt::StructOpt;
    let model = ModelSpec::from_args();
    let model = create_model(&model);

    //let model = create_model( &ModelSpec { 
    //    num_serial: 2,
    //    num_parallel: 3,
    //    station_sep_length: 500.0,
    //    switch_length: 50.0,
    //    effective_station_track_length: 250.0,
    //});

    let xml = output_xml(&model);

    println!(r#"

<?xml version="1.0" encoding="UTF-8"?>
<railml version="2.2" xmlns="http://www.railml.org/schemas/2013" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.railml.org/schemas/2013 http://schemas.railml.org/2013/railML-2.2/railML.xsd">
    <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
        <dc:source>Example model generator (rolling)</dc:source>
        <dc:date></dc:date>
    </metadata>
    <infrastructure>
      <tracks>
"#);

    println!("{}", xml);

println!(r#"
        </tracks>
    </infrastructure>
</railml>
"#);

}
