// Print railway branching-track model to railML
//
use railway::*;

pub fn to_railml(model :Vec<Track>) -> Result<String, String> {
    let mut buf = String::new();
    buf.push_str(r#"
    <?xml version="1.0" encoding="UTF-8"?>
<railml xmlns="https://www.railml.org/schemas/2018"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="https://www.railml.org/schemas/2018 ../schema/railML.xsd"
        version="2.4">
        
       
        "#);
    buf.push_str(r#"  <infrastructure id="inf01"> "#);
    buf.push_str("  <tracks> \n");

    for t in model {
        buf.push_str(&format!("<track id=\"{}\" name=\"{}\">\n",  t.id, t.name));
        buf.push_str("<trackTopology>\n");
        buf.push_str("<trackBegin pos=\"0\">\n");
        buf.push_str(&conv_node(t.begin));
        buf.push_str("</trackBegin>\n");
        buf.push_str(&format!("<trackEnd pos=\"{}\">\n", t.length));
        buf.push_str(&conv_node(t.end));
        buf.push_str("</trackEnd>\n");
        conv_switches(&mut buf, t.switches);
        buf.push_str("</trackTopology>\n");
        conv_objects(&mut buf, t.objects);
        buf.push_str(&format!("</track>\n"));
        buf.push_str("\n\n");
    }

    buf.push_str("\n  </tracks> ");
    buf.push_str("\n  </infrastructure> ");
    buf.push_str("\n  </railml> ");
    Ok(buf)
}

fn conv_switches(buf :&mut String, sws :Vec<Switch>) {
    if sws.len() == 0 { return; }
    buf.push_str("<connections>\n");
    for sw in sws {
        let orientation = match &sw.dir {
            Dir::Up => "outgoing",
            Dir::Down => "incoming",
        };
        let (id,ref_) = if let RConnection::Internal(i,r) = sw.connection { (i,r) } 
            else { panic!("Switch connection is not model-internal."); };

        buf.push_str(&format!("<switch id=\"{}\" name=\"{}\" dir=\"{}\" pos=\"{}\">\n", sw.id, sw.name, sw.dir.to_string(), sw.pos));
        buf.push_str(&format!("<connection course=\"{}\" orientation=\"{}\" id=\"{}\" ref=\"{}\" />",
                              sw.side.to_string(), orientation, id, ref_));
        buf.push_str("</switch>\n");
    }
    buf.push_str("</connections>\n");
}

fn conv_objects(buf :&mut String, objs :Vec<Object>) {
    let signals = objs.iter().filter(|o| if let ObjectData::Signal { .. } = o.data { true } else { false }).collect::<Vec<_>>();
    let detectors = objs.iter().filter(|o| if let ObjectData::Detector { .. } = o.data { true } else { false }).collect::<Vec<_>>();

    if signals.len() > 0 || detectors.len() > 0 {
        buf.push_str("<ocsElements>\n");
    }

    if signals.len() > 0 {
        buf.push_str("<signals>\n");
        for s in &signals {
            if let ObjectData::Signal { dir } = s.data {
                buf.push_str(&format!("<signal id=\"{}\" name=\"{}\" dir=\"{}\" pos=\"{}\" type=\"main\" />\n", s.id, s.name, dir.to_string(), s.pos ));
            }
        }
        buf.push_str("</signals>\n");
    }
    
    if detectors.len() > 0 {
        buf.push_str("<trainDetectionElements>\n");
        for s in &signals {
            if let ObjectData::Detector { } = s.data {
                buf.push_str(&format!("<trainDetector id=\"{}\" name=\"{}\" dir=\"both\" pos=\"{}\" type=\"main\" />\n", s.id, s.name, s.pos ));
            }
        }
        buf.push_str("</trainDetectionElements>\n");
    }

    if signals.len() > 0 || detectors.len() > 0 {
        buf.push_str("</ocsElements>\n");
    }
}

fn conv_node(node :Node) -> String {
    match node {
        Node::Stop => format!("<bufferStop />\n"),
        Node::Connection(RConnection::Internal(a,b)) => 
            format!("<connection id=\"{}\" ref=\"{}\"/>\n", a,b),
        Node::Connection(RConnection::External(a)) => 
            format!("<openEnd id=\"{}\" />\n", a),
        _ => panic!("Convert node maybe"),
    }
}
