use std::fmt::Write;
use failure::Error;
use super::history;

use std::collections::HashMap;
use input::staticinfrastructure::{StaticInfrastructure, SwitchPosition};
fn get(x :&HashMap<String,usize>, n :usize) -> &str {
    for (k,v) in x.iter() {
        if *v == n { return k; }
    }
    panic!("unknown node/object {}", n);
}

use std::io;

pub fn javascript_history<W : io::Write>(inf :&StaticInfrastructure, history: &history::History, f :&mut W) -> Result<(),Error> {
    write!(f, "var data = ")?;
    json_history(inf, history, f);
    write!(f, ";")?;
    Ok(())
}

pub fn json_history<W : io::Write>(inf :&StaticInfrastructure, history: &history::History, f :&mut W) -> Result<(),Error> {
    let w = |f :&mut W,t,e,r,v| { write!(f, "{{ \"time\": {}, \"event\": \"{}\", \"ref\": \"{}\", \"value\": \"{}\" }}", t, e, r, v).unwrap(); };

    write!(f, "{{ \"infrastructure\": [\n" )?;
    let mut t = 0.0;
    let mut first = true;
    for ev in &history.inf {
        use output::history::InfrastructureLogEvent::*;
        match *ev {
            Wait(dt) => t += dt,
            Authority(n,x) => {
                if first { first = false; } else { write!(f, ", ")?; }
                w(f, t, "signal", get(&inf.object_names, n), if x.is_some() { "green" } else { "false "});
            },
            Route(n,x) => {
                if first { first = false; } else { write!(f, " ,")?; }
                write!(f, "{{ \"time\": {}, \"event\": \"{}\", \"ref\": \"{}\", \"value\": \"{:?}\" }}", t, "route", n, x)?;
            },
            Reserved(n,x) => {
                if first { first = false; } else { write!(f, ", ")?; }
                w(f, t, "reserved", get(&inf.object_names, n), if x { "true" } else { "false" });
            }
            Occupied(n,x) => {
                if first { first = false; } else { write!(f, " ,")?; }
                w(f, t, "occupied", get(&inf.object_names, n), if x { "true" } else { "false" });
            }
            Position(n,pos) => {
                if first { first = false; } else { write!(f, " ,")?; }
                w(f, t, "occupied", get(&inf.object_names, n), if let SwitchPosition::Left = pos { "left" } else { "right" });
            }
            _ => {},
        }
    }
    write!(f, " ]" )?;

    write!(f, ", \"trains\": {{ ")?;
    let mut firsttrain = true;
    for &(ref name,ref his) in &history.trains {
        if firsttrain { firsttrain = false; } else { write!(f, ", ")?; }
        write!(f, " \"{}\": [", name)?;
        let mut t = 0.0;
        let mut first = true;
        let mut x = 0.0;
        for ev in his {
            use output::history::TrainLogEvent::*;
            use railway::dynamics::DistanceVelocity;
            match *ev {
                Wait(dt) => t += dt,
                Node(n1, n2) => {},
                Sight(s, x) => {},
                Move(dt, action, DistanceVelocity { dx, v }) => {
                    t += dt;
                    x += dx;
                    if first { first = false; } else { write!(f, ", ")?; }
                    write!(f, " {{ \"time\" : {}, \"action\": \"{:?}\", \"x\" : {}, \"dx\" : {}, \"v\": {} }}", 
                           t, action, x, dx, v)?;
                },
            }
        }
        write!(f, " ]")?;
    }
    write!(f, " }} " )?;
    write!(f, " }}" )?;
    Ok(())
}
