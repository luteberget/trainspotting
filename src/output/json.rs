use failure::Error;
use super::history;
use railway::dynamics::DriverAction;

use std::collections::HashMap;
use input::staticinfrastructure::{StaticInfrastructure, SwitchPosition};
fn get(x: &HashMap<String, usize>, n: usize) -> &str {
    for (k, v) in x.iter() {
        if *v == n {
            return k;
        }
    }
    panic!("unknown node/object {}", n);
}

use std::io;

pub fn javascript_history<W: io::Write>(inf: &StaticInfrastructure,
                                        history: &history::History,
                                        f: &mut W)
                                        -> Result<(), Error> {
    write!(f, "var data = ")?;
    json_history(inf, history, f)?;
    write!(f, ";")?;
    Ok(())
}

pub fn json_history<W: io::Write>(inf: &StaticInfrastructure,
                                  history: &history::History,
                                  f: &mut W)
                                  -> Result<(), Error> {
    let w = |f: &mut W, t, e, r, v| {
        write!(f,
               "{{ \"time\": {}, \"event\": \"{}\", \"ref\": \"{}\", \"value\": \"{}\" }}",
               t,
               e,
               r,
               v)
            .unwrap();
    };

    write!(f, "{{ \"infrastructure\": [\n")?;
    let mut t = 0.0;
    let mut first = true;
    for ev in &history.inf {
        use output::history::InfrastructureLogEvent::*;
        match *ev {
            Wait(dt) => t += dt,
            Authority(n, x) => {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                w(f,
                  t,
                  "signal",
                  get(&inf.object_names, n),
                  if x.is_some() { "green" } else { "false " });
            }
            Route(n, x) => {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f,
                       "{{ \"time\": {}, \"event\": \"{}\", \"ref\": \"{}\", \"value\": \
                        \"{:?}\" }}",
                       t,
                       "route",
                       n,
                       x)?;
            }
            Reserved(n, x) => {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                w(f,
                  t,
                  "reserved",
                  get(&inf.object_names, n),
                  if x { "true" } else { "false" });
            }
            Occupied(n, x) => {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                w(f,
                  t,
                  "occupied",
                  get(&inf.object_names, n),
                  if x { "true" } else { "false" });
            }
            Position(n, pos) => {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                w(f,
                  t,
                  "position",
                  get(&inf.object_names, n),
                  if let SwitchPosition::Left = pos {
                      "left"
                  } else {
                      "right"
                  });
            }
        }
    }
    write!(f, " ]")?;

    write!(f, ", \"trains\": {{ ")?;
    let mut firsttrain = true;
    for &(ref name, ref his) in &history.trains {
        if firsttrain {
            firsttrain = false;
        } else {
            write!(f, ", ")?;
        }
        write!(f, " \"{}\": [", name)?;
        let mut t = 0.0;
        let mut first = true;
        let mut x = 0.0;
        let mut edges = Vec::new();
        let trainlength = 200.0;

        for ev in his {
            use output::history::TrainLogEvent::*;
            use railway::dynamics::DistanceVelocity;
            match *ev {
                Wait(dt) => {
                    t += dt;
                }
                Node(_n1) => {}
                Edge(n1, n2) => {
                    edges.insert(0, ((n1, n2), (0.0, 0.0)));
                }
                Sight(_s, _x) => {}
                Move(dt, action, DistanceVelocity { dx, v }) => {
                    if first {
                        first = false;
                        let edge_strings = edges.iter()
                            .map(|&((n1, n2), (a, b))| {
                                format!("{{\"n1\": \"{}\", \"n2\": {}, \"start\": {}, \
                                         \"end\": {}}}",
                                        get(&inf.node_names, n1),
                                        match n2 {
                                            Some(n2) => format!("\"{}\"", get(&inf.node_names, n2)),
                                            None => format!("null"),
                                        },
                                        a,
                                        b)
                            })
                            .collect::<Vec<String>>();
                        let edge_string = format!("[{}]", edge_strings.join(", "));
                        write!(f,
                               "{{ \"time\" : {}, \"action\": \"{:?}\", \"x\": {}, \"dx\": {}, \
                                \"v\": {}, \"edges\": {} }}",
                               t,
                               DriverAction::Coast,
                               0.0,
                               0.0,
                               0.0,
                               edge_string)?;

                    }

                    t += dt;
                    x += dx;

                    let mut l = trainlength;
                    if edges.len() > 0 {
                        let mut frontier = &mut edges[0].1;
                        frontier.1 += dx;
                    }

                    let mut edge_num = 0;
                    for edge in &mut edges {
                        edge_num += 1;
                        let mut interval = &mut edge.1;
                        if interval.1 - interval.0 > l {
                            interval.0 = interval.1 - l;
                            break;
                        } else {
                            l -= interval.1 - interval.0;
                        }
                    }
                    edges.truncate(edge_num);
                    let edge_strings = edges.iter()
                        .map(|&((n1, n2), (a, b))| {
                            format!("{{\"n1\": \"{}\", \"n2\": {}, \"start\": {}, \"end\": \
                                     {}}}",
                                    get(&inf.node_names, n1),
                                    match n2 {
                                        Some(n2) => format!("\"{}\"", get(&inf.node_names, n2)),
                                        None => format!("null"),
                                    },
                                    a,
                                    b)
                        })
                        .collect::<Vec<String>>();
                    let edge_string = format!("[{}]", edge_strings.join(", "));

                    write!(f, ", ")?;
                    write!(f,
                           " {{ \"time\" : {}, \"action\": \"{:?}\", \"x\" : {}, \"dx\" : {}, \
                            \"v\": {}, \"edges\": {} }}",
                           t,
                           action,
                           x,
                           dx,
                           v,
                           edge_string)?;
                }
            }
        }
        write!(f, " ]")?;
    }
    write!(f, " }} ")?;
    write!(f, " }}")?;
    Ok(())
}
