use ViewUpdate;
use std::sync::mpsc::Sender;
use watch;
use serde_json;
use std::path::Path;
use milelang;
use vis_rs;
use rolling::input::staticinfrastructure::*;
use std::collections::HashMap;

fn infrastructure_objects(inf :&StaticInfrastructure) -> serde_json::Value {
    fn get(x: &HashMap<String, usize>, n: usize) -> Option<&str> {
        for (k, v) in x.iter() {
            if *v == n {
                return Some(k);
            }
        }
        None
    }

    let mut i = 0;
    let mut j = &mut i;
    let mut fresh = move || { *j += 1; format!("unnamed{}",j) };

    let mut v = json!({});
    for (node_idx,node) in inf.nodes.iter().enumerate() {
        for obj in &node.objects {
            println!("OBJECT {:?}", inf.objects[*obj]);
            use rolling::input::staticinfrastructure::StaticObject;
            let mut data = json!({"node": get(&inf.node_names, node_idx).unwrap()});
            match inf.objects[*obj] {
                StaticObject::Signal => {
                    data.as_object_mut().unwrap().insert(format!("type"),json!("signal"));
                },
                StaticObject::TVDLimit { .. } => {
                    data.as_object_mut().unwrap().insert(format!("type"),json!("detector"));
                },
                StaticObject::Sight { distance, signal } => {
                    data.as_object_mut().unwrap().insert(format!("type"),json!("sight"));
                    data.as_object_mut().unwrap().insert(format!("distance"),json!(distance));
                    data.as_object_mut().unwrap().insert(format!("signal"),json!(get(&inf.object_names, signal).unwrap()));
                },
                StaticObject::Switch { branch_side, .. } => {
                    data.as_object_mut().unwrap().insert(format!("type"),json!("switch"));
                    data.as_object_mut().unwrap().insert(format!("side"),
                    json!(match branch_side {
                        SwitchPosition::Left => "left",
                        SwitchPosition::Right => "right",
                    }));
                }
                _ => {} , // ignore tvdsection
            }

            v.as_object_mut().unwrap().insert(get(&inf.object_names,*obj).map(|x| x.to_string()).unwrap_or_else(|| fresh()), data);
        }
    }
    v
}

fn schematic_update(s :&str) -> Result<serde_json::Value, String> {
    let inf = milelang::convert_dgraph(s).map_err(|e| format!("{:?}",e))?;
    let object_data = infrastructure_objects(&inf);
    let schematic = vis_rs::convert_dgraph(&inf)?;
    let (edge_lines,node_data) = vis_rs::convert_javascript(schematic)?;
    Ok(json!({"lines": edge_lines, "nodes": node_data, "objects": object_data}))
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
