use branching::*;

pub fn add_sight(m :&mut BranchingModel) {
    let mut signals = m.tracks.iter().enumerate()
        .flat_map(|(i,x)| {
            x.objs.iter()
                .filter_map(|obj| {
                    if let BrObjectData::Signal { dir, sight } = obj.data {
                        //Some((i, obj.pos, sight, sight, dir, obj.name.clone()))
                        Some((BrCursor { track: i, offset: obj.pos, dir: dir.opposite() },
                             sight, sight, obj.name.clone()))
                    } else {
                        None
                    }
                }).collect::<Vec<_>>()
        }).collect::<Vec<_>>();

    let delta = 1e-6;
    while signals.len() > 0 {
        let (cursor, total_dist, remaining_dist, name) = signals.pop().unwrap();
        use branching::WalkResult;
        let res = walk(&m, &cursor, remaining_dist, delta);
        println!("walk result {:?} {:?}", name, res);
        match res {
            WalkResult::Ok(cursor) => {
                // Just insert
                println!("Inserting on track {:?}", m.tracks[cursor.track].name);
                m.tracks[cursor.track].objs.push(BrObject {
                    name: format!("sight_{}",name),
                    pos: cursor.offset,
                    data: BrObjectData::Sight { 
                        dir: cursor.dir.opposite(), signal: name, distance: total_dist }
                });
            },
            WalkResult::TrailingSwitch(d, before, _after) => {
                // Insert sight here, truncating the sight distance, because
                // we don't know what to do for a driver which sees two different signals.
                let deficiency = remaining_dist - d;
                println!("Warning: sight distance for {:?} truncated at trailing switch from {} to {}", name, total_dist, total_dist - deficiency);
                m.tracks[cursor.track].objs.push(BrObject {
                    name: format!("sight_{}", name),
                    pos: before.offset,
                    data: BrObjectData::Sight { 
                        dir: before.dir.opposite(), signal: name, 
                        distance: total_dist - deficiency }
                });
            },
            WalkResult::FacingSwitch(d, _before,after1,after2) => {
                signals.push((after1, total_dist, remaining_dist - d, name.clone()));
                signals.push((after2, total_dist, remaining_dist - d, name.clone()));
            },
            WalkResult::End(d, cursor) => {
                let deficiency = remaining_dist - d;
                println!("Warning: sight distance for {:?} truncated at model boundary/stop from {} to {}", name, total_dist, total_dist - deficiency);
                m.tracks[cursor.track].objs.push(BrObject {
                    name: format!("sight_{}", name),
                    pos: cursor.offset,
                    data: BrObjectData::Sight { 
                        dir: cursor.dir.opposite(), signal: name, 
                        distance: total_dist - deficiency }
                });
            }
        }
    }
}
