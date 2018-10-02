use base::*;
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
        match walk(&m, &cursor, remaining_dist, delta) {
            WalkResult::Ok(cursor) => {
                // Just insert
                m.tracks[cursor.track].objs.push(BrObject {
                    name: format!("sight_{}",name),
                    pos: cursor.offset,
                    data: BrObjectData::Sight { 
                        dir: cursor.dir.opposite(), signal: name, distance: total_dist }
                });
            },
            WalkResult::TrailingSwitch(d, _c, cursor2) => {
                // Search further
                signals.push((cursor2, total_dist, remaining_dist - d, name.clone()));
            },
            WalkResult::FacingSwitch(d, cursor,_c1,_c2) => {
                // Just insert with warning
                let deficiency = remaining_dist - d;
                println!("Warning: sight distance for {:?} truncated at facing switch from {} to {}", name, total_dist, total_dist - deficiency);
                m.tracks[cursor.track].objs.push(BrObject {
                    name: format!("sight_{}", name),
                    pos: cursor.offset,
                    data: BrObjectData::Sight { 
                        dir: cursor.dir.opposite(), signal: name, 
                        distance: total_dist - deficiency }
                });
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
