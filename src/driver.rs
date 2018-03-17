use simulation::{Simulation, Process, ProcessState};
use railway::{Railway, TrainId, NodeId, Train, ObjectId, TrainParams, TrainVisitable};
use smallvec::SmallVec;


pub enum DriverAction { Accel, Brake, Coast }

pub struct Driver {
    train_id: TrainId,
    authority: f64,
    step :(DriverAction, f64),
}

impl Driver {
    pub fn new(sim :&mut Simulation<Railway>, node :NodeId, auth :f64, params: TrainParams) -> Self {
        let train_id = sim.world.trains.len();
        let target_node = sim.world.next_from(node).unwrap();
        sim.world.trains.push(Train {
            params: params,
            location: ((node, target_node), 0.0),
            velocity: 0.0,
            under_train: SmallVec::new(),
            connected_signals: SmallVec::new(),
        });

        //let length = sim.world.trains[train_id].length;
        //for obj in sim.world.graph.objects_at(target_node) {
        //    //sim.world.objects[obj].arrive_front(sim, train_id);
        //    //sim.start_process(
        //    let mut f = sim.world.objects[obj].cloneit();
        //    f.arrive_front(sim, train_id);
        //    sim.world.trains[train_id].under_train.push((obj, length));
        //}



        let d = Driver {
            train_id: train_id,
            authority: auth,
            step: (DriverAction::Coast, *sim.time),
        };

        for obj in sim.world.objects_at(node) {
            for p in sim.world.objects[obj].arrive_front(obj, train_id) {
                sim.start_process(p);
            }
            d.arrive_front(obj);
        }

        //d.arrive_front(sim, node);
        d
    }

    pub fn arrive_front(&mut self, obj :ObjectId) {
    //    match object type  {
    //        ...
    //    }
    }
}

impl Process<Railway> for Driver {
    fn resume(&mut self, sim :&mut Simulation<Railway>) -> ProcessState {
        let train = sim.world.trains.get_mut(self.train_id).unwrap();
        ProcessState::Finished
    }
}
