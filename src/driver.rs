use simulation::{Simulation, Process, ProcessState};
use railway::{Railway, TrainId, NodeId, Train, ObjectId, TrainParams, TrainVisitable};
use smallvec::SmallVec;


pub struct Driver {
    train_id: TrainId,
    authority: f64,
    step :(DriverAction, f64),
    connected_signals: SmallVec<[(ObjectId, f64); 4]>,
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
        });

        let mut d = Driver {
            train_id: train_id,
            authority: auth,
            step: (DriverAction::Coast, *sim.time),
            connected_signals: SmallVec::new(),
        };
        d.goto_node(sim, node);
        d
    }

    pub fn goto_node(&mut self, sim :&mut Simulation<Railway>, node :NodeId) {
        for obj in sim.world.objects_at(node) {
            for p in sim.world.objects[obj].arrive_front(obj, self.train_id) {
                sim.start_process(p);
            }
            self.arrive_front(sim, obj);
            let length = sim.world.trains[self.train_id].params.len;
            sim.world.trains[self.train_id].under_train.push((obj, length));
        }
    }

    pub fn arrive_front(&mut self, sim :&Simulation<Railway>, obj :ObjectId) {
        use railway::Object::*;
        match sim.world.objects[obj] {
            Sight { distance, signal } => { 
                self.connected_signals.push((signal, distance));
            },
            Signal { .. } => {
                self.connected_signals.retain(|&mut (s,d)| s != obj);
            },
        }
    }

    pub fn move_train(&mut self, sim :&mut Simulation<Railway>) -> ModelContainment {
        let (action, action_time) = self.step;
        let dt = *sim.time - action_time;
        if dt <= 1e-4 { return ModelContainment::Inside; }

        let mut procs = Vec::new();
        let mut containment = ModelContainment::Inside;
        let mut new_start_node = None;

        {
          let ref mut trains = sim.world.trains;
          let ref mut objects = sim.world.objects;
          let ref nodes = sim.world.nodes;

          let ref mut t = trains[self.train_id];
          let TrainUpdate { dx, v } = train_update( &t.params, t.velocity,
                                                     (action, dt));

          t.velocity    = v;
          t.location.1 -= dx;

          t.under_train.retain(|&mut (obj, ref mut dist)| {
              *dist -= dx;
              if *dist < 1e-4 {
                  // Cleared a node.
                  for p in objects[obj].arrive_back(obj, self.train_id) {
                      procs.push(p);
                  }
                  false
              } else {
                  true
              }
          });

          self.connected_signals.retain(|&mut (obj, ref mut dist)| {
              *dist -= dx;
              *dist < 1e-4 
          });

          let ((start_node, end_node), dist) = t.location;
          if dist < 1e-4 {
              use railway::Edges::*;
              new_start_node = Some(nodes[end_node].other_node);
              match nodes[new_start_node.unwrap()].edges {
                  Nothing => panic!("Ran off end of track."),
                  ModelBoundary => { 
                      containment = ModelContainment::Exiting; 
                      t.location = ((new_start_node.unwrap(), 0), 1e100);
                  },
                  Single((new_end_node,new_dist)) => { 
                      t.location = ((new_start_node.unwrap(),new_end_node), new_dist); 
                  },
                  Switchable(sw_id) => unimplemented!(),
              };
          }
        }

        for p in procs {
            sim.start_process(p);
        }

        for new_start_node in new_start_node {
            self.goto_node(sim, new_start_node);
        }

        containment
    }

    pub fn plan_ahead(&mut self) -> DriverPlan { 
        DriverPlan { max_t: 0.0 }
    }

}

pub fn train_update(params :&TrainParams, velocity :f64, (action,dt) :(DriverAction, f64)) -> TrainUpdate {
    TrainUpdate{ dx:0.0, v:0.0 }
}


impl Process<Railway> for Driver {
    fn resume(&mut self, sim :&mut Simulation<Railway>) -> ProcessState {
        let modelcontainment = self.move_train(sim);
        match modelcontainment {
            ModelContainment::Exiting => ProcessState::Finished,
            ModelContainment::Inside => {
              let auth = calc_authority(&sim, self.train_id, &self.connected_signals);
              let plan = self.plan_ahead();

              let mut events = SmallVec::new();
              if plan.max_t > 1e-4 {
                  events.push(sim.create_timeout(plan.max_t));
              }
              for &(ref sig,_) in self.connected_signals.iter() {
                  use railway::Object::*;
                  match sim.world.objects[*sig] {
                      Signal { ref authority } => events.push(authority.event()),
                      _ => panic!("Object is not a signal"),
                  }
              }
              ProcessState::Wait(events)
            }
        }
    }
}

pub enum ModelContainment {
    Inside,
    Exiting,
}


fn calc_authority(sim :&Simulation<Railway>, train_id :TrainId, conn: &[(ObjectId,f64)]) -> () {
    ()
}
