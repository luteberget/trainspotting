use simulation::{Simulation, Process, ProcessState};
use railway::{Railway, TrainId, NodeId, Train, ObjectId, TrainVisitable, Object};
use smallvec::SmallVec;
use dynamics::*;

pub enum ModelContainment {
    Inside,
    Exiting,
}

pub struct Driver {
    train_id: TrainId,
    authority: f64,
    step: (DriverAction, f64),
    connected_signals: SmallVec<[(ObjectId, f64); 4]>,
}

impl Driver {
    pub fn new(sim: &mut Simulation<Railway>,
               node: NodeId,
               auth: f64,
               params: TrainParams)
               -> Self {
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

    pub fn goto_node(&mut self, sim: &mut Simulation<Railway>, node: NodeId) {
        for obj in sim.world.objects_at(node) {
            for p in sim.world.objects[obj].arrive_front(obj, self.train_id) {
                sim.start_process(p);
            }
            self.arrive_front(sim, obj);
            let length = sim.world.trains[self.train_id].params.len;
            sim.world.trains[self.train_id].under_train.push((obj, length));
        }
    }

    pub fn arrive_front(&mut self, sim: &Simulation<Railway>, obj: ObjectId) {
        use railway::Object::*;
        match sim.world.objects[obj] {
            Sight { distance, signal } => {
                self.connected_signals.push((signal, distance));
            }
            Signal { .. } => {
                self.connected_signals.retain(|&mut (s, d)| s != obj);
            }
        }
    }

    pub fn move_train(&mut self, sim: &mut Simulation<Railway>) -> ModelContainment {
        let (action, action_time) = self.step;
        let dt = *sim.time - action_time;
        if dt <= 1e-4 {
            return ModelContainment::Inside;
        }

        let mut procs = Vec::new();
        let mut containment = ModelContainment::Inside;
        let mut new_start_node = None;

        {
            let ref mut trains = sim.world.trains;
            let ref mut objects = sim.world.objects;
            let ref nodes = sim.world.nodes;

            let ref mut t = trains[self.train_id];
            let DistanceVelocity { dx, v } = 
                dynamic_update(&t.params, t.velocity, 
                               DriverPlan { action, dt });
                                                            

            t.velocity = v;
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
                    }
                    Single((new_end_node, new_dist)) => {
                        t.location = ((new_start_node.unwrap(), new_end_node), new_dist);
                    }
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

    pub fn plan_ahead(&mut self, sim: &Simulation<Railway>) -> DriverPlan {
        let train = &sim.world.trains[self.train_id];

        // Travel distance is limited by next node
        let mut max_dist = train.location.1;

        // Travel distance is limited by nodes under train
        for &(n,d) in train.under_train.iter() {
            max_dist = max_dist.min(d);
        }

        // Travel distance is limited by sight distances
        for &(n,d) in self.connected_signals.iter() {
            max_dist = max_dist.min(d);
        }

        // Authority is updated by signals
        for &(sig, dist) in self.connected_signals.iter() {
            match sim.world.objects[sig] {
                Object::Signal { ref authority } => {
                    match authority.get() {
                        &Some(d) => {
                            self.authority = dist + d;
                        } 
                        &None => {
                            self.authority = dist - 20.0;
                            break;
                        }
                    }
                }
                _ => panic!("Not a signal")
            }
        }

        // Static maximum speed profile ahead from current position
        // TODO: other speed limitations
        let static_speed_profile = StaticMaximumVelocityProfile {
            local_max_velocity: 100.0,
            max_velocity_ahead: SmallVec::from_slice(&[
               DistanceVelocity { dx: self.authority, v: 0.0 }]),
        };

        dynamic_plan_step(&train.params, max_dist, 
                          train.velocity, &static_speed_profile)
    }
}

impl Process<Railway> for Driver {
    fn resume(&mut self, sim: &mut Simulation<Railway>) -> ProcessState {
        let modelcontainment = self.move_train(sim);
        match modelcontainment {
            ModelContainment::Exiting => ProcessState::Finished,
            ModelContainment::Inside => {
                let plan = self.plan_ahead(sim);

                let mut events = SmallVec::new();
                if plan.dt > 1e-4 {
                    events.push(sim.create_timeout(plan.dt));
                }
                for &(ref sig, _) in self.connected_signals.iter() {
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
