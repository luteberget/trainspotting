use eventsim::{Simulation, Process, ProcessState};
use super::infrastructure::*;
use input::staticinfrastructure::*;
use smallvec::SmallVec;
use super::dynamics::*;
use std::f64::INFINITY;


enum ModelContainment {
    Inside,
    Exiting,
}

#[derive(Debug)]
struct Train {
    name: String,
    location: (NodeId, (Option<NodeId>, f64)),
    velocity: f64,
    params: TrainParams,
    under_train: SmallVec<[(ObjectId, f64); 4]>,
}

pub struct Driver {
    train: Train,
    authority: f64,
    step: (DriverAction, f64),
    connected_signals: SmallVec<[(ObjectId, f64); 4]>,
}

impl Driver {
    pub fn new(sim: &mut Simulation<Infrastructure>,
               name: String,
               node: NodeId,
               auth: f64,
               params: TrainParams)
               -> Self {
        let next = match sim.world.next_node(node) {
            Some(x) => x,
            None => panic!("Derailed in first node"),
        };

        let train = Train {
            name: name,
            params: params,
            location: (node, next),
            velocity: 0.0,
            under_train: SmallVec::new(),
        };

        let mut d = Driver {
            train: train,
            authority: auth,
            step: (DriverAction::Coast, *sim.time),
            connected_signals: SmallVec::new(),
        };
        d.goto_node(sim, node);
        d
    }

    fn goto_node(&mut self, sim: &mut Simulation<Infrastructure>, node: NodeId) {
        for obj in sim.world.statics.nodes[node].objects.clone() {
            for p in sim.world.statics.objects[obj].arrive_front(obj) {
                sim.start_process(p);
            }
            self.arrive_front(sim, obj);
            self.train.under_train.push((obj, self.train.params.length));
        }
    }

    fn arrive_front(&mut self, sim: &Simulation<Infrastructure>, obj: ObjectId) {
        match sim.world.statics.objects[obj] {
            StaticObject::Sight { distance, signal } => {
                self.connected_signals.push((signal, distance));
            }
            StaticObject::Signal { .. } => {
                self.connected_signals.retain(|&mut (s, d)| s != obj);
            }
            _ => {}
        }
    }

    fn move_train(&mut self, sim: &mut Simulation<Infrastructure>) -> ModelContainment {
        let (action, action_time) = self.step;
        let dt = *sim.time - action_time;

        if dt <= 1e-4 {
            return ModelContainment::Inside;
        }

        let DistanceVelocity { dx, v } = dynamic_update(&self.train.params,
                                                        self.train.velocity,
                                                        DriverPlan {
                                                            action: action,
                                                            dt: dt,
                                                        });

        self.train.velocity = v;
        (self.train.location.1).1 -= dx;

        self.train.under_train.retain(|&mut (obj, ref mut dist)| {
            *dist -= dx;
            if *dist < 1e-4 {
                // Cleared a node.
                for p in sim.world.statics.objects[obj].arrive_back(obj) {
                    sim.start_process(p);
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

        let (start_node, (end_node, dist)) = self.train.location;
        if dist < 1e-4 && end_node.is_some() {
            let new_start = sim.world.statics.nodes[end_node.unwrap()].other_node;
            self.goto_node(sim, new_start);
            match sim.world.next_node(new_start) {
                Some((Some(new_end_node), d)) => {
                    self.train.location = (new_start, (Some(new_end_node), d));
                    ModelContainment::Inside
                }
                Some((None, d)) => {
                    self.train.location = (new_start, (None, d));
                    ModelContainment::Exiting
                }
                None => panic!("Derailed"),
            }
        } else {
            ModelContainment::Inside
        }
    }

    fn plan_ahead(&mut self, sim: &Simulation<Infrastructure>) -> DriverPlan {
        // Travel distance is limited by next node
        let mut max_dist = (self.train.location.1).1;

        // Travel distance is limited by nodes under train
        for &(n, d) in self.train.under_train.iter() {
            max_dist = max_dist.min(d);
        }

        // Travel distance is limited by sight distances
        for &(n, d) in self.connected_signals.iter() {
            max_dist = max_dist.min(d);
        }

        // Authority is updated by signals
        for &(sig, dist) in self.connected_signals.iter() {
            match sim.world.state[sig] {
                ObjectState::Signal { ref authority } => {
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
                _ => panic!("Not a signal"),
            }
        }

        // Static maximum speed profile ahead from current position
        // TODO: other speed limitations
        let static_speed_profile = StaticMaximumVelocityProfile {
            local_max_velocity: 100.0,
            max_velocity_ahead: SmallVec::from_slice(&[DistanceVelocity {
                                                           dx: self.authority,
                                                           v: 0.0,
                                                       }]),
        };

        dynamic_plan_step(&self.train.params,
                          max_dist,
                          self.train.velocity,
                          &static_speed_profile)
    }
}

impl<'a> Process<Infrastructure<'a>> for Driver {
    fn resume(&mut self, sim: &mut Simulation<Infrastructure>) -> ProcessState {
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
                    match sim.world.state[*sig] {
                        ObjectState::Signal { ref authority } => events.push(authority.event()),
                        _ => panic!("Object is not a signal"),
                    }
                }
                ProcessState::Wait(events)
            }
        }
    }
}
