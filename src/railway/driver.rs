use eventsim::{Process, ProcessState};
use super::infrastructure::*;
use input::staticinfrastructure::*;
use smallvec::SmallVec;
use super::dynamics::*;
use output::history::TrainLogEvent;
use super::Sim;

enum ModelContainment {
    Inside,
    Exiting,
}

#[derive(Debug)]
struct Train {
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
    logger: Box<Fn(TrainLogEvent)>,
}

impl Driver {
    pub fn new(sim: &mut Sim,
               node: NodeId,
               auth: f64,
               params: TrainParams,
               logger: Box<Fn(TrainLogEvent)>)
               -> Self {

        if *sim.time() > 0.0 {
            logger(TrainLogEvent::Wait(*sim.time()));
        }

        // The starting node is actually the opposite node of the
        // boundary node given as input here.
        logger(TrainLogEvent::Node(node));
        let node = sim.world.statics.nodes[node].other_node;
        logger(TrainLogEvent::Node(node));
        let next = match sim.world.edge_from(node) {
            Some(x) => x,
            None => panic!("Derailed in first node"),
        };

        let train = Train {
            params: params,
            location: (node, next),
            velocity: 0.0,
            under_train: SmallVec::new(),
        };

        logger(TrainLogEvent::Edge(node, next.0));

        let mut d = Driver {
            train: train,
            authority: auth,
            step: (DriverAction::Coast, *sim.time()),
            connected_signals: SmallVec::new(),
            logger: logger,
        };
        d.goto_node(sim, node);
        d
    }

    fn goto_node(&mut self, sim: &mut Sim, node: NodeId) {
        //println!("TRAIN goto node {}", node);
        for obj in sim.world.statics.nodes[node].objects.clone() {
            if let Some(p) = sim.world.statics.objects[obj].arrive_front() {
                sim.start_process(p);
            }
            self.arrive_front(sim, obj);
            self.train.under_train.push((obj, self.train.params.length));
        }
    }

    fn arrive_front(&mut self, sim: &Sim, obj: ObjectId) {
        match sim.world.statics.objects[obj] {
            StaticObject::Sight { distance, signal } => {
                self.connected_signals.push((signal, distance));
                (self.logger)(TrainLogEvent::Sight(signal,true));
            }
            StaticObject::Signal { .. } => {
                let log = &mut self.logger;
                self.connected_signals.retain(|&mut (s, _d)| {
                    let lost = s == obj;
                    if lost { log(TrainLogEvent::Sight(s,false)); }
                    !lost
                });
            }
            _ => {}
        }
    }

    fn move_train(&mut self, sim: &mut Sim) -> ModelContainment {
        let (action, action_time) = self.step;
        let dt = *sim.time() - action_time;

        if dt <= 1e-4 {
            return ModelContainment::Inside;
        }

        let update = dynamic_update(&self.train.params, self.train.velocity, 
                                    DriverPlan { action: action, dt: dt, });

        (self.logger)(TrainLogEvent::Move(dt, action, update));
        self.train.velocity = update.v;
        (self.train.location.1).1 -= update.dx;

        self.train.under_train.retain(|&mut (obj, ref mut dist)| {
            *dist -= update.dx;
            if *dist < 1e-4 {
                // Cleared a node.
                if let Some(p) = sim.world.statics.objects[obj].arrive_back() {
                    sim.start_process(p);
                }
                false
            } else {
                true
            }
        });

        {
        let log = &mut self.logger;
        self.connected_signals.retain(|&mut (obj, ref mut dist)| {
            *dist -= update.dx;
            let lost = *dist < 1e-4;
            if lost { log(TrainLogEvent::Sight(obj, false)); } 
            !lost
        });
        }

        let (_, (end_node, dist)) = self.train.location;
        if dist < 1e-4 && end_node.is_some() {
            let new_start = sim.world.statics.nodes[end_node.unwrap()].other_node;
            (self.logger)(TrainLogEvent::Node(end_node.unwrap()));
            self.goto_node(sim, new_start);
            (self.logger)(TrainLogEvent::Node(new_start));
            match sim.world.edge_from(new_start) {
                Some((Some(new_end_node), d)) => {
                    self.train.location = (new_start, (Some(new_end_node), d));
                    (self.logger)(TrainLogEvent::Edge(new_start, Some(new_end_node)));
                    ModelContainment::Inside
                }
                Some((None, d)) => {
                    self.train.location = (new_start, (None, d));
                    (self.logger)(TrainLogEvent::Edge(new_start, None));
                    ModelContainment::Exiting
                }
                None => panic!("Derailed"),
            }
        } else {
            ModelContainment::Inside
        }
    }

    fn plan_ahead(&mut self, sim: &Sim) -> DriverPlan {
        // Travel distance is limited by next node
        let mut max_dist = (self.train.location.1).1;

        // Travel distance is limited by nodes under train
        for &(_n, d) in self.train.under_train.iter() {
            max_dist = max_dist.min(d);
        }

        // Travel distance is limited by sight distances
        for &(_n, d) in self.connected_signals.iter() {
            max_dist = max_dist.min(d);
        }

        // Authority is updated by signals
        for &(sig, dist) in self.connected_signals.iter() {
            match sim.world.state[sig] {
                ObjectState::Signal { ref authority } => {
                    match *authority.get() {
                        Some(d) => {
                            self.authority = dist + d;
                        }
                        None => {
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
            local_max_velocity: 20.0,
            max_velocity_ahead: SmallVec::from_slice(&[DistanceVelocity {
               dx: self.authority, v: 0.0}]),
        };

        dynamic_plan_step(&self.train.params,
                          max_dist,
                          self.train.velocity,
                          &static_speed_profile)
    }
}

impl<'a> Process<Infrastructure<'a>> for Driver {
    fn resume(&mut self, sim: &mut Sim) -> ProcessState {
        //println!("resume train");
        let modelcontainment = self.move_train(sim);
        match modelcontainment {
            ModelContainment::Exiting => {
                //println!("TRAIN FINISHED");
                ProcessState::Finished
            },

            ModelContainment::Inside => {
                let plan = self.plan_ahead(sim);
                self.step = (plan.action, *sim.time());

                let mut events = SmallVec::new();
                if plan.dt > 1e-4 {
                    events.push(sim.create_timeout(plan.dt));
                } else {
                    if self.train.velocity > 1e-4 { panic!("Velocity, but no plan."); }
                    self.train.velocity = 0.0;
                    self.step.0 = DriverAction::Coast;
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
