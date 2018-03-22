use smallvec::SmallVec;
use eventsim::*;
use eventsim::observable::Observable;
use super::dynamics::TrainParams;
use input::staticinfrastructure::*;
use std::f64::INFINITY;

pub type TrainId = usize;

pub trait TrainVisitable {
    fn arrive_front(&self, object: ObjectId) -> Option<Box<Process<Infrastructure>>> {
        None
    }
    fn arrive_back(&self, object: ObjectId) -> Option<Box<Process<Infrastructure>>> {
        None
    }
}

#[derive(Debug)]
pub enum ObjectState {
    Sight ,
    Signal { authority: Observable<Option<f64>> },
    Switch {
        position: Observable<Option<SwitchPosition>>,
        throwing: Option<ProcessId>,
        reserved: Observable<bool>,
    },
    TVDSection {
        reserved: Observable<bool>,
        occupied: Observable<bool>,
    },
    TVDLimit,
}

pub struct MoveSwitch {
    pub sw :ObjectId,
    pub pos :SwitchPosition,
    pub state: bool,
}

impl Process<Infrastructure> for MoveSwitch {
    fn resume(&mut self, sim: &mut Simulation<Infrastructure>) -> ProcessState {
        if !self.state {
            self.state = true;
            ProcessState::Wait(SmallVec::from_slice(&[sim.create_timeout(5.0)]))
        } else {
            match sim.world.state[self.sw] {
                ObjectState::Switch { ref mut position, .. } => position.set(&mut sim.scheduler, Some(self.pos)),
                _ => panic!("Not a switch"),
            }
            ProcessState::Finished
        }
    }
}


#[derive(Copy, Clone)]
enum DetectEvent {
    Enter(ObjectId),
    Exit(ObjectId),
}

impl Process<Infrastructure> for DetectEvent {
    fn resume(&mut self, sim: &mut Simulation<Infrastructure>) -> ProcessState {
        let ref mut infstate = sim.world.state;
        let ref mut scheduler = sim.scheduler;
        match self.clone() {
            DetectEvent::Enter(obj) => {
                match infstate[obj] {
                    ObjectState::TVDSection { ref mut occupied, .. } => occupied.set(scheduler, true),
                    _ => panic!("Not a TVD section"),
                }
            }
            DetectEvent::Exit(obj) => {
                match infstate[obj] {
                    ObjectState::TVDSection { ref mut occupied, .. } => occupied.set(scheduler, false),
                    _ => panic!("Not a TVD section"),
                }
            }
        };
        ProcessState::Finished
    }
}

impl TrainVisitable for StaticObject {
    fn arrive_front(&self, object: ObjectId) -> Option<Box<Process<Infrastructure>>> {
        match self {
            &StaticObject::TVDLimit { enter, .. } => {
                match enter {
                    Some(tvd) => Some(Box::new(DetectEvent::Enter(tvd))),
                    _ => None,
                }
            }
            //&Object::Other(ref obj) => obj.arrive_front(object, train),
            _ => None,
        }
    }

    fn arrive_back(&self, object: ObjectId) -> Option<Box<Process<Infrastructure>>> {
        match self {
            &StaticObject::TVDLimit { exit, .. } => {
                match exit {
                    Some(tvd) => Some(Box::new(DetectEvent::Exit(tvd))),
                    _ => None,
                }
            }
            //&Object::Other(ref obj) => obj.arrive_back(object, train),
            _ => None,
        }
    }
}


#[derive(Debug)]
pub struct Infrastructure {
    pub statics :StaticInfrastructure,
    pub state :Vec<ObjectState>,
}

impl Infrastructure {
    pub fn new(scheduler :&mut Scheduler, 
               infrastructure :StaticInfrastructure) -> Infrastructure {
        use input::staticinfrastructure::StaticObject::*;
        let state = infrastructure.objects.iter().map(|o| match o {
            &Sight { .. } => ObjectState::Sight,
            &Signal { .. } => ObjectState::Signal { 
                authority: Observable::new(scheduler, None) },
            &TVDLimit { .. } => ObjectState::TVDLimit,
            &TVDSection => ObjectState::TVDSection {
                reserved: Observable::new(scheduler, false),
                occupied: Observable::new(scheduler, false),
            },
            &Switch {..}=> ObjectState::Switch {
                position: Observable::new(scheduler, None),
                throwing: None,
                reserved: Observable::new(scheduler, false),
            }
        }).collect();
        Infrastructure {
            statics: infrastructure,
            state: state,
        }
    }

    pub fn next_node(&self, node: NodeId)
                     -> Option<(Option<NodeId>, f64)> {
        let new_start_node = self.statics.nodes[node].other_node;
        match self.statics.nodes[new_start_node].edges {
            Edges::Nothing => None,
            Edges::ModelBoundary => Some((None, 1000.0)),
            Edges::Single(next_node, dist) => Some((Some(next_node), dist)),
            Edges::Switchable(sw) => {
                match (&self.statics.objects[sw], &self.state[sw]) {
                    (&StaticObject::Switch { ref left_link, ref right_link, .. },
                     &ObjectState::Switch { ref position, .. } ) => {
                        match position.get() {
                            &Some(SwitchPosition::Left) => Some((Some(left_link.0), left_link.1)),
                            &Some(SwitchPosition::Right) => Some((Some(right_link.0), right_link.1)),
                            &None => None,
                        }
                    },
                    _ => panic!("Not a switch"),
                }
            }
        }
    }
}

