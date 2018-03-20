use smallvec::SmallVec;
use simulation::*;
use observable::Observable;
use dynamics::TrainParams;
use std::f64::INFINITY;

pub type NodeId = usize;
pub type ObjectId = usize;
pub type TrainId = usize;

pub trait TrainVisitable {
    fn arrive_front(&self, object: ObjectId, train: TrainId) -> Option<Box<Process<Railway>>> {
        None
    }
    fn arrive_back(&self, object: ObjectId, train: TrainId) -> Option<Box<Process<Railway>>> {
        None
    }
}

#[derive(Copy, Clone, Debug)]
pub enum SwitchPosition {
    Left,
    Right,
}

// State of the train, NOT the driver
#[derive(Debug)]
pub struct Train {
    pub location: (NodeId, (Option<NodeId>, f64)),
    pub velocity: f64,
    pub params: TrainParams,
    pub under_train: SmallVec<[(ObjectId, f64); 4]>,
}

pub struct Infrastructure {
    nodes :Vec<Node>,
    //sight  :Vec<Sight>,
    //switch :Vec<Switch>,
    //limits: Vec<Limit>,
}

pub struct InfrastructureState {
}

pub struct Route {
    pub signal :ObjectId,
    pub first_trigger :ObjectId,
    pub sections: SmallVec<[ObjectId; 4]>,
    pub switch_positions: SmallVec<[(ObjectId, SwitchPosition);2]>,
    pub length: f64,
    pub releases: SmallVec<[Release;2]>,
}

pub struct Release {
    pub trigger :ObjectId,
    pub resources :SmallVec<[ObjectId; 4]>,
}

// pub struct Object {}

#[derive(Debug)]
pub enum StaticObject {
    Sight { distance: f64, signal: ObjectId },
    Signal, 
    Switch {
        left_link: (NodeId, f64),
        right_link: (NodeId, f64),
    },
    TVDLimit {
        enter: Option<ObjectId>,
        exit: Option<ObjectId>,
    },
    TVDSection,
}

#[derive(Debug)]
pub enum Object {
    Sight { distance: f64, signal: ObjectId },
    Signal { authority: Observable<Option<f64>> },
    Switch {
        position: Observable<Option<SwitchPosition>>,
        left_link: (NodeId, f64),
        right_link: (NodeId, f64),
        throwing: Option<ProcessId>,
        reserved: Observable<bool>,
    },
    TVDSection {
        reserved: Observable<bool>,
        occupied: Observable<bool>,
    },
    TVDLimit {
        enter: Option<ObjectId>,
        exit: Option<ObjectId>,
    },
    //Other(Box<TrainVisitable>),
}

#[derive(Copy, Clone)]
enum Detect {
    Enter(ObjectId),
    Exit(ObjectId),
}

impl Process<Railway> for Detect {
    fn resume(&mut self, sim: &mut Simulation<Railway>) -> ProcessState {
        let ref mut objects = sim.world.objects;
        let ref mut scheduler = sim.scheduler;
        match self.clone() {
            Detect::Enter(obj) => {
                match objects[obj] {
                    Object::TVDSection { ref mut occupied, .. } => occupied.set(scheduler, true),
                    _ => panic!("Not a TVD section"),
                }
            }
            Detect::Exit(obj) => {
                match objects[obj] {
                    Object::TVDSection { ref mut occupied, .. } => occupied.set(scheduler, false),
                    _ => panic!("Not a TVD section"),
                }
            }
        };
        ProcessState::Finished
    }
}

impl TrainVisitable for Object {
    fn arrive_front(&self, object: ObjectId, train: TrainId) -> Option<Box<Process<Railway>>> {
        match self {
            &Object::TVDLimit { enter, .. } => {
                match enter {
                    Some(tvd) => Some(Box::new(Detect::Enter(tvd))),
                    _ => None,
                }
            }
            //&Object::Other(ref obj) => obj.arrive_front(object, train),
            _ => None,
        }
    }

    fn arrive_back(&self, object: ObjectId, train: TrainId) -> Option<Box<Process<Railway>>> {
        match self {
            &Object::TVDLimit { exit, .. } => {
                match exit {
                    Some(tvd) => Some(Box::new(Detect::Exit(tvd))),
                    _ => None,
                }
            }
            //&Object::Other(ref obj) => obj.arrive_back(object, train),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct StaticInfrastructure {
    pub nodes :Vec<Node>,
    pub objects :Vec<StaticObject>,
}

#[derive(Debug)]
pub struct Railway {
    pub nodes: Vec<Node>,
    pub objects: Vec<Object>,
    pub trains: Vec<Train>,
}

#[derive(Debug)]
pub struct Node {
    pub other_node: NodeId,
    pub edges: Edges,
    pub objects: SmallVec<[ObjectId; 2]>,
}

#[derive(Debug)]
pub enum Edges {
    Nothing,
    ModelBoundary,
    Single(NodeId, f64),
    Switchable(ObjectId),
}


pub fn next_node(objects: &Vec<Object>,
                 nodes: &Vec<Node>,
                 n: NodeId)
                 -> Option<(Option<NodeId>, f64)> {
    let new_start_node = nodes[n].other_node;
    match nodes[new_start_node].edges {
        Edges::Nothing => None,
        Edges::ModelBoundary => Some((None, 1000.0)),
        Edges::Single(node, dist) => Some((Some(node), dist)),
        Edges::Switchable(sw) => {
            match objects[sw] {
                Object::Switch { ref position, ref left_link, ref right_link, .. } => {
                    match position.get() {
                        &Some(SwitchPosition::Left) => Some((Some(left_link.0), left_link.1)),
                        &Some(SwitchPosition::Right) => Some((Some(right_link.0), right_link.1)),
                        &None => None,
                    }
                }
                _ => panic!("Not a switch"),
            }
        }
    }
}
