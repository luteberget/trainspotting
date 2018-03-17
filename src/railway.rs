use smallvec::SmallVec;
use simulation::*;

pub type NodeId = usize;
pub type ObjectId = usize;
pub type TrainId = usize;

pub trait TrainVisitable {
    fn arrive_front(&self, object :ObjectId, train :TrainId) -> Option<Box<Process<Railway>>> { None }
    fn arrive_back(&self, object :ObjectId, train :TrainId) -> Option<Box<Process<Railway>>> { None }
}

pub struct TrainParams {
    pub len :f64,
    pub max_acc: f64,
    pub max_brk: f64,
    pub max_vel: f64,
}

// State of the train, NOT the driver
pub struct Train {
    pub location: ((NodeId,NodeId), f64),
    pub velocity: f64,
    pub params: TrainParams,
    pub under_train: SmallVec<[(ObjectId, f64); 4]>,
    pub connected_signals: SmallVec<[(ObjectId, f64); 4]>,
}

pub struct Object {}

impl TrainVisitable for Object {}

pub struct Railway {
    pub nodes :Vec<Node>,
    pub objects :Vec<Object>,
    pub trains :Vec<Train>,
}

pub struct Node {
    other_node :usize,
    edges: Edges,
    objects: SmallVec<[ObjectId;2]>,
}

pub enum Edges {
    Nothing,
    ModelBoundary,
    Single((usize,f64)),
    Switchable(ObjectId),
}


impl Railway {
    pub fn next_from(&self, n :usize) -> Option<usize> {
        let node = &self.nodes[n];
        use self::Edges::*;
        match node.edges {
            Nothing => None,
            ModelBoundary => None,
            Single((other, ref _data)) => Some(other),
            Switchable(obj) => None,
        }
    }

    pub fn objects_at(&self, n: usize) -> SmallVec<[ObjectId; 2]> {
        self.nodes[n].objects.clone()
    }
}
