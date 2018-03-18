use smallvec::SmallVec;
use simulation::*;
use observable::Observable;

pub type NodeId = usize;
pub type ObjectId = usize;
pub type TrainId = usize;

pub trait TrainVisitable {
    fn arrive_front(&self, object :ObjectId, train :TrainId) -> Option<Box<Process<Railway>>> { None }
    fn arrive_back(&self, object :ObjectId, train :TrainId) -> Option<Box<Process<Railway>>> { None }
}

// State of the train, NOT the driver
pub struct Train {
    pub location: ((NodeId,NodeId), f64),
    pub velocity: f64,
    pub params: TrainParams,
    pub under_train: SmallVec<[(ObjectId, f64); 4]>,
}

//pub struct Object {}
pub enum Object {
    Sight { distance: f64, signal: ObjectId } ,
    Signal { authority: Observable<Option<f64>> },
}

impl TrainVisitable for Object {}

pub struct Railway {
    pub nodes :Vec<Node>,
    pub objects :Vec<Object>,
    pub trains :Vec<Train>,
}

pub struct Node {
    pub other_node :NodeId,
    pub edges: Edges,
    pub objects: SmallVec<[ObjectId;2]>,
}

pub enum Edges {
    Nothing,
    ModelBoundary,
    Single((NodeId,f64)),
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
