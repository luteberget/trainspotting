use smallvec::SmallVec;
use simulation::*;
use dgraph::*;
use std::any::*;

pub trait TrainVisitable : Any {
  fn arrive_front(&mut self, sim :&mut Simulation<Railway>, train_id :TrainId) {}
  fn arrive_back(&mut self, sim :&mut Simulation<Railway>, train_id :TrainId) {}
  fn cloneit(&self) -> Box<TrainVisitable>;
}

pub type NodeId = usize;
pub type ObjectId = usize;
pub type TrainId = usize;

// State of the train, NOT the driver
pub struct Train {
    pub length: f64,
    pub location: ((NodeId,NodeId), f64),
    pub velocity: f64,
    pub under_train: SmallVec<[(ObjectId, f64); 4]>,
    pub connected_signals: SmallVec<[(ObjectId, f64); 4]>,
}

pub struct Railway {
    pub graph :DGraph<f64>,
    pub objects :Vec<Box<TrainVisitable>>,
    pub trains: Vec<Train>,
}

pub struct DGraph< EdgeData> {
    pub nodes :Vec<Node<EdgeData>>,
}

pub struct Node<Edge> {
    other_node :usize,
    edges: Edges<Edge>,
    objects: SmallVec<[ObjectId;2]>,
}

pub enum Edges<Edge> {
    Nothing,
    ModelBoundary,
    Single((usize,Edge)),
    Switchable(ObjectId),
}


impl<E> DGraph<E> {
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
