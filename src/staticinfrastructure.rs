use smallvec::SmallVec;

pub type NodeId = usize;
pub type ObjectId = usize;

#[derive(Debug)]
pub struct StaticInfrastructure {
    pub nodes :Vec<Node>,
    pub objects :Vec<StaticObject>,
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

