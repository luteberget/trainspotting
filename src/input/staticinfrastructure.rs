use smallvec::SmallVec;

pub type Dist = f64;

pub type NodeId = usize;
pub type ObjectId = usize;

use std::collections::HashMap;
pub type NameMap = HashMap<String,usize>;

#[derive(Debug)]
pub struct StaticInfrastructure {
    pub nodes :Vec<Node>,
    pub objects :Vec<StaticObject>,
    pub node_names: NameMap,
    pub object_names: NameMap,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SwitchPosition {
    Left,
    Right,
}

pub type Routes = HashMap<String, Route>;

#[derive(Debug,Clone)]
pub struct Route {
    pub signal :ObjectId,
    pub signal_trigger :ObjectId,
    pub sections: SmallVec<[ObjectId; 4]>,
    pub switch_positions: SmallVec<[(ObjectId, SwitchPosition);2]>,
    pub length: f64,
    pub releases: SmallVec<[Release;2]>,
}

#[derive(Debug,Clone)]
pub struct Release {
    pub trigger :ObjectId,
    pub resources :SmallVec<[ObjectId; 4]>,
}
