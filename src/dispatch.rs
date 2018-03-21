use staticinfrastructure::*;

pub struct Dispatch {
    actions :Vec<DispatchAction>
}

pub enum DispatchAction {
    Wait(f64),
    Route(String),
    Train(String, TrainParams, NodeId, Dist),
}
