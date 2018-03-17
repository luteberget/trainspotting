use observable::*;
use railway::{TrainVisitable};

pub struct Signal {
    authority: Observable<Option<f64>>,
}

// Nothing happens when train arrives or leaves at signal
impl TrainVisitable for Signal {}
