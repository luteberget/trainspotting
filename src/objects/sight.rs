use observable::*;
use railway::{ObjectId, TrainVisitable};

pub struct Sight {
    signal :ObjectId,
    distance :f64,
}

// Nothing happens in the infrastructure when train arrives at sighting point
impl TrainVisitable for Sight {}
