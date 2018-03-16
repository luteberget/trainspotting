use observable::Observable;
use simulation::Simulation;
use railway::{Railway, TrainId, TrainVisitable};

#[derive(Clone)]
pub struct Signal {
    green: Observable<bool>,
    //authority: Observable<f64>
}

impl TrainVisitable for Signal {
    fn arrive_front(&mut self, sim :&mut Simulation<Railway>, t :TrainId) {
    }
    fn cloneit(&self) -> Box<TrainVisitable> { Box::new(self.clone()) }
}
