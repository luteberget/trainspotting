use observable::Observable;
use simulation::{Simulation, EventId};
use railway::{Railway, TrainId, TrainVisitable};

#[derive(Copy, Clone)]
pub enum SwitchPosition {
    Left, 
    Right,
    Unknown
}

#[derive(Clone)]
pub struct Switch {
    pub locked: Observable<SwitchPosition>,
    turning: Option<(SwitchPosition, f64)>,
    position: f64,
}

impl TrainVisitable for Switch {
    fn arrive_front(&mut self, sim :&mut Simulation<Railway>, t :TrainId) {
    }
    fn cloneit(&self) -> Box<TrainVisitable> { Box::new(self.clone()) }
}
