use simulation::Simulation;
use railway::{Railway, ObjectId, TrainId, TrainVisitable};

#[derive(Clone)]
pub struct Sight {
    to_signal: ObjectId,
    distance: f64,
}

impl TrainVisitable for Sight {
    fn arrive_front(&mut self, sim :&mut Simulation<Railway>, t :TrainId) {
        match sim.world.trains.get_mut(t) {
            Some(t) => (), //t.can_see(self.to_signal, self.distance),
            _ => panic!("Unknown train"),
        }
    }

    fn cloneit(&self) -> Box<TrainVisitable> { Box::new(self.clone()) }
}
