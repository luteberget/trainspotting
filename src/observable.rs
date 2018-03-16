use simulation::{Simulation, EventId};


#[derive(Clone)]
pub struct Observable<T: Clone> {
    event_id: EventId,
    value: T,
}

impl<T :Clone> Observable<T> {
    pub fn new<X>(sim: &mut Simulation<X>, value :T) -> Observable<T> {
        let event_id = sim.new_event();
        Observable { event_id, value }
    }
    
    pub fn get(&self) -> &T { &self.value }
    pub fn set<X>(&mut self, sim: &mut Simulation<X>, x :T) { 
        self.value = x;
        sim.schedule(self.event_id, 0.0);
        self.event_id = sim.new_event();
    }
}
