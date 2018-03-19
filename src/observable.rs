use simulation::{Scheduler, EventId};


#[derive(Clone)]
pub struct Observable<T: Clone> {
    event_id: EventId,
    value: T,
}

impl<T: Clone> Observable<T> {
    pub fn new(scheduler: &mut Scheduler, value: T) -> Observable<T> {
        let event_id = scheduler.new_event();
        Observable {
            event_id: event_id,
            value: value,
        }
    }

    pub fn event(&self) -> EventId {
        self.event_id
    }
    pub fn get(&self) -> &T {
        &self.value
    }
    pub fn set(&mut self, scheduler: &mut Scheduler, x: T) {
        self.value = x;
        scheduler.schedule(self.event_id, 0.0);
        self.event_id = scheduler.new_event();
    }
}
