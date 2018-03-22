use smallvec::SmallVec;
use ordered_float::OrderedFloat;
use std::collections::BinaryHeap;
use std::mem;

pub trait TimeLogger {
    fn advance_time(&mut self, t :f64);
}

pub type EventId = usize;
pub type ProcessId = usize;

pub enum ProcessState {
    Finished,
    Wait(SmallVec<[EventId; 2]>),
}

pub trait Process<T,L:TimeLogger> {
    fn resume(&mut self, sim: &mut Simulation<T,L>) -> ProcessState;
    fn abort(&mut self, sim: &mut Simulation<T,L>) {}
}

pub enum EventState {
    Ready,
    Firing,
    Success,
    Failure,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Debug)]
pub struct QueuedEvent {
    pub time: OrderedFloat<f64>,
    pub id: usize,
    pub event: EventId,
}

pub struct Event {
    state: EventState,
    listeners: Vec<ProcessId>,
}

pub struct Simulation<T,L:TimeLogger> {
    pub time: OrderedFloat<f64>,
    pub world: T,
    procs: Vec<Option<(EventId, Box<Process<T,L>>)>>,
    pub scheduler: Scheduler,
    pub logger: Option<L>,
}

pub struct Scheduler {
    events: Vec<Event>,
    pub queue: BinaryHeap<QueuedEvent>,
    id_counter: usize,
}

impl Scheduler {
    pub fn new() -> Self {
        Scheduler {
            events: Vec::new(),
            queue: BinaryHeap::new(),
            id_counter: 0,
        }
    }
    pub fn new_event(&mut self) -> EventId {
        let event_id = self.events.len();
        self.events.push(Event {
            state: EventState::Ready,
            listeners: vec![],
        });
        event_id
    }

    pub fn schedule(&mut self, id: EventId, t: f64) {
        let qe = QueuedEvent {
            time: OrderedFloat::from(t),
            id: self.id_counter,
            event: id,
        };
        self.id_counter += 1;
        self.queue.push(qe);
    }

    pub fn fire(&mut self, id: EventId) -> Vec<ProcessId> {
        self.events[id].state = EventState::Firing;
        mem::replace(&mut self.events[id].listeners, Vec::new())
    }

    pub fn finish(&mut self, id: EventId) {
        self.events[id].state = EventState::Success;
    }
}

impl<T,L:TimeLogger> Simulation<T,L> {
    pub fn create_timeout(&mut self, dt: f64) -> EventId {
        let id = self.scheduler.new_event();
        self.schedule(id, dt);
        id
    }

    pub fn set_logger(&mut self, logger :L) {
        self.logger = Some(logger);
    }
    pub fn take_logger(&mut self) -> Option<L>{
        self.logger.take()
    }

    pub fn schedule(&mut self, id: EventId, dt: f64) {
        self.scheduler.schedule(id, *self.time + dt)
    }

    pub fn new_with_scheduler(world: T, scheduler: Scheduler) -> Self {
        Simulation {
            time: OrderedFloat::from(0.0),
            procs: Vec::new(),
            scheduler: scheduler,
            world: world,
            logger: None,
        }
    }

    pub fn new(world: T) -> Self {
        Simulation {
            time: OrderedFloat::from(0.0),
            procs: Vec::new(),
            scheduler: Scheduler {
                events: Vec::new(),
                queue: BinaryHeap::new(),
                id_counter: 0,
            },
            world: world,
            logger: None,
        }
    }

    pub fn start_process(&mut self, p: Box<Process<T,L>>) -> EventId {
        let eventid = self.scheduler.new_event();
        let process_id = self.procs.len();
        self.procs.push(Some((eventid, p)));
        self.resume(process_id);
        eventid
    }


    pub fn advance_by(&mut self, dt: f64) {
        let target = OrderedFloat::from(*self.time + dt);
        while let Some(&QueuedEvent { time, .. }) = self.scheduler.queue.peek() {
            if time > target {
                break;
            }
            self.step();
        }
        if let Some(ref mut logger) = self.logger { logger.advance_time(*target - *self.time) }
        self.time = OrderedFloat::from(target);
    }

    pub fn step(&mut self) -> bool {
        match self.scheduler.queue.pop() {
            Some(ev) => {
                if let Some(ref mut logger) = self.logger { 
                    logger.advance_time(*ev.time - *self.time) 
                }
                self.time = ev.time;
                self.fire(ev.event);
                true
            }
            None => false,
        }
    }


    pub fn fire(&mut self, event_id: EventId) {
        let proc_ids = self.scheduler.fire(event_id);
        for process_id in proc_ids {
            self.resume(process_id);
        }
        self.scheduler.finish(event_id);
    }

    fn resume(&mut self, process_id: ProcessId) {
        let (event_id, mut process) = {
            let a = self.procs.get_mut(process_id).unwrap();
            // We need to take the process out of the simulation
            // This creates safety againts the process
            // firing events that modify the process itself.
            // This should be impossible -- a process must either be
            // running OR waiting for an event, not both.
            a.take().unwrap()
        };
        match process.resume(self) {
            ProcessState::Finished => {
                self.schedule(event_id, 0.0);
            }
            ProcessState::Wait(evs) => {
                for x in evs {
                    if !self.scheduler.events[x].listeners.contains(&process_id) {
                        self.scheduler.events[x].listeners.push(process_id);
                    }
                }

                // Put the process back in the array.
                self.procs[process_id] = Some((event_id, process));
            }
        }
    }
}
