use smallvec::SmallVec;
use ordered_float::OrderedFloat;
use std::collections::BinaryHeap;

pub type EventId = usize;
pub type ProcessId = usize;

pub enum ProcessState {
    Finished,
    Wait(SmallVec<[EventId;2]>),
}

pub trait Process<T> {
    fn resume(&mut self, sim: &mut Simulation<T>) -> ProcessState;
}

pub enum EventState {
    Ready,
    Firing,
    Success,
    Failure,
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Debug)]
pub struct QueuedEvent {
    pub time :OrderedFloat<f64>,
    pub id: usize,
    pub event: EventId,
}

pub struct Event {
    state :EventState,
    listeners: Vec<ProcessId>,
}

pub struct Simulation<T> {
    pub time: OrderedFloat<f64>,
    pub world: T,
    procs: Vec<Option<(EventId,Box<Process<T>>)>>,
    events: Vec<Event>,
    queue: BinaryHeap<QueuedEvent>,
    id_counter: usize,
}

impl<T> Simulation<T> {
    pub fn new(world :T) -> Self { Simulation {
        time: OrderedFloat::from(0.0),
        procs: Vec::new(),
        events: Vec::new(),
        queue: BinaryHeap::new(),
        id_counter: 0,
        world: world,
    }}

    pub fn new_event(&mut self) -> EventId {
        let event_id = self.events.len();
        self.events.push(Event { state: EventState::Ready, listeners: vec![] });
        event_id
    }

    pub fn start_process(&mut self, p: Box<Process<T>>) -> EventId {
        let eventid = self.new_event();
        let process_id = self.procs.len();
        self.procs.push(Some((eventid,p)));
        self.resume(process_id);
        eventid
    }

    pub fn create_timeout(&mut self, dt :f64) -> EventId {
        let id = self.new_event();
        self.schedule(id, dt);
        id
    }

    pub fn schedule(&mut self, id: EventId, dt :f64) {
        let qe = QueuedEvent { time: OrderedFloat::from(*self.time + dt), 
            id: self.id_counter,
            event: id };
        self.id_counter += 1;
        self.queue.push(qe);
    }

    pub fn advance_by(&mut self, dt: f64) {
        let target = OrderedFloat::from(*self.time + dt);
        while let Some(&QueuedEvent { time, .. }) = self.queue.peek() {
            if time > target { break; }
            self.step();
        }
        self.time = OrderedFloat::from(target);
    }

    pub fn step(&mut self) -> bool {
        match self.queue.pop() {
            Some(ev) => {
                self.time = ev.time;
                self.fire(ev.event);
                true
            },
            None => false
        }
    }


    pub fn fire(&mut self, event_id :EventId) {
        let proc_ids = {
            let ev = self.events.get_mut(event_id).unwrap();
            ev.state = EventState::Firing;
            ev.listeners.drain(..).collect::<Vec<_>>() // TODO mem replace?
        };

        for process_id in proc_ids {
            self.resume(process_id);
        }

        let ev = self.events.get_mut(event_id).unwrap();
        ev.state = EventState::Success;
    }

    fn resume(&mut self, process_id :ProcessId) {
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
            },
            ProcessState::Wait(evs) => {
                for x in evs {
                    if !self.events[x].listeners.contains(&process_id) {
                      self.events[x].listeners.push(process_id);
                    }
                }

                // Put the process back in the array.
                self.procs[process_id] = Some((event_id, process));
            }
        }
    }
}
