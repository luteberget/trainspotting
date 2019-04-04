use smallvec::SmallVec;
use ordered_float::OrderedFloat;
use std::collections::BinaryHeap;
use std::mem;

pub type EventId = usize;
pub type ProcessId = usize;

pub enum ProcessState {
    Finished,
    Wait(SmallVec<[EventId; 2]>),
}

pub trait Process<T> {
    fn resume(&mut self, sim: &mut Simulation<T>) -> ProcessState;
    fn abort(&mut self, _sim: &mut Simulation<T>) {}
}

pub enum EventState {
    Ready,
    Firing,
    Success,
    Failure,
}

#[derive(Eq, PartialEq, Debug)]
pub struct QueuedEvent {
    pub time: OrderedFloat<f64>,
    pub id: usize,
    pub event: EventId,
}

use std::cmp::Ordering;
impl Ord for QueuedEvent {
    fn cmp(&self, other :&QueuedEvent) -> Ordering {
        // Note that the order is flipped on purpose -- to turn
        // the (maximum) BinaryHeap into a minimum heap.
        other.time.cmp(&self.time).
            then_with(|| other.id.cmp(&self.id))
    }
}

impl PartialOrd for QueuedEvent {
    fn partial_cmp(&self,other :&QueuedEvent) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[test]
fn test_ordering() {
    println!("Test");
    let mut p = BinaryHeap::new();
    p.push(QueuedEvent { 
        time: OrderedFloat::from(123.0),
        id: 0,
        event: 0
    });
    p.push(QueuedEvent { 
        time: OrderedFloat::from(0.0),
        id: 0,
        event: 0
    });
    p.push(QueuedEvent { 
        time: OrderedFloat::from(122.0),
        id: 0,
        event: 0
    });
    assert_eq!(*p.pop().unwrap().time, 0.0);
    assert_eq!(*p.pop().unwrap().time, 122.0);
    assert_eq!(*p.pop().unwrap().time, 123.0);
}

pub struct Event {
    state: EventState,
    listeners: Vec<ProcessId>,
}

pub struct Simulation<T> {
    pub world: T,
    procs: Vec<Option<(EventId, Box<Process<T>>)>>,
    pub scheduler: Scheduler,
    pub logger: Option<Box<Fn(f64)>>,
}

#[derive(Default)]
pub struct Scheduler {
    pub time: OrderedFloat<f64>,
    events: Vec<Event>,
    pub queue: BinaryHeap<QueuedEvent>,
    id_counter: usize,
}

impl Scheduler {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn new_event(&mut self) -> EventId {
        let event_id = self.events.len();
        self.events.push(Event {
            state: EventState::Ready,
            listeners: vec![],
        });
        event_id
    }

    pub fn schedule(&mut self, id: EventId, dt: f64) {
        if dt < 0.0 { panic!("dt < 0"); }
        if dt.is_infinite() { return; } // Will never happen
        let qe = QueuedEvent {
            time: OrderedFloat::from(*self.time + dt),
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

impl<T> Simulation<T> {
    pub fn has_fired(&self, event :EventId) -> bool {
        if let EventState::Ready = &self.scheduler.events[event].state {
            false
        } else {
            true
        }
    }

    pub fn time(&self) -> &f64 { &self.scheduler.time }

    pub fn create_timeout(&mut self, dt: f64) -> EventId {
        let id = self.scheduler.new_event();
        self.scheduler.schedule(id, dt);
        id
    }

    pub fn set_time_log(&mut self, logger: Box<Fn(f64)>) {
        self.logger = Some(logger);
    }

    pub fn new_with_scheduler(world: T, scheduler: Scheduler) -> Self {
        Simulation {
            procs: Vec::new(),
            scheduler: scheduler,
            world: world,
            logger: None,
        }
    }

    pub fn new(world: T) -> Self {
        Simulation {
            procs: Vec::new(),
            scheduler: Scheduler {
                time: OrderedFloat::from(0.0),
                events: Vec::new(),
                queue: BinaryHeap::new(),
                id_counter: 0,
            },
            world: world,
            logger: None,
        }
    }

    pub fn start_process(&mut self, p: Box<Process<T>>) -> EventId {
        let eventid = self.scheduler.new_event();
        let process_id = self.procs.len();
        self.procs.push(Some((eventid, p)));
        self.resume(process_id);
        eventid
    }


    pub fn advance_to(&mut self, ev :EventId) {
        while let Some(&QueuedEvent { .. }) = self.scheduler.queue.peek() {
            if let EventState::Ready = self.scheduler.events[ev].state {
                self.step();
            } else {
                break;
            }
        }
    }

    pub fn advance_by(&mut self, dt: f64) {
        let target = OrderedFloat::from(*self.time() + dt);
        while let Some(&QueuedEvent { time, .. }) = self.scheduler.queue.peek() {
            if time > target {
                break;
            }
            self.step();
        }

        let time = *self.time();
        if let Some(ref mut logger) = self.logger {
            logger(*target - time);
        }

        self.scheduler.time = target;
    }

    pub fn step(&mut self) -> bool {
        match self.scheduler.queue.pop() {
            Some(ev) => {
                let time = *self.time();
                if let Some(ref mut logger) = self.logger {
                    logger(*ev.time - time);
                }
                self.scheduler.time = ev.time;
                self.fire(ev.event);
                true
            }
            None => false,
        }
    }

    pub fn run(&mut self) { 
        while let true = self.step() {}
    }


    pub fn fire(&mut self, event_id: EventId) {
        let proc_ids = self.scheduler.fire(event_id);
        for process_id in proc_ids {
            self.resume(process_id);
        }
        self.scheduler.finish(event_id);
    }

    fn resume(&mut self, process_id: ProcessId) {
        if let Some((event_id, mut process)) = {
            let a = &mut self.procs[process_id];
            // We need to take the process out of the simulation
            // This creates safety againts the process
            // firing events that modify the process itself.
            // This should be impossible -- a process must either be
            // running OR waiting for an event, not both.
            a.take()

        } {
            loop {
                match process.resume(self) {
                    ProcessState::Finished => {
                        self.scheduler.schedule(event_id, 0.0);
                        break;
                    }
                    ProcessState::Wait(evs) => {
                        let mut waiting = false;
                        for x in evs {
                            let already_listening = self.scheduler.events[x].listeners.contains(&process_id);
                            let event_pending = !self.has_fired(x);
                                //if let EventState::Ready = &self.scheduler.events[x].state { true } else { false };
                            if  event_pending {
                                waiting = true;
                                if !already_listening {
                                    self.scheduler.events[x].listeners.push(process_id);
                                }
                            }
                        }

                        if waiting {
                            // Put the process back in the array.
                            self.procs[process_id] = Some((event_id, process));
                            break;
                        }

                        // If none of the events are pending, resume the process
                        // immediately.
                    }
                }
            }
        }
    }
}
