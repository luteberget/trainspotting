extern crate ordered_float;
use ordered_float::OrderedFloat;
use std::collections::BinaryHeap;

pub type EventId = usize;
pub type ProcessId = usize;

pub enum ProcessState {
    Finished,
    Wait(Vec<EventId>),
}

pub trait Process<T> {
    fn resume(&mut self, sim: &mut Simulation<T>, world: &mut T) -> ProcessState;
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
    time: OrderedFloat<f64>,
    procs: Vec<Option<(EventId,Box<Process<T>>)>>,
    events: Vec<Event>,
    queue: BinaryHeap<QueuedEvent>,
    id_counter: usize,
}

impl<T> Simulation<T> {
    pub fn new() -> Self { Simulation {
        time: OrderedFloat::from(0.0),
        procs: Vec::new(),
        events: Vec::new(),
        queue: BinaryHeap::new(),
        id_counter: 0
    }}

    pub fn start_process(&mut self, world :&mut T, p: Box<Process<T>>) -> EventId {
        let eventid = self.events.len();
        self.events.push(Event { state: EventState::Ready, listeners: vec![] });
        let processid = self.procs.len();
        self.procs.push(Some((eventid,p)));
        self.resume(world, processid);
        eventid
    }

    pub fn create_timeout(&mut self, dt :f64) -> EventId {
        let id = self.events.len();
        self.events.push(Event { state: EventState::Ready, listeners: vec![] });
        let qe = QueuedEvent { time: OrderedFloat::from(*self.time + dt), 
            id: self.id_counter,
            event: id };
        self.id_counter += 1;
        self.queue.push(qe);
        id
    }

    pub fn advance_by(&mut self, world: &mut T, dt: f64) {
        let target = OrderedFloat::from(*self.time + dt);
        while let Some(&QueuedEvent { time, .. }) = self.queue.peek() {
            if time > target { break; }
            self.step(world);
        }
        self.time = OrderedFloat::from(target);
    }

    pub fn step(&mut self, world: &mut T) -> bool {
        match self.queue.pop() {
            Some(ev) => {
                self.time = ev.time;
                self.fire(world, ev.event);
                true
            },
            None => false
        }
    }

    fn fire(&mut self, world :&mut T, event_id :EventId) {
        let proc_ids = {
            let ev = self.events.get_mut(event_id).unwrap();
            ev.state = EventState::Firing;
            ev.listeners.drain(..).collect::<Vec<_>>() // TODO mem replace?
        };

        for process_id in proc_ids {
            self.resume(world, process_id);
        }

        let ev = self.events.get_mut(event_id).unwrap();
        ev.state = EventState::Success;
    }

    fn resume(&mut self, world :&mut T, process_id :ProcessId) {
        let (event_id, mut process) = {
            let a = self.procs.get_mut(process_id).unwrap();
            // We need to take the process out of the simulation 
            // This creates safety againts the process
            // firing events that modify the process itself.
            // This should be impossible -- a process must either be 
            // running OR waiting for an event, not both.
            a.take().unwrap()
        };
        match process.resume(self, world) {
            ProcessState::Finished => {
                self.fire(world, event_id);
            },
            ProcessState::Wait(evs) => {
                for x in evs {
                    self.events[x].listeners.push(process_id);
                }

                self.procs[process_id] = Some((event_id, process));
            }
        }
    }
}

struct Car {
    name :usize,
    state: CarState,
}
enum CarState { Starting, Running, Stopping }

impl Process<f64> for Car {
    fn resume(&mut self, sim: &mut Simulation<f64>, world: &mut f64) -> ProcessState {
        match self.state {
            CarState::Starting => {
                println!("Starting the car {} @{}.", self.name, sim.time);
                *world += 15.19;

                self.state = CarState::Running;
                ProcessState::Wait(vec![sim.create_timeout(5.0)])
            },
            CarState::Running => {
                *world += 31.12;
                self.state = CarState::Stopping;
                if self.name < 50 {
                    println!("Waiting for futher car {} @{}", self.name, sim.time);
                    ProcessState::Wait(vec![sim.start_process(world, Box::new(Car { 
                        name: self.name + 10, state: CarState::Starting }))])
                } else {
                println!("Not waiting for futher car {} @{}.", self.name, sim.time);
                    ProcessState::Finished
                }
            },
            CarState::Stopping => {
                *world += 50.123;
                println!("Finished after waiting {} @{}", self.name, sim.time);
                ProcessState::Finished
            }
        }
    }
}

fn main() {
    let mut sim = Simulation::new();
    let mut world :f64 = 0.0;
    let car = Box::new(Car { name: 10, state: CarState::Starting });
    sim.start_process(&mut world, car);
    sim.advance_by(&mut world, 100.0);
    println!("Total gasoline expense: {}", world);
}
