use eventsim::{Process, ProcessState, EventId, Scheduler};
use super::Sim;
use smallvec::SmallVec;
use input::staticinfrastructure::*;
use super::infrastructure::*;

enum ActivateRouteState {
    Allocate, // Waiting for resources
    Move, // Waiting for movable elements
}

pub struct ActivateRoute {
    route: Route,
    state: ActivateRouteState,
}

impl ActivateRoute {
    pub fn new(r: Route) -> Self {
        ActivateRoute {
            route: r,
            state: ActivateRouteState::Allocate,
        }
    }
}

fn unavailable_resource(r: &Route, infrastructure: &Infrastructure) -> Option<EventId> {
    for s in r.sections.iter() {
        match infrastructure.state[*s] {
            ObjectState::TVDSection { ref reserved, .. } => {
                if *reserved.get() {
                    return Some(reserved.event());
                }
            }
            _ => panic!("Not a TVD"),
        };
    }

    for &(sw, _pos) in r.switch_positions.iter() {
        match infrastructure.state[sw] {
            ObjectState::Switch { ref reserved, .. } => {
                if *reserved.get() {
                    return Some(reserved.event());
                }
            }
            _ => panic!("Not a switch"),
        }
    }

    None
}

fn allocate_resources(r: &Route, infrastructure: &mut Infrastructure, scheduler: &mut Scheduler) {
    for s in r.sections.iter() {
        match infrastructure.state[*s] {
            ObjectState::TVDSection { ref mut reserved, .. } => reserved.set(scheduler, true),
            _ => panic!("Not a TVD"),
        };
    }

    for &(sw, _pos) in r.switch_positions.iter() {
        match infrastructure.state[sw] {
            ObjectState::Switch { ref mut reserved, .. } => reserved.set(scheduler, true),
            _ => panic!("Not a switch"),
        }
    }
}


fn movable_events(r: &Route, sim: &mut Sim) -> Vec<EventId> {
    let throw = r.switch_positions
        .iter()
        .filter_map(|&(sw, pos)| {
            match sim.world.state[sw] {
                ObjectState::Switch { ref position, ref mut throwing, .. } => {
                    if position.get() != &Some(pos) && !throwing.is_some() {
                        Some((sw, pos))
                    } else {
                        None
                    }
                }
                _ => panic!("Not a switch"),
            }
        })
        .collect::<Vec<_>>();

    for (sw, pos) in throw {
        sim.start_process(Box::new(MoveSwitch {
            sw: sw,
            pos: pos,
            state: false,
        }));
    }

    r.switch_positions
        .iter()
        .filter_map(|&(sw, _pos)| {
            match sim.world.state[sw] {
                ObjectState::Switch { ref throwing, .. } => *throwing,
                _ => panic!("Not a switch"),
            }
        })
        .collect::<Vec<_>>()

}

impl<'a> Process<Infrastructure<'a>> for ActivateRoute {
    fn resume(&mut self, sim: &mut Sim) -> ProcessState {
        if let ActivateRouteState::Allocate = self.state {
            match unavailable_resource(&self.route, &sim.world) {
                Some(ev) => return ProcessState::Wait(SmallVec::from_slice(&[ev])),
                None => {
                    allocate_resources(&self.route, &mut sim.world, &mut sim.scheduler);
                    self.state = ActivateRouteState::Move;
                }
            }
        }

        // TODO smallvec
        let wait_move = movable_events(&self.route, sim);
        if !wait_move.is_empty() {
            return ProcessState::Wait(wait_move.into());
        }

        // Set the signal to green
        match sim.world.state[self.route.signal] {
            ObjectState::Signal { ref mut authority } => {
                authority.set(&mut sim.scheduler, Some(self.route.length))
            }
            _ => panic!("Not a signal"),
        }

        sim.start_process(Box::new(CatchSignal {
            signal: self.route.signal,
            tvd: self.route.signal_trigger,
            state: CatchSignalState::Start,
        }));

        for release in self.route.releases.iter() {
            sim.start_process(Box::new(ReleaseRoute {
                trigger: release.trigger,
                resources: release.resources.clone().to_vec(),
                state: ReleaseRouteState::Start,
            }));
        }

        ProcessState::Finished
    }
}

enum CatchSignalState {
    Start,
    AwaitTrigger,
}

struct CatchSignal {
    tvd: ObjectId,
    signal: ObjectId,
    state: CatchSignalState,
}

impl<'a> Process<Infrastructure<'a>> for CatchSignal {
    fn resume(&mut self, sim: &mut Sim) -> ProcessState {
        match self.state {
            CatchSignalState::Start => {
                let event = match sim.world.state[self.tvd] {
                    ObjectState::TVDSection { ref mut occupied, .. } => occupied.event(),
                    _ => panic!("Not a TVD section"),
                };
                self.state = CatchSignalState::AwaitTrigger;
                ProcessState::Wait(SmallVec::from_slice(&[event]))
            }
            CatchSignalState::AwaitTrigger => {
                match sim.world.state[self.signal] {
                    ObjectState::Signal { ref mut authority } => {
                        authority.set(&mut sim.scheduler, None)
                    }
                    _ => panic!("Not a signal"),
                };
                ProcessState::Finished
            }
        }
    }
}

enum ReleaseRouteState {
    Start,
    AwaitEntry,
    AwaitExit,
}

struct ReleaseRoute {
    trigger: ObjectId,
    resources: Vec<ObjectId>,
    state: ReleaseRouteState,
}

impl<'a> Process<Infrastructure<'a>> for ReleaseRoute {
    fn resume(&mut self, sim: &mut Sim) -> ProcessState {
        let event = match sim.world.state[self.trigger] {
            ObjectState::TVDSection { ref mut occupied, .. } => occupied.event(),
            _ => panic!("Not a TVD section"),
        };

        match self.state {
            ReleaseRouteState::Start => {
                self.state = ReleaseRouteState::AwaitEntry;
                ProcessState::Wait(SmallVec::from_slice(&[event]))
            }
            ReleaseRouteState::AwaitEntry => {
                self.state = ReleaseRouteState::AwaitExit;
                ProcessState::Wait(SmallVec::from_slice(&[event]))
            }
            ReleaseRouteState::AwaitExit => {
                for obj in &self.resources {
                    match sim.world.state[*obj] {
                        ObjectState::TVDSection { ref mut reserved, .. } |
                        ObjectState::Switch { ref mut reserved, .. } => {
                            reserved.set(&mut sim.scheduler, false)
                        }
                        _ => panic!("Not a resource"),
                    };;
                }
                ProcessState::Finished
            }
        }
    }
}
