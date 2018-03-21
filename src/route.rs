use simulation::{Simulation, Process, ProcessState};
use smallvec::SmallVec;
use staticinfrastructure::*;
use infrastructure::*;

enum ActivateRouteState {
    Allocate, // Waiting for resources
    Move,     // Waiting for movable elements
}

struct ActivateRoute {
    route :Route,
    state :ActivateRouteState,
}

fn resources_available(r :&Route, infrastructure :&Infrastructure) -> bool {
    false
}

impl Process<Infrastructure> for ActivateRoute {
    fn resume(&mut self, sim: &mut Simulation<Infrastructure>) -> ProcessState {
        if let ActivateRouteState::Allocate = self.state {
            if resources_available(&self.route, &sim.world) {
                self.state = ActivateRouteState::Move;
            } else {
                return ProcessState::Wait(SmallVec::new());
            }
        }

        if let ActivateRouteState::Move = self.state {
        }

        // Set the signal to green
        match sim.world.state[self.route.signal] {
            ObjectState::Signal { ref mut authority } => authority.set(&mut sim.scheduler, Some(self.route.length)),
            _ => panic!("Not a signal"),
        }

        sim.start_process(Box::new(CatchSignal { 
            signal: self.route.signal,
            tvd: self.route.first_trigger,
            state: CatchSignalState::Start }));

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

enum CatchSignalState { Start, AwaitTrigger }

struct CatchSignal {
    tvd: ObjectId,
    signal: ObjectId,
    state: CatchSignalState,
}

impl Process<Infrastructure> for CatchSignal {
    fn resume(&mut self, sim :&mut Simulation<Infrastructure>) -> ProcessState {
        match self.state {
            CatchSignalState::Start => {
                let event = match sim.world.state[self.tvd] {
                    ObjectState::TVDSection { ref mut occupied, .. } => occupied.event(),
                    _ => panic!("Not a TVD section"),
                };
                self.state = CatchSignalState::AwaitTrigger;
                ProcessState::Wait(SmallVec::from_slice(&[event]))
            },
            CatchSignalState::AwaitTrigger => {
                match sim.world.state[self.signal] {
                    ObjectState::Signal { ref mut authority } => authority.set(&mut sim.scheduler, None),
                    _ => panic!("Not a signal"),
                };
                ProcessState::Finished
            },
        }
    }
}

enum ReleaseRouteState { Start, AwaitEntry, AwaitExit }

struct ReleaseRoute {
    trigger: ObjectId,
    resources: Vec<ObjectId>,
    state: ReleaseRouteState,
}

impl Process<Infrastructure> for ReleaseRoute {
    fn resume(&mut self, sim :&mut Simulation<Infrastructure>) -> ProcessState {
        let event = match sim.world.state[self.trigger] {
            ObjectState::TVDSection { ref mut occupied, .. } => occupied.event(),
            _ => panic!("Not a TVD section"),
        };

        match self.state {
            ReleaseRouteState::Start => {
                self.state = ReleaseRouteState::AwaitEntry;
                ProcessState::Wait(SmallVec::from_slice(&[event]))
            },
            ReleaseRouteState::AwaitEntry => {
                self.state = ReleaseRouteState::AwaitExit;
                ProcessState::Wait(SmallVec::from_slice(&[event]))
            },
            ReleaseRouteState::AwaitExit => {
                for obj in self.resources.iter() {
                    match sim.world.state[*obj] {
                       ObjectState:: TVDSection { ref mut reserved, .. } => reserved.set(&mut sim.scheduler, false),
                        ObjectState::Switch { ref mut reserved, ..} => reserved.set(&mut sim.scheduler, false),
                        _ => panic!("Not a resource"),
                    };;
                }
                ProcessState::Finished
            },
        }
    }
}
