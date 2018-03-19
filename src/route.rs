use simulation::{Simulation, Process, ProcessState};
use railway::*;
use smallvec::SmallVec;

enum ActivateRouteState {
    Allocate, // Allocate resources
    Move, // Movable elements being put into place
}

struct ActivateRoute {
    route :Route,
    state :ActivateRouteState,
}

fn resources_available(r :&Route, world :&Railway) -> bool {
    false
}

impl Process<Railway> for ActivateRoute {
    fn resume(&mut self, sim: &mut Simulation<Railway>) -> ProcessState {
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
        match sim.world.objects[self.route.signal] {
            Object::Signal { ref mut authority } => authority.set(&mut sim.scheduler, Some(self.route.length)),
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

impl Process<Railway> for CatchSignal {
    fn resume(&mut self, sim :&mut Simulation<Railway>) -> ProcessState {
        use railway::Object::{ TVDSection, Signal };
        let ref mut objects = sim.world.objects;
        let ref mut scheduler = sim.scheduler;

        match self.state {
            CatchSignalState::Start => {
                let event = match objects[self.tvd] {
                    TVDSection { ref mut occupied, .. } => occupied.event(),
                    _ => panic!("Not a TVD section"),
                };
                self.state = CatchSignalState::AwaitTrigger;
                ProcessState::Wait(SmallVec::from_slice(&[event]))
            },
            CatchSignalState::AwaitTrigger => {
                match objects[self.signal] {
                    Signal { ref mut authority } => authority.set(scheduler, None),
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

impl Process<Railway> for ReleaseRoute {
    fn resume(&mut self, sim :&mut Simulation<Railway>) -> ProcessState {
        use railway::Object::{ TVDSection, Signal, Switch };
        let ref mut objects = sim.world.objects;
        let ref mut scheduler = sim.scheduler;

        let event = match objects[self.trigger] {
            TVDSection { ref mut occupied, .. } => occupied.event(),
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
                    match objects[*obj] {
                        TVDSection { ref mut reserved, .. } => reserved.set(scheduler, false),
                        Switch { ref mut reserved, ..} => reserved.set(scheduler, false),
                        _ => panic!("Not a resource"),
                    };;
                }
                ProcessState::Finished
            },
        }
    }
}
