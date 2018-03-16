extern crate smallvec;
extern crate ordered_float;

mod simulation;
use simulation::*;

use smallvec::SmallVec;

struct Car {
    name :usize,
    state: CarState,
}
enum CarState { Starting, Running, Stopping }

impl Process<f64> for Car {
    fn resume(&mut self, sim: &mut Simulation<f64>) -> ProcessState {
        match self.state {
            CarState::Starting => {
                println!("Starting the car {} @{}.", self.name, sim.time);
                sim.world += 15.19;

                self.state = CarState::Running;
                ProcessState::Wait(SmallVec::from_slice(&[sim.create_timeout(5.0)]))
            },
            CarState::Running => {
                sim.world += 31.12;
                self.state = CarState::Stopping;
                if self.name < 50 {
                    println!("Waiting for futher car {} @{}", self.name, sim.time);
                    ProcessState::Wait(SmallVec::from_slice(&[sim.start_process(Box::new(Car { 
                        name: self.name + 10, state: CarState::Starting }))]))
                } else {
                println!("Not waiting for futher car {} @{}.", self.name, sim.time);
                    ProcessState::Finished
                }
            },
            CarState::Stopping => {
                sim.world += 50.123;
                println!("Finished after waiting {} @{}", self.name, sim.time);
                ProcessState::Finished
            }
        }
    }
}

fn main() {
    let mut sim = Simulation::new(0.0);
    let car = Box::new(Car { name: 10, state: CarState::Starting });
    sim.start_process(car);
    sim.advance_by(100.0);
    println!("Total gasoline expense: {}", sim.world);
}
