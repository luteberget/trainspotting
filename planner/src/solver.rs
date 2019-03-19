use rolling::input::staticinfrastructure::{NodeId};
use minisat::{*, symbolic::*};
use std::collections::HashMap;
use log::{debug};

use crate::movement::*;
use crate::problem::*;

// State management types (private)

type RoutePartId = usize;
type TrainId = usize;
type OverlapId = usize;

struct State {
    occupation: HashMap<RoutePartId, Occupation>,
    trains:     HashMap<TrainId,     TrainState>,
}

struct Occupation {
    occupation:     Symbolic<Option<TrainId>>,
    overlap_choice: Symbolic<OverlapId>,
}

struct TrainState {
    progress_before: Vec<(RoutePartId, Lit)>,
    born_before:     Lit,
    visit_before:    Vec<(Vec<NodeId>, Lit)>,
}



pub fn plan<F : Fn(&RoutePlan) -> bool>(config :&Config, problem :&Problem, test :F) -> Option<RoutePlan> {
    let mut s = Solver::new();
    let mut failed_steps = None;

    debug!("Adding state {}", state.len());
    let mut states = Vec::new();
    states.push(mk_state(&mut s, problem));

    let test_solution = |model| {
        // UNSAT = add new state or abort
        if let Err(()) = model { 
            let increase = match failed_steps {
                None => state.len() < config.n_before,
                Some(f) => f < config.n_after,
            };

            return if increase { Action::AddState } else { Action::Fail };
        };

        // Make schedule
        let schedule = get_schedule(problem, model);

        // LOOP CHECK
        if let Err(loop_) = loop_check(schedule) {
            return Action::RemoveLoop(loop_);
        }

        // REPEAT CHECK
        if let Err(repeat) = repeat_check(schedule) {
            return Action::RemoveRepeat(repeat);
        }

        // TEST FUNCTION
        if test(schedule) {
             Action::Finished(schedule)
        } else {
            Action::Next
        }
    };

    loop {
        debug!("Solving for n_states={}", state.len());
        enum Action { AddState, Fail }


        match test_solution() {
            Action::Fail => {
                debug!("Solver failed.");
                break;
            },
            Action::AddState => {
                debug!("Add new state (start at the top of the loop again.");
                states.push(mk_state(&mut s, problem));
            },
            Action::RemoveSchedule(schedule) => {
                delete(schedule);
            },
            Action::RemoveLoop(loop_) => {
            },
        }
    }
}

fn mk_state(s :&mut Solver, problem :&Problem) -> State {
}




