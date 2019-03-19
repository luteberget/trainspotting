use rolling::input::staticinfrastructure::{NodeId};
use minisat::{*, symbolic::*};
use std::collections::HashMap;
use log::{debug};

use crate::movement::*;
use crate::problem::*;

// State management types (private)


struct State {
    partial_routes: HashMap<PartialRouteId, Occupation>,
    trains: HashMap<TrainId, TrainState>,
}

struct Occupation {
    occupation:     Symbolic<Option<TrainId>>,
    overlap_choice: Symbolic<OverlapId>,
}

struct TrainState {
    progress_before: Vec<(PartialRouteId, Lit)>,
    born_before:     Lit,
    visit_before:    Vec<(Vec<NodeId>, Lit)>,
}



pub fn plan<F : Fn(&RoutePlan) -> bool>(config :&Config, problem :&Problem, test :F) -> Option<RoutePlan> {
    let mut s = Solver::new();
    let mut failed_steps = None;

    debug!("Adding initial state");
    let mut states = Vec::new();
    states.push(mk_state(&mut s, problem));

    loop {
        debug!("Solving with n={}.", states.len());
        if let Ok(model) = s.solve_under_assumptions(
            end_state_condition(states.last().unwrap())) {

            let schedule = mk_schedule(&model);

            // Built-in checks:
            // Loop check
            if let Err(loop_) = loop_check(&schedule) {
                debug!("Loop check failed. Removing loop.");
                disallow_loop(&mut s, &loop_);
                continue;
            } 
            debug!("Loop check succeeded.");
            // Repeat check
            if let Err(repeat) = repeat_check(&problem, &schedule) {
                debug!("Repeat check failed. Removing.");
                disallow_repeat(&mut s, &repeat);
                continue;
            }
            debug!("Repeat check succeeded.");

            // User check: e.g. simulation
            if !test(&schedule) {
                debug!("User check failed.");
                disallow_schedule(&mut s, &schedule);
                continue;
            }
            debug!("User check suceeded, returning resulting plan.");
            failed_steps = Some(0);

            // Everything is ok
            break Some(schedule);

        } else {
            debug!("No more plans for n={}", states.len());
            let increase = match failed_steps {
                None => states.len() < config.n_before,
                Some(f) => f < config.n_after,
            };
            failed_steps = failed_steps.map(|x| x+1);
            if increase {
                debug!("Adding new state.");
                states.push(mk_state(&mut s, problem));
                continue;
            } else {
                break None;
            };
        }
    }
}


fn mk_state(s :&mut Solver, problem :&Problem) -> State {
    use std::iter::{once};

    // Each partial route can be occupied by a train,
    // and if so, there is (might be) a choice of overlap.
    let partial_routes :HashMap<PartialRouteId, Occupation> = problem.partial_routes.iter()
        .map(|(name,r)| (*name,Occupation {
            occupation: Symbolic::new(s, 
              once(None).chain((0..problem.trains.len()).map(|x| Some(x))).collect()),
            overlap_choice: Symbolic::new(s, (0..r.conflicts.len()-1).collect())
        })).collect();

    // Conflicting routes are excluded
    for (r1n,r1) in partial_routes.iter() {
        for overlap in r1.overlap_choice.domain() {
            for (confl_r, confl_overlap) in &problem.partial_routes[r1n].conflicts[*overlap] {
                let r2 = &partial_routes[confl_r];
                s.add_clause(vec![
                    r1.occupation.has_value(&None),
                    !r1.overlap_choice.has_value(&overlap),
                    r2.occupation.has_value(&None),
                    !r2.overlap_choice.has_value(&confl_overlap)]);
                    
            }
        }
    }


    let trains = unimplemented!();

    State { partial_routes, trains }
}

fn mk_schedule(model :&Model) -> RoutePlan {
    unimplemented!()
}

fn end_state_condition(state :&State) -> Vec<Bool> {
    unimplemented!()
}

pub struct Loop {}
fn loop_check(plan :&RoutePlan) -> Result<(), Loop> {
    unimplemented!()
}

pub struct Repeat {}
fn repeat_check(problem :&Problem, plan :&RoutePlan) -> Result<(), Repeat> {
    unimplemented!()
}


fn disallow_loop(s :&mut Solver, loop_ :&Loop) {
}

fn disallow_repeat(s :&mut Solver, repeat :&Repeat) {
}

fn disallow_schedule(s :&mut Solver, plan :&RoutePlan) {
}



