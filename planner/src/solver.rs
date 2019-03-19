use rolling::input::staticinfrastructure::{NodeId};
use minisat::{*, symbolic::*};
use std::collections::{HashMap, HashSet};
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
    states.push(mk_state(&mut s, None, problem));

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
                states.push(mk_state(&mut s, states.last(), problem));
                continue;
            } else {
                break None;
            };
        }
    }
}


fn mk_state(s :&mut Solver, prev_state :Option<&State>, problem :&Problem) -> State {
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
    
    // At most one alternative route is taken. 
    // This means that 
    //   (1) a train at a signal can only take one of the routes starting in that signal
    //       in the same step. This should already be disallowed by conflicting routes,
    //       but in case it is not, this constraint is needed to ensure train consistency.
    //   (2) a train entering from a border can only do so at one place, because all of the
    //       border entry train routes share the None value for their entry field.
    //
    for entry in problem.partial_routes.iter().map(|(_,r)| r.entry).collect::<HashSet<_>>() {
        for (train_id,train) in &problem.trains {
            s.assert_at_most_one(problem.partial_routes.iter()
                     .filter(|(_,r)| r.entry == entry)
                     .map(|(i,_)| partial_routes[i].occupation.has_value(&Some(*train_id))));
        }
    }

    let did_activate = |s :&mut Solver, r :&PartialRouteId, t :&TrainId| 
        s.and_literal(vec![       partial_routes[r].occupation.has_value(&Some(*t)),
            prev_state.map(|p| !p.partial_routes[r].occupation.has_value(&Some(*t)))
              .unwrap_or(true.into())]);

    //
    // Partial routes are allocated together.
    //
    for (train_id,train) in problem.trains.iter() {
        for route_set in problem.elementary_routes.iter() {
            for (r1,r2) in route_set.iter().zip(route_set.iter().skip(1)) {

                let r1_activated = did_activate(s,r1,train_id);
                let r2_activated = did_activate(s,r2,train_id);
                s.equal(&r1_activated, &r2_activated);
            }
        }
    }

    //
    // One of the conflict sets (overlap choices) might be excluded on allocation,
    // which would then be the conflict set after timeout.
    // 
    for (train_id,train) in problem.trains.iter() {
        for (rn,r) in partial_routes.iter() {
            if let Some(confl) = problem.partial_routes[rn].wait_conflict {
                let activated = did_activate(s, rn, train_id);
                s.add_clause(vec![ !activated, !r.overlap_choice.has_value(&confl)]);
            }
        }
    }


    // TODO don't swing the overlap unless conflict

    // Route allocation constraints:
    //
    // New allocations must have a preceding route active in the same step.
    // Trains cannot swap places in one step.
    //
    for (train_id, train) in problem.trains.iter() {
        for (rn,r) in partial_routes.iter() {
            if let Some(signal) = problem.partial_routes[rn].entry {
                let was_allocated = prev_state.map(|p|
                   p.partial_routes[rn].occupation.has_value(&Some(*train_id)));
                let not_allocated = Some(!r.occupation.has_value(&Some(*train_id)));
                let prev_is_allocated = problem.partial_routes.iter()
                    .filter(|(_,r)| r.exit == Some(signal))
                    .map(|(id,_)| Some(partial_routes[id].occupation.has_value(&Some(*train_id))));

                s.add_clause(once(not_allocated).chain(once(was_allocated)).chain(prev_is_allocated)
                             .filter_map(|x| x));
            }
        }
    }

    // Route free constraints:
    // 
    // Can only free routes when train has sufficient length allocated ahead of 
    // the route.  If this is the case, then the route _must_ also be freed to 
    // ensure maximal progress.
    //
    if let Some(prev) = prev_state {
        for (train_id, train) in problem.trains.iter() {
            for (rn,r) in partial_routes.iter() {

                //Bool::assert_equal_or(s, vec![

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



