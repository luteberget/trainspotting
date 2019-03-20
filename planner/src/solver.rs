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
    progress_before: HashMap<PartialRouteId, Bool>,
    born_before:     Bool,
    visit_before:    Vec<Bool>,
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

            let schedule = mk_schedule(&states, &model);

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
            overlap_choice: Symbolic::new(s, (0..r.conflicts.len()).collect())
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


    //
    // Don't swing the overlap unless it is needed (someone need to use 
    // a conflicting route)
    //
    if let Some(prev_state) = prev_state {
        for (rn,r) in partial_routes.iter() {
            let overlaps = r.overlap_choice.domain().cloned().collect::<Vec<_>>();
            let overlap_pairs = overlaps.iter().flat_map(|a| overlaps.iter().map(move |b| (*a,*b)))
                .filter(|(a,b)| a != b).collect::<Vec<_>>();

            for (ol1,ol2) in overlap_pairs {

                let mut clause = Vec::new();

                clause.push(r.occupation.has_value(&None));                             // Is active
                clause.push(prev_state.partial_routes[rn].occupation.has_value(&None)); // Was active

                clause.push(!prev_state.partial_routes[rn].overlap_choice.has_value(&ol1));  // Switched 
                clause.push(!r.overlap_choice.has_value(&ol2));                              // from ol1 to ol2.

                let conflict_diff = problem.partial_routes[rn].conflicts[ol1].difference(
                                   &problem.partial_routes[rn].conflicts[ol2]);
                for (conflicting_route, conflicting_overlap) in conflict_diff {
                    let using_conflicting = s.and_literal(vec![
                            !partial_routes[conflicting_route].occupation.has_value(&None),
                             partial_routes[conflicting_route].overlap_choice.has_value(conflicting_overlap)]);
                    clause.push(using_conflicting);
                }

                s.add_clause(clause);
            }
        }
    }

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
    if let Some(prev_state) = prev_state {
        for (train_id, train) in problem.trains.iter() {
            for (rn,r) in partial_routes.iter() {

                let freeable_paths = is_freeable_after(
                    s, &problem.partial_routes[rn], &problem.partial_routes, 
                    &prev_state.partial_routes, *train_id, train.length);
                let free = s.or_literal(freeable_paths);

                let was_occupied = prev_state.partial_routes[rn].occupation.has_value(&Some(*train_id));
                let  is_occupied = r.occupation.has_value(&Some(*train_id));
                Bool::assert_equal_or(s, vec![!was_occupied], &!free, &is_occupied);
            }
        }
    }

    //
    // Train state
    //
    //
    let mut empty_prev_trains = None;
    let prev_trains = if let Some(p) = prev_state { &p.trains } else {
        empty_prev_trains = Some(problem.trains.iter().map(|(train_id,train)| {
            (*train_id, TrainState {
                progress_before: problem.partial_routes.iter().map(|(rn,_)| (*rn,false.into())).collect(),
                born_before: false.into(),
                visit_before: train.visits.iter().map(|_| false.into()).collect(),
            })
        }).collect());

        empty_prev_trains.as_ref().unwrap()
    };

    // Forced progress: 
    // each train needs to allocate unless it has deferred progress,
    // which means that it will allocate something in the future after a conflict has been
    // resolved. That is, it is currently yielding to a train (either another train or itself).

    let mut trains_state = HashMap::new();
    for (train_id, train) in problem.trains.iter() {
        let mut r_pr = HashMap::new();
        for (rn, progress_before) in prev_trains[train_id].progress_before.iter() {
            if let Some(signal) = problem.partial_routes[rn].exit {
                let is_allocated = partial_routes[rn].occupation.has_value(&Some(*train_id));

                let progress_now = problem.partial_routes.iter()
                    .filter(|(rn,r)| r.entry == Some(signal))
                    .map(|(rn,_)| partial_routes[rn].occupation.has_value(&Some(*train_id)));

                let progress_future = s.new_lit();

                let mut alternatives = progress_now.collect::<Vec<_>>();
                alternatives.push(progress_future);
                alternatives.push(!is_allocated);
                s.add_clause(alternatives);

                if let Some(prev_state) = prev_state {
                    let next_routes : Vec<_> = problem.partial_routes.iter()
                        .filter(|(rn,r)| r.entry == Some(signal))
                        .map(|(rn,r)| *rn).collect();
                    let mut conflict_resolved = resolve_conflict_with(s, &problem.partial_routes,
                          &problem.elementary_routes, *train_id, 
                          &prev_state.partial_routes, &partial_routes,
                          &next_routes);

                    conflict_resolved.push(*progress_before);
                    conflict_resolved.push(progress_future);
                    s.add_clause(conflict_resolved);
                }

                r_pr.insert(*rn, !progress_future);
            } else {
                // Boundary exit routes do not require progress
                r_pr.insert(*rn, true.into());
            }
        }


        // Born: each train can only appear once.
        let mut born_now_alternatives = Vec::new();
        for (rn, r) in problem.partial_routes.iter() {
            // Can the train be born here?
            if r.contains_nodes.intersection(&train.visits[0]).nth(0).is_some() {
                born_now_alternatives.push(did_activate(s, rn, train_id));
            } else {
                // Don't go here
                s.add_clause(vec![ !partial_routes[rn].occupation.has_value(&Some(*train_id)) ]);
            }
        }
        let born_before = prev_trains[train_id].born_before;
        let born_now    = s.or_literal(born_now_alternatives);
        let born_future = s.new_lit();
        exactly_one(s, vec![born_before, born_now, born_future]);

        if let Some(prev_state) = prev_state {
            let birth_candidates = problem.partial_routes.iter()
                .filter(|(rn,r)| r.contains_nodes.intersection(&train.visits[0]).nth(0).is_some())
                .map(|(rn,r)| *rn).collect::<Vec<_>>();

            let mut conflict_resolved =  resolve_conflict_with(s, &problem.partial_routes,
                           &problem.elementary_routes, *train_id,
                           &prev_state.partial_routes, &partial_routes,
                           &birth_candidates);

            conflict_resolved.push(!born_now);
            s.add_clause(conflict_resolved);
        }

        // Visits:
        // Each visit has to happen some time.
        let mut train_visit = Vec::new();
        for (i,visit_before) in prev_trains[train_id].visit_before.iter().enumerate() {
            let alternative_nodes  = &problem.trains[train_id].visits[i];
            let alternative_routes = problem.partial_routes.iter()
                .filter(|(rn,r)| r.contains_nodes.intersection(alternative_nodes).nth(0).is_some())
                .map(|(rn,r)| partial_routes[rn].occupation.has_value(&Some(*train_id)));

            let visit_now = s.or_literal(alternative_routes);
            let visit_future = s.new_lit();

            s.add_clause(vec![*visit_before, visit_now, visit_future]);
            train_visit.push(!visit_future);
        }



        trains_state.insert(*train_id, TrainState {
            progress_before: r_pr,
            born_before: !born_future,
            visit_before: train_visit,
        });

    }
    // Visits need to happen in order
    for ord in problem.train_ord.iter() {
        let (t1,v1) = &ord.a;
        let (t2,v2) = &ord.b;

        // if v2 happens now or before, v1 must have happened now of before.
        // !v2_future => v1_before || v1_now
        // !v2_future => !v1_future.

        // This is an unfortunate naming, because visit_before refers to
        // how the next_state will use the value. Actually it contains 
        // !visit_future in this state.
        let v1_future = !trains_state[t2].visit_before[*v2];
        let v2_future = !trains_state[t1].visit_before[*v1];

        s.add_clause(vec![v2_future, !v1_future]);
    }

    State { partial_routes, trains: trains_state }
}

fn exactly_one(s :&mut Solver, v :Vec<Bool>) {
    s.assert_at_most_one(v.iter().cloned());
    s.add_clause(v);
}

fn resolve_conflict_with(s :&mut Solver,
                         problem_partial_routes :&HashMap<PartialRouteId, PartialRoute>,
                         problem_elementary_routes :&Vec<HashSet<PartialRouteId>>,
                         train_id :TrainId,
                         prev_state :&HashMap<PartialRouteId, Occupation>,
                              state :&HashMap<PartialRouteId, Occupation>,
                         candidates :&[PartialRouteId]) -> Vec<Bool> {

    let mut result = Vec::new();

    for next_route in candidates {
        let elementary_route :&HashSet<PartialRouteId> = problem_elementary_routes.iter()
            .find(|x| x.contains(next_route)).unwrap();
        let elementary_and_overlap = elementary_route.iter()
            .flat_map(move |r| (0..problem_partial_routes[r].conflicts.len()).map(move |i| (r,i)));

        for (new_route,new_overlap) in elementary_and_overlap {
            // parts of a new route and overlap which we want to allocate,
            // but have yielded for because of conflict in previous step.

            let mut conflicts :HashSet<(PartialRouteId,Option<OverlapId>)>= HashSet::new();

            // Any conflicting routes with new_route can be used to show yielding.
            for (i,set) in problem_partial_routes[new_route].conflicts.iter().enumerate() {
                for (conflicting_route,conflicting_overlap) in set {
                    conflicts.insert((*conflicting_route, Some(*conflicting_overlap)));
                }
            }

            // The route itself is conflicting with itself, with any overlap.
            conflicts.insert((*new_route, None));

            for (prev_route, opt_prev_overlap) in conflicts {
                let had_conflict_route = !prev_state[&prev_route].occupation.has_value(&None);
                let opt_had_conflict_overlap = opt_prev_overlap
                    .map(|o| prev_state[&prev_route].overlap_choice.has_value(&o)).unwrap_or(true.into());

                let was_allocated = prev_state[new_route].occupation.has_value(&Some(train_id));
                let  is_allocated =      state[new_route].occupation.has_value(&Some(train_id));
                let has_overlap   =      state[new_route].overlap_choice.has_value(&new_overlap);

                result.push(s.and_literal(vec![
                  had_conflict_route, 
                  opt_had_conflict_overlap,
                  !was_allocated,
                  is_allocated,
                  has_overlap]));
            }
        }
    }

    result
}

fn is_freeable_after(s :&mut Solver,
                     r :&PartialRoute, 
                     all_routes :&HashMap<PartialRouteId, PartialRoute>,
                     partial_routes :&HashMap<PartialRouteId, Occupation>, 
                     train_id :TrainId,
                     remaining_length :f32) -> Vec<Bool> {
    match r.exit {
        None => return vec![true.into()],
        Some(signal) => {
            let mut alternatives = Vec::new();
            for (next_route_id,next_route) in all_routes.iter().filter(|(_,r)| r.entry == Some(signal)) {
                let occupied = partial_routes[next_route_id].occupation.has_value(&Some(train_id));
                if next_route.length >= remaining_length {
                    alternatives.push(occupied);
                } else {
                    let nexts = is_freeable_after(s, next_route, all_routes, partial_routes, train_id, 
                                                  remaining_length - next_route.length);
                    let any_next = s.or_literal(nexts);
                    alternatives.push(s.and_literal(vec![ occupied, any_next ]));
                }
            }
            alternatives
        },
    }
}

fn mk_schedule(states :&[State], model :&Model) -> RoutePlan {
    states.iter().map(|state| {
        state.partial_routes.iter().map(|(rn,r)| {
            (*rn, *model.value(&r.occupation))
        }).collect()
    }).collect()
}

fn end_state_condition(state :&State) -> Vec<Bool> {
    let mut condition = Vec::new();
    for (train_id,ts) in state.trains.iter() {
        condition.push(ts.born_before);
        for v in &ts.visit_before { condition.push(*v); }
        for (k,v) in &ts.progress_before { condition.push(*v); }
    }
    condition
}

pub struct Loop {}
fn loop_check(plan :&RoutePlan) -> Result<(), Loop> {
    // TODO
    Ok(())
}

pub struct Repeat {}
fn repeat_check(problem :&Problem, plan :&RoutePlan) -> Result<(), Repeat> {
    // TODO
    Ok(())
}


fn disallow_loop(s :&mut Solver, loop_ :&Loop) {
}

fn disallow_repeat(s :&mut Solver, repeat :&Repeat) {
}

fn disallow_schedule(s :&mut Solver, plan :&RoutePlan) {
}



