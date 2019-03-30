use rolling::input::staticinfrastructure::{NodeId};
use minisat::{*, symbolic::*, unary::*};
use std::collections::{HashMap, HashSet};
use log::{debug};

use crate::input::*;

// State management types (private)


struct State {
    infrastructure: HashMap<PartialRouteId, InfrastructureState>,
    trains: HashMap<TrainId, TrainsState>,
}

struct InfrastructureState {
    occupation:     Symbolic<Option<TrainId>>,
    overlap_choice: Symbolic<OverlapId>,
}

#[derive(Debug, Clone)]
struct TrainsState {
    progress_before: HashMap<PartialRouteId, Bool>,
    born_before:     Bool,
    visit_before:    Vec<Bool>,
}


pub struct SignalOptimizer<'a> {
    solver :Solver,
    active_signals :HashMap<SignalId, Bool>,
    states :Vec<Vec<State>>,
    infrastructure :&'a Infrastructure,
    usages :&'a [Usage],
    current_signals :Option<HashSet<SignalId>>,
    last_signal_set_lit :Option<Bool>,
}

impl<'a> SignalOptimizer<'a> {
    pub fn new(inf :&'a Infrastructure, usages :&'a [Usage]) -> SignalOptimizer<'a> {
        let mut solver = Solver::new();

        use std::iter::once;
        let all_signals = inf.partial_routes.iter()
            .flat_map(|(_,r)| once(r.entry).chain(once(r.exit)))
            .filter(|e| e != &SignalId::Boundary).collect::<HashSet<_>>();
        let active_signals :HashMap<SignalId,Bool>
            = all_signals.into_iter().map(|x| (x, solver.new_lit())).collect();


        let mut s = SignalOptimizer {
            solver,
            active_signals,
            states: (0..usages.len()).map(|_| vec![]).collect(),
            infrastructure: inf,
            usages,
            current_signals: None,
            last_signal_set_lit: None,
        };
        //
        // add the first state
        s.add_state();
        s
    }

    pub fn add_state(&mut self) {
        for (usage_idx,usage) in self.usages.iter().enumerate() {
            let prev_state = self.states[usage_idx].last();
            let new_state = mk_state(&mut self.solver, prev_state, 
                       &self.infrastructure, usage,
                       Some(&self.active_signals));
            self.states.get_mut(usage_idx).unwrap().push(new_state);
        }
    }

    pub fn next_signal_set(&'a mut self) -> Option<SignalSet<'a>> {
        use std::iter::once;
        // TODO parameter
        let relative_cost :usize = 3;

        // If we have already found a set of signals, exclude that one
        if let Some(prev) = self.last_signal_set_lit.take() {
            self.solver.add_clause(once(!prev));
        }

        // see if we can solve it now
        'next: loop {
            let all_end_state_conditions = self.states.iter()
                .flat_map(|i| i.last().unwrap().trains.clone()) 
                .collect::<HashMap<TrainId, TrainsState>>();

            let assumption = end_state_condition(&all_end_state_conditions);
            let (n_signals, n_detectors) = 
                if let Ok(model) = self.solver.solve_under_assumptions(assumption) {
                // the number of states (and the maximal design) works.
                // Now it is time to optimize for the number of signals.

                let n_signals = self.active_signals.iter()
                    .filter(|(s,_)| if let SignalId::ExternalId(_) = s 
                            { true } else { false })
                    .map(|(_,v)| model.value(v)).count();

                let n_detectors = self.active_signals.iter()
                    .filter(|(s,_)| if let SignalId::Anonymous(_) = s 
                            { true } else { false })
                    .map(|(_,v)| model.value(v)).count();

                (n_signals,n_detectors)
            } else {
                self.add_state();
                continue 'next;
            };


            // try to optimize the number of signals
            // first count the number of signals and detectors and make
            // a truncated unary number containing the relative values

            let signal_cost = self.active_signals.iter()
                .filter(|(s,_)| if let SignalId::ExternalId(_) = s 
                        { true } else { false })
                .map(|(_,v)| Unary::from_bool(*v).mul_const(relative_cost));

            let detector_cost = self.active_signals.iter()
                .filter(|(s,_)| if let SignalId::Anonymous(_) = s 
                        { true } else { false })
                .map(|(_,v)| Unary::from_bool(*v));


            let init_cost :usize = n_signals*relative_cost + n_detectors;
            let costs = signal_cost.chain(detector_cost).collect::<Vec<Unary>>();
            let sum_cost = Unary::sum_truncate(&mut self.solver, costs, init_cost+1);



            let bound : usize = {
                // optimize
                let (mut lo, mut hi) :(usize,usize) = (0,init_cost);

                'minimize: loop {
                    let mid : usize = (lo + hi)/2;
                    println!("In optimize_signals: Solving with mid={}", mid);
                    if let Ok(model) = self.solver.solve_under_assumptions(
                            vec![sum_cost.lte_const(mid as isize)]) {
                        // sucess, lower hi bound
                        hi = mid;
                        // TODO make this conditional for later increase 
                        // in the number of signals.
                        self.solver.add_clause(vec![sum_cost.lte_const(mid as isize)]);
                    }  else {
                        lo = mid;
                    }

                    if lo >= hi {
                        break 'minimize;
                    }
                };

                lo
            };

                // Get model
            if let Ok(model) = self.solver.solve_under_assumptions(
                    vec![sum_cost.lte_const(bound as isize)]) {

                let signals : HashMap<SignalId,bool> = self.active_signals.iter()
                    .map(|(sig,val)| (*sig, model.value(val))).collect();
                let this_set_lit = self.solver.new_lit();
                for (sig,v) in self.active_signals.iter() {
                    // this_set_lit implies that v has its current value
                    self.solver.add_clause(vec![!this_set_lit,
                                           if signals[sig] { *v } else { !*v }]);
                }

                self.last_signal_set_lit = Some(this_set_lit);
                return Some(SignalSet { 
                    solver: &mut self.solver,
                    states: &self.states,
                    usages: self.usages,
                    this_set_lit: this_set_lit,
                    signals: signals });
            } else {
                    //return Err(format!("In optimize_signals: SAT query failed unexpectedly."));
                    println!("In optimize_signals: SAT query failed unexpectedly.");
                    return None;
            };

        }
    }
}

pub struct SignalSet<'a> {
    solver :&'a mut Solver, 
    states :&'a Vec<Vec<State>>,
    usages :&'a [Usage],
    this_set_lit :Bool,
    signals :HashMap<SignalId, bool>
}

impl<'a> SignalSet<'a> {

    pub fn get_signals(&self) -> &HashMap<SignalId, bool> {
        &self.signals
    }

    pub fn get_dispatches(&mut self) -> Vec<Vec<RoutePlan>> {
        self.usages.iter().enumerate()
            .map(|(i,u)| self.get_usage_dispatch(u, &self.states[i])).collect()
    }

    fn get_usage_dispatch(&mut self, usage :&Usage, states :&[State]) -> Vec<RoutePlan> {
        let usage_lit = self.solver.new_lit();
        let mut results = Vec::new();
        while let Ok(model) = self.solver.solve_under_assumptions(vec![self.this_set_lit, usage_lit]) {
            let schedule = mk_schedule(states, &model);
            disallow_schedule(&mut self.solver, vec![usage_lit], states, &schedule);
            results.push(schedule);
        }

        results
    }
}


pub fn plan<F : Fn(&RoutePlan) -> bool>(config :&Config, 
                                        infrastructure :&Infrastructure,
                                        usage :&Usage,
                                        test :F) -> Option<RoutePlan> {
    let mut s = Solver::new();
    let mut failed_steps = None;


    println!("Adding initial state");
    let mut states = Vec::new();
    states.push(mk_state(&mut s, None, infrastructure, usage, None));

    loop {
        println!("Solving with n={}.", states.len());
        if let Ok(model) = s.solve_under_assumptions(
            end_state_condition(&states.last().unwrap().trains)) {

            let schedule = mk_schedule(&states, &model);

            // Built-in checks:
            // Loop check
            if let Err(loop_) = loop_check(&schedule) {
                println!("Loop check failed. Removing loop.");
                disallow_loop(&mut s, &loop_);
                continue;
            } 
            println!("Loop check succeeded.");
            // Repeat check
            if let Err(repeat) = repeat_check(&infrastructure, &schedule) {
                println!("Repeat check failed. Removing.");
                disallow_repeat(&mut s, &repeat);
                continue;
            }
            println!("Repeat check succeeded.");

            // User check: e.g. simulation
            if !test(&schedule) {
                println!("User check failed.");
                disallow_schedule(&mut s, vec![], &states, &schedule);
                continue;
            }
            println!("User check suceeded, returning resulting plan.");
            failed_steps = Some(0);

            // Everything is ok
            break Some(schedule);

        } else {
            println!("No more plans for n={}", states.len());
            let increase = match failed_steps {
                None => states.len() < config.n_before,
                Some(f) => f < config.n_after,
            };
            failed_steps = failed_steps.map(|x| x+1);
            if increase {
                println!("Adding new state.");
                states.push(mk_state(&mut s, states.last(), infrastructure, usage, None));
                continue;
            } else {
                break None;
            };
        }
    }
}


fn mk_state(s :&mut Solver, 
            prev_state :Option<&State>, 
            infrastructure :&Infrastructure,
            usage :&Usage,
            active_signals: Option<&HashMap<SignalId, Bool>>,
            ) -> State  {
    use std::iter::{once};

    // Each partial route can be occupied by a train,
    // and if so, there is (might be) a choice of overlap.
    let inf_state :HashMap<PartialRouteId, InfrastructureState> = infrastructure.partial_routes.iter()
        .map(|(name,r)| (*name,InfrastructureState {
            occupation: Symbolic::new(s, 
              once(None).chain((0..usage.trains.len()).map(|x| Some(x))).collect()),
            overlap_choice: Symbolic::new(s, (0..r.conflicts.len()).collect())
        })).collect();

    // Conflicting routes are excluded
    for (r1n,r1) in inf_state.iter() {
        for overlap in r1.overlap_choice.domain() {
            for (confl_r, confl_overlap) in &infrastructure.partial_routes[r1n].conflicts[*overlap] {
                let r2 = &inf_state[confl_r];
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
    for entry in infrastructure.partial_routes.iter().map(|(_,r)| r.entry).collect::<HashSet<_>>() {
        for (train_id,train) in &usage.trains {
            s.assert_at_most_one(infrastructure.partial_routes.iter()
                     .filter(|(_,r)| r.entry == entry)
                     .map(|(i,_)| inf_state[i].occupation.has_value(&Some(*train_id))));
        }
    }

    let did_activate = |s :&mut Solver, r :&PartialRouteId, t :&TrainId| 
        s.and_literal(vec![            inf_state[r].occupation.has_value(&Some(*t)),
            prev_state.map(|p| !p.infrastructure[r].occupation.has_value(&Some(*t)))
              .unwrap_or(true.into())]);

    let did_deactivate = |s :&mut Solver, r :&PartialRouteId, t :&TrainId| 
        s.and_literal(vec![          !inf_state[r].occupation.has_value(&Some(*t)),
            prev_state.map(|p| p.infrastructure[r].occupation.has_value(&Some(*t)))
              .unwrap_or(false.into())]);

    //
    // Partial routes are allocated together.
    //
    for (train_id,train) in usage.trains.iter() {
        for route_set in infrastructure.elementary_routes.iter() {
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
    for (train_id,train) in usage.trains.iter() {
        for (rn,r) in inf_state.iter() {
            if let Some(confl) = infrastructure.partial_routes[rn].wait_conflict {
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
        for (rn,r) in inf_state.iter() {
            let overlaps = r.overlap_choice.domain().cloned().collect::<Vec<_>>();
            let overlap_pairs = overlaps.iter().flat_map(|a| overlaps.iter().map(move |b| (*a,*b)))
                .filter(|(a,b)| a != b).collect::<Vec<_>>();

            for (ol1,ol2) in overlap_pairs {

                let mut clause = Vec::new();

                clause.push(r.occupation.has_value(&None));                             // Is active
                clause.push(prev_state.infrastructure[rn].occupation.has_value(&None)); // Was active

                clause.push(!prev_state.infrastructure[rn].overlap_choice.has_value(&ol1));  // Switched 
                clause.push(!r.overlap_choice.has_value(&ol2));                              // from ol1 to ol2.

                let conflict_diff = infrastructure.partial_routes[rn].conflicts[ol1].difference(
                                   &infrastructure.partial_routes[rn].conflicts[ol2]);
                for (conflicting_route, conflicting_overlap) in conflict_diff {
                    let using_conflicting = s.and_literal(vec![
                            !inf_state[conflicting_route].occupation.has_value(&None),
                             inf_state[conflicting_route].overlap_choice.has_value(conflicting_overlap)]);
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
    for (train_id, train) in usage.trains.iter() {
        for (rn,r) in inf_state.iter() {
            if !infrastructure.partial_routes[rn].entry.is_boundary() {
                let was_allocated = prev_state.map(|p|
                   p.infrastructure[rn].occupation.has_value(&Some(*train_id)));
                let not_allocated = Some(!r.occupation.has_value(&Some(*train_id)));
                let prev_is_allocated = infrastructure.partial_routes.iter()
                    .filter(|(_,r_prev)| r_prev.exit == infrastructure.partial_routes[rn].entry)
                    .map(|(id,_)| Some(inf_state[id].occupation.has_value(&Some(*train_id)))).collect::<Vec<_>>();

                s.add_clause(once(not_allocated).chain(once(was_allocated)).chain(prev_is_allocated.iter().cloned())
                             .filter_map(|x| x));
            }
        }
    }

    // Signal active: is false if two consecutive partial routes separated by a signal
    // are always activated together:   equal_or(signal_used, did_activate(sig), did_activate(sig))
    if let Some(active_signals) = active_signals {
        let starts_in : HashMap<SignalId, HashSet<PartialRouteId>> = active_signals
            .iter().filter(|(sig,_)| sig.is_signal())
            .map(|(sig,_)| (*sig, infrastructure.partial_routes.iter()
                 .filter(|(_,r)| r.entry == *sig)
                 .map(|(n,_)| *n).collect())).collect();

        let ends_in : HashMap<SignalId, HashSet<PartialRouteId>> = active_signals
            .iter().filter(|(sig,_)| sig.is_signal())
            .map(|(sig,_)| (*sig, infrastructure.partial_routes.iter()
                 .filter(|(_,r)| r.exit == *sig)
                 .map(|(n,_)| *n).collect())).collect();

        for (t,_) in usage.trains.iter() {
            for (sig,is_active) in active_signals.iter().filter(|(sig,_)| sig.is_signal()) {
                // is any route ending in this signal activated?
                let end_activated_alternatives = ends_in[sig].iter()
                    .map(|r| did_activate(s, r, t)).collect::<Vec<_>>();
                let end_activated = s.or_literal(end_activated_alternatives);
                // is any route beginning in this signal activated?
                let start_activated_alternatives = starts_in[sig].iter()
                    .map(|r| did_activate(s, r, t)).collect::<Vec<_>>();
                let start_activated = s.or_literal(start_activated_alternatives);

                Bool::assert_equal_or(s, vec![*is_active], &end_activated, &start_activated);
            }
        }
    }

    // Detector active: ?
    if let Some(active_signals) = active_signals {
        let starts_in : HashMap<SignalId, HashSet<PartialRouteId>> = active_signals
            .iter().filter(|(sig,_)| !sig.is_boundary())
            .map(|(sig,_)| (*sig, infrastructure.partial_routes.iter()
                 .filter(|(_,r)| r.entry == *sig)
                 .map(|(n,_)| *n).collect())).collect();

        let ends_in : HashMap<SignalId, HashSet<PartialRouteId>> = active_signals
            .iter().filter(|(sig,_)| !sig.is_boundary())
            .map(|(sig,_)| (*sig, infrastructure.partial_routes.iter()
                 .filter(|(_,r)| r.exit == *sig)
                 .map(|(n,_)| *n).collect())).collect();

        for (t,_) in usage.trains.iter() {
            for (sig,is_active) in active_signals.iter().filter(|(sig,_)| sig.is_signal()) {

                // is any route ending in this signal deactivated?
                let end_deactivated_alternatives = ends_in[sig].iter()
                    .map(|r| did_deactivate(s, r, t)).collect::<Vec<_>>();
                let end_deactivated = s.or_literal(end_deactivated_alternatives);


                // is any route beginning in this signal deactivated?
                let start_deactivated_alternatives = starts_in[sig].iter()
                    .map(|r| did_deactivate(s, r, t)).collect::<Vec<_>>();
                let start_deactivated = s.or_literal(start_deactivated_alternatives);


                Bool::assert_equal_or(s, vec![*is_active], &end_deactivated, &start_deactivated);
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
        for (train_id, train) in usage.trains.iter() {
            for (rn,r) in inf_state.iter() {

                let freeable_paths = is_freeable_after(
                    s, &infrastructure.partial_routes[rn], &infrastructure.partial_routes, 
                    &prev_state.infrastructure, active_signals, *train_id, train.length);
                let free = s.or_literal(freeable_paths);

                let was_occupied = prev_state.infrastructure[rn].occupation.has_value(&Some(*train_id));
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
        empty_prev_trains = Some(usage.trains.iter().map(|(train_id,train)| {
            (*train_id, TrainsState {
                progress_before: infrastructure.partial_routes.iter().map(|(rn,_)| (*rn,false.into())).collect(),
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
    for (train_id, train) in usage.trains.iter() {
        let mut r_pr = HashMap::new();
        for (rn, progress_before) in prev_trains[train_id].progress_before.iter() {
            if infrastructure.partial_routes[rn].exit.is_boundary() {
                // Boundary exit routes do not require progress
                r_pr.insert(*rn, true.into());
            } else {
                let is_allocated = inf_state[rn].occupation.has_value(&Some(*train_id));

                let progress_now = infrastructure.partial_routes.iter()
                    .filter(|(_,r_next)| r_next.entry == infrastructure.partial_routes[rn].exit)
                    .map(|(r2,_)| inf_state[r2].occupation.has_value(&Some(*train_id)));

                let progress_future = s.new_lit();

                let mut alternatives = progress_now.collect::<Vec<_>>();
                alternatives.push(progress_future);
                alternatives.push(!is_allocated);
                s.add_clause(alternatives);

                if let Some(prev_state) = prev_state {
                    let next_routes : Vec<_> = infrastructure.partial_routes.iter()
                        .filter(|(rn,r_next)| r_next.entry == infrastructure.partial_routes[rn].exit)
                        .map(|(rn,r)| *rn).collect();
                    let mut conflict_resolved = resolve_conflict_with(s, &infrastructure.partial_routes,
                          &infrastructure.elementary_routes, *train_id, 
                          &prev_state.infrastructure, &inf_state,
                          &next_routes);

                    conflict_resolved.push(*progress_before);
                    conflict_resolved.push(progress_future);
                    s.add_clause(conflict_resolved);
                }

                r_pr.insert(*rn, !progress_future);
            } 
        }


        // Born: each train can only appear once.
        let mut born_now_alternatives = Vec::new();
        let first_visit_nodes = train.visits.get(0);

        // TODO This section got a bit messy
        for (rn, r) in infrastructure.partial_routes.iter().filter(|(rn,r)| r.entry == SignalId::Boundary) {
            // Can the train be born here?
            if let Some(first_visit_nodes) = first_visit_nodes {
                //if r.contains_nodes.intersection(first_visit_nodes).nth(0).is_some() {
                if first_visit_nodes.contains(&rn.0) {
                    born_now_alternatives.push(did_activate(s, rn, train_id));
                } else {
                    // Don't go here ever.
                    s.add_clause(vec![ !inf_state[rn].occupation.has_value(&Some(*train_id)) ]);
                }
            } else {
                // There are no visits. That's strange, but then every route is allowed.
                born_now_alternatives.push(did_activate(s, rn, train_id));
            }
        }

        let born_before = prev_trains[train_id].born_before;
        let born_now    = s.or_literal(born_now_alternatives);
        let born_future = s.new_lit();
        exactly_one(s, vec![born_before, born_now, born_future]);

        if let Some(prev_state) = prev_state {
            let birth_candidates = infrastructure.partial_routes.iter().filter(|(rn,r)| r.entry == SignalId::Boundary);
            let birth_candidates : Vec<_> = if let Some(first_visit_nodes) = first_visit_nodes {
                    birth_candidates.filter(|(rn,r)| first_visit_nodes.contains(&rn.0)).collect()
                } else { birth_candidates.collect() };
            let birth_candidates = birth_candidates.into_iter().map(|(rn,r)| *rn).collect::<Vec<_>>();

            let mut conflict_resolved =  resolve_conflict_with(s, &infrastructure.partial_routes,
                           &infrastructure.elementary_routes, *train_id,
                           &prev_state.infrastructure, &inf_state,
                           &birth_candidates);

            conflict_resolved.push(!born_now);
            s.add_clause(conflict_resolved);
        }

        // Visits:
        // Each visit has to happen some time.
        let mut train_visit = Vec::new();
        for (i,visit_before) in prev_trains[train_id].visit_before.iter().enumerate() {
            let alternative_routes = infrastructure.partial_routes.iter()
                .filter(|(rn,r)| usage.trains[train_id].visits[i].contains(&rn.0))
                .map(|(rn,r)| inf_state[rn].occupation.has_value(&Some(*train_id)));

            let visit_now = s.or_literal(alternative_routes);
            let visit_future = s.new_lit();

            s.add_clause(vec![*visit_before, visit_now, visit_future]);
            train_visit.push(!visit_future);
        }



        trains_state.insert(*train_id, TrainsState {
            progress_before: r_pr,
            born_before: !born_future,
            visit_before: train_visit,
        });

    }
    // Visits need to happen in order
    for ord in usage.train_ord.iter() {
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

    println!("mk_state: Trains state {:?}", trains_state);
    State { infrastructure: inf_state, trains: trains_state }
}

fn exactly_one(s :&mut Solver, v :Vec<Bool>) {
    s.assert_at_most_one(v.iter().cloned());
    s.add_clause(v);
}

fn resolve_conflict_with(s :&mut Solver,
                         problem_partial_routes :&HashMap<PartialRouteId, PartialRoute>,
                         problem_elementary_routes :&Vec<HashSet<PartialRouteId>>,
                         train_id :TrainId,
                         prev_state :&HashMap<PartialRouteId, InfrastructureState>,
                              state :&HashMap<PartialRouteId, InfrastructureState>,
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
                     partial_routes :&HashMap<PartialRouteId, InfrastructureState>, 
                     active_signals: Option<&HashMap<SignalId, Bool>>,
                     train_id :TrainId,
                     remaining_length :f32) -> Vec<Bool> {
    if r.exit.is_boundary() {
        vec![true.into()]
    } else {
        let mut alternatives = Vec::new();
        for (next_route_id,next_route) in all_routes.iter().filter(|(_,r_next)| r_next.entry == r.exit) {
            let occupied = partial_routes[next_route_id].occupation.has_value(&Some(train_id));

            let detector_works = active_signals.map(|m| m[&r.exit]).unwrap_or(true.into());
            if next_route.length >= remaining_length {
                alternatives.push(occupied);
            } else {
                let nexts = is_freeable_after(s, next_route, all_routes, partial_routes, 
                                              active_signals, train_id, remaining_length - next_route.length);
                let any_next = s.or_literal(nexts);
                alternatives.push(s.and_literal(vec![ detector_works,  occupied, any_next ]));

                // TODO this might do infinite loops on looping infrastructure (use memoization?)
                if let Some(active_signals) = active_signals {
                    let nexts_deactivated = 
                        is_freeable_after(s, next_route, all_routes, partial_routes, 
                                          Some(active_signals), train_id, remaining_length);
                    let any_next_deactivated = s.or_literal(nexts_deactivated);
                    alternatives.push(s.and_literal(vec![ !detector_works, occupied, any_next_deactivated ]));
                }
            }
        }
        alternatives
    }
}

fn mk_schedule(states :&[State], model :&Model) -> RoutePlan {
    states.iter().map(|state| {
        state.infrastructure.iter().map(|(rn,r)| {
            (*rn, *model.value(&r.occupation))
        }).collect()
    }).collect()
}

fn end_state_condition(trains :&HashMap<TrainId, TrainsState>) -> Vec<Bool> {
    let mut condition = Vec::new();
    for (train_id,ts) in trains.iter() {
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
fn repeat_check(problem :&Infrastructure, plan :&RoutePlan) -> Result<(), Repeat> {
    // TODO
    Ok(())
}


fn disallow_loop(s :&mut Solver, loop_ :&Loop) {
}

fn disallow_repeat(s :&mut Solver, repeat :&Repeat) {
}

fn disallow_schedule(s :&mut Solver, prefix :Vec<Bool>, states :&[State], plan :&RoutePlan) {
}


