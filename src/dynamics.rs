use smallvec::SmallVec;

#[derive(Copy, Clone)]
pub struct TrainParams {
    pub len: f64,
    pub max_acc: f64,
    pub max_brk: f64,
    pub max_vel: f64,
}

#[derive(Copy,Clone)]
pub enum DriverAction {
    Accel,
    Brake,
    Coast,
}

#[derive(Copy,Clone)]
pub struct DistanceVelocity {
    pub dx: f64,
    pub v: f64,
}

#[derive(Copy,Clone)]
pub struct DriverPlan {
    pub action: DriverAction,
    pub dt: f64,
}

pub struct StaticMaximumVelocityProfile {
    local_max_velocity: f64,
    max_velocity_ahead: SmallVec<[DistanceVelocity; 4]>,
}

pub fn dynamic_update(train: &TrainParams,
                      current_velocity: f64,
                      plan: DriverPlan)
                      -> DistanceVelocity {
    match plan.action {
        DriverAction::Accel => {
            DistanceVelocity {
                dx: current_velocity * plan.dt + 0.5 * train.max_acc * plan.dt * plan.dt,
                v: current_velocity + plan.dt * train.max_acc,
            }
        }
        DriverAction::Brake => {
            DistanceVelocity {
                dx: current_velocity * plan.dt - 0.5 * train.max_brk * plan.dt * plan.dt,
                v: current_velocity - plan.dt * train.max_brk,
            }
        }
        DriverAction::Coast => {
            DistanceVelocity {
                dx: current_velocity * plan.dt,
                v: current_velocity,
            }
        }
    }
}


#[derive(Copy, Clone)]
struct TimeDistVel {
    t: f64,
    x: f64,
    v: f64,
}
type Point = TimeDistVel;

// Plan types
fn plan_accel_v(start: Point, v: f64, acc: f64) -> Point {
    let dt = (v - start.v) * acc;
    let dx = start.v * dt + 0.5 * acc * dt * dt;
    Point {
        t: start.t + dt,
        x: start.x + dx,
        v: v,
    }
}

fn plan_accel_x(start: Point, dx: f64, acc: f64) -> Point {
    let v = (2.0 * acc * dx + start.v * start.v).sqrt();
    let dt = (v - start.v) / acc;
    TimeDistVel {
        t: start.t + dt,
        x: start.x + dx,
        v: v,
    }
}

fn plan_accel_brake_intersection(start: Point,
                                 restriction: DistanceVelocity,
                                 acc: f64,
                                 brk: f64)
                                 -> (Point, Point) {
    let intersection_dx = (2.0 * brk * restriction.dx + restriction.v * restriction.v -
                           start.v * start.v) / (2.0 * (acc + brk));

    let intersection_v = (2.0 * acc * intersection_dx + start.v * start.v).sqrt();
    let intersection_dt = (intersection_v - start.v) / acc;

    let brake_dx = (start.v * start.v - restriction.v * restriction.v) / (2.0 * brk);
    let brake_dt = (start.v - restriction.v) / brk;

    (Point {
         // Accelerate to
         t: start.t + intersection_dt,
         x: start.x + intersection_dx,
         v: intersection_v,
     },
     Point {
         // Then brake to
         t: start.t + intersection_dt + brake_dt,
         x: start.x + intersection_dx + brake_dx,
         v: restriction.v,
     })
}

fn plan_coast_x(start: Point, dx: f64) -> Point {
    TimeDistVel {
        t: start.t + dx / start.v,
        x: start.x + dx,
        v: start.v,
    }
}

fn plan_coast_brake_intersection(start: Point,
                                 restriction: DistanceVelocity,
                                 brk: f64)
                                 -> (Point, Point) {
    let brake_dx = (start.v * start.v - restriction.v * restriction.v) / (2.0 * brk);
    let brake_dt = (start.v - restriction.v) / brk;
    let coast_dx = restriction.dx - brake_dx;
    let coast_dt = coast_dx / start.v;

    (Point {
         // Coast to
         t: start.t + coast_dt,
         x: start.x + coast_dx,
         v: start.v,
     },
     Point {
         t: start.t + coast_dt + brake_dt,
         x: start.x + coast_dx + brake_dx,
         v: restriction.v,
     })
}

pub fn dynamic_plan_step(train: &TrainParams,
                         max_dist: f64,
                         current_velocity: f64,
                         profile: &StaticMaximumVelocityProfile)
                         -> DriverPlan {

    let tol = 1e-4;

    let mut accel_plans = Vec::new();
    let mut coast_plans = Vec::new();
    let mut brake_plans = Vec::new();

    let p = Point {
        t: 0.0,
        x: 0.0,
        v: current_velocity,
    };

    // Acceleration is limited by current max speed
    accel_plans.push(plan_accel_v(p, profile.local_max_velocity, train.max_acc));

    // Acceleration is limited by maximum travel distance
    accel_plans.push(plan_accel_x(p, max_dist, train.max_acc));

    // Coasting is limited by maximum travel distance
    coast_plans.push(plan_coast_x(p, max_dist));

    for &restriction in profile.max_velocity_ahead.iter() {
        // Acceleration limited by braking curve (+ braking curve)
        let (acc, brk) =
            plan_accel_brake_intersection(p, restriction, train.max_acc, train.max_brk);
        accel_plans.push(acc);
        brake_plans.push(brk);

        // Coasting limited by braking curve (+ braking curve)
        let (coast, brk) = plan_coast_brake_intersection(p, restriction, train.max_brk);

        coast_plans.push(coast);
        brake_plans.push(brk);
    }

    let shortest_accel_plan = accel_plans.iter()
        .fold(accel_plans[0], |a, b| if a.t < b.t { a } else { *b });
    let shortest_coast_plan = coast_plans.iter()
        .fold(coast_plans[0], |a, b| if a.t < b.t { a } else { *b });
    let shortest_brake_plan = brake_plans.iter()
        .fold(brake_plans[0], |a, b| if a.t < b.t { a } else { *b });

    if shortest_accel_plan.t > tol {
        DriverPlan {
            action: DriverAction::Accel,
            dt: shortest_accel_plan.t,
        }
    } else if shortest_coast_plan.t > tol {
        DriverPlan {
            action: DriverAction::Coast,
            dt: shortest_coast_plan.t,
        }
    } else {
        DriverPlan {
            action: DriverAction::Brake,
            dt: shortest_brake_plan.t,
        }
    }
}
