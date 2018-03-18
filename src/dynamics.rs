use smallvec::SmallVec;

#[derive(Copy, Clone)]
pub struct TrainParams {
    pub len :f64,
    pub max_acc: f64,
    pub max_brk: f64,
    pub max_vel: f64,
}

#[derive(Copy,Clone)]
pub enum DriverAction { Accel, Brake, Coast }

#[derive(Copy,Clone)]
pub struct DistanceVelocity {
    dx :f64,
    v :f64,
}

#[derive(Copy,Clone)]
pub struct DriverPlan {
    action :DriverAction,
    dt  :f64,
}

pub struct StaticMaximumVelocityProfile {
    local_max_velocity :f64,
    max_velocity_ahead :SmallVec<[DistanceVelocity; 4]>,
}

pub fn dynamic_update(train :&TrainParams, current_velocity :f64,
                      plan :DriverPlan) -> DistanceVelocity {
    match plan.action {
        DriverAction::Accel => {
            DistanceVelocity { 
                dx: current_velocity * plan.dt + 0.5 * train.max_acc * plan.dt * plan.dt,
                v:  current_velocity + plan.dt * train.max_acc
            } 
        },
        DriverAction::Brake => {
            DistanceVelocity {
                dx: current_velocity * plan.dt - 0.5 * train.max_brk * plan.dt * plan.dt,
                v:  current_velocity - plan.dt * train.max_brk,
            }
        },
        DriverAction::Coast => {
            DriverAction {
                dx: current_velocity * plan.dt,
                v:  current_velocity,
            }
        }
    }
}


#[derive(Copy, Clone)]
struct TimeDistVel {
    t:f64,
    x:f64,
    v:f64,
}

fn min_t(a :TimeDistVel, b :TimeDistVel) -> TimeDistVel {
    if a.t < b.t { a } else { b }
}

fn plan_const_acc_velocity((v0,v1) :(f64,f64), acc :f64) -> TimeDistVel {
    let t = (v1-v0)*acc;
    let x = v0*t + 0.5*acc*t*t;
    let v = v1;
    TimeDistVel { t, x, v }
}

fn plan_const_acc_dist(v0 :f64, x: f64, acc: f64) -> TimeDistVel {
    let v = (2*acc*x + v0*v0).sqrt();
    let t = (v - v0) / acc;
    TimeDistVel { t, x, v }
}

#[derive(Copy, Clone)]
struct AccelBrakeTime {
    accel_time :f64,
    brake_time :f64,
}

fn limit_accel_braking_curve(acc_brk_times :AccelBrakeTime, restriction :DistanceTime, plan :TimeDistVel, params :&TrainParams) -> AccelBrakeTime {
    // TODO plan.v is wrong, should it be current_velocity?
    let brake_dx = (plan.v * plan.v - restriction.v * restriction.v) / 2* params.max_brk;
    if restriction.dx < plan.x + brake_dx {
        let intersection_dx = (2 * params.max_brk * restriction.dx + restriction.v * restriction.v - plan.v*plan.v) / 
            (2 * (params.max_acc + params.max_brk));

        let intersection_v = (2 * params.max_acc * intersection_dx + plan.v * plan.v).sqrt();
        let intersection_t = (intersection.v - plan_v) / params.max_acc;

        let brake_dx = (plan.v * plan.v - restriction.v*restriction.v) / (2*params.max_brk);
        let brake_dt = (plan.v - restriction.v) / params.max_brk;

        if brake_dx > plan.x

    } else {
        acc_brk_times
    }
}

pub fn dynamic_plan_step(train :&TrainParams, max_dist :f64,
                         current_velocity :f64, profile :&StaticMaximumVelocityProfile) -> DriverPlan {
    let tol = 1e-4;

    if current_velocity + tol < profile.local_max_velocity {
        let (v0,v1) = (current_velocity, profile.local_max_velocity);

        let plan1 = plan_const_acc_velocity((v0,v1), acc);
        let plan2 = plan_const_acc_dist(v0, x, acc);
        let plan = min_t(plan1, plan2);

        let mut accel_brake_time = AccelBrakeTime { accel_time: plan.t, brake_time: 0.0 };
        for restriction in profile.max_velocity_ahead {
            accel_brake_time = limit_accel_braking_curve(accel_brake_time, restriction)
        }
    }
}
