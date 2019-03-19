use smallvec::SmallVec;
use rolling::input::staticinfrastructure::{NodeId};

pub struct UsagePattern {
    vechicles :Vec<Vehicle>,
    movements :Vec<MovementSpec>,
    timings :Vec<TimingSpec>,
}
pub type Time = f32;

pub type Visit = (SmallVec<[NodeId;2]>,  Option<Time>);
pub type VisitId = usize;

pub struct MovementSpec {
    vehicle_ref :usize,
    visits: SmallVec<[Visit;4]>,
}

pub type VisitRef = (usize,usize); // UsagePaggern.movements[ref.0].visits[ref.1]

pub struct TimingSpec {
    visit_a :VisitRef,
    visit_b :VisitRef,
    timing_diff :Option<Time>,
}

pub struct Vehicle {
    name :String,
    length :f32,
    max_accel :f32,
    max_brake :f32,
    max_velocity :f32,
}
