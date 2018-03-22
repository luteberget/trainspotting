//! Railway simulation.

pub mod infrastructure;
pub mod driver;
pub mod dynamics;
pub mod route;

use super::eventsim;
use railway::infrastructure::Infrastructure;
use railway::infrastructure::Logger;
type Sim<'a,L> = eventsim::Simulation<Infrastructure<'a>, L>;
type Proc<'a,L> = eventsim::Process<Infrastructure<'a>, L>;

