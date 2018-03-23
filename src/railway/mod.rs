//! Railway simulation.

pub mod infrastructure;
pub mod driver;
pub mod dynamics;
pub mod route;

use super::eventsim;
use railway::infrastructure::Infrastructure;
type Sim<'a> = eventsim::Simulation<Infrastructure<'a>>;
type Proc<'a> = eventsim::Process<Infrastructure<'a>>;
