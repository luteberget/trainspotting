//! Railway simulation.

pub mod infrastructure;
pub mod driver;
pub mod dynamics;
pub mod route;

use super::eventsim;
use railway::infrastructure::Infrastructure;
use output::history::InfrastructureLogEvent;
type Sim<'a> = eventsim::Simulation<Infrastructure<'a>, InfrastructureLogEvent>;
type Proc<'a> = eventsim::Process<Infrastructure<'a>, InfrastructureLogEvent>;

