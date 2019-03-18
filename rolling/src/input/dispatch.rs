use railway::dynamics::TrainParams;
use regex::Regex;

type TrainName = String;

#[derive(Debug)]
pub struct Dispatch<RouteRef> {
    pub actions: Vec<DispatchAction<RouteRef>>,
}

#[derive(Debug)]
pub enum DispatchAction<RouteRef> {
    Wait(Option<f64>),
    Route(RouteRef),
    Train(TrainName, TrainParams, RouteRef), // train name, train params, entry route name
}


#[derive(Debug, Fail)]
pub enum ParseError {
    #[fail(display = "error in regular expression: {}", _0)]
    RegexError(String),
    #[fail(display = "error converting number")]
    NumberError,
    #[fail(display = "unrecognized dispatch: {}", _0)]
    Unrecognized(String),
}

/// Parses dispatch plan format
///
/// * wait 10.0
/// * route rb1
/// * train t1 (b1 -> 200.0) l=200.0 a=1.0 b=0.5 v=10.0
///
pub fn parse_dispatch(input: &str) -> Result<Dispatch<String>, ParseError> {
    let mut actions = Vec::new();
    let wait_time_re = Regex::new(r"^\s*wait\s*([\d\.]+)\s*$")
        .map_err(|e| ParseError::RegexError(format!("{:?}",e)))?;
    let wait_re = Regex::new(r"^\s*wait\s*$")
        .map_err(|e| ParseError::RegexError(format!("{:?}",e)))?;
    let route_re = Regex::new(r"^\s*route\s*([\w\.]+)\s*$")
        .map_err(|e| ParseError::RegexError(format!("{:?}",e)))?;
    let train_re = Regex::new(r"(?x) ^ \s* train \s+ (?P<name>\w+) \s+
            l \s* = \s* (?P<len>[\d\.]+) \s+
            a \s* = \s* (?P<acc>[\d\.]+) \s+
            b \s* = \s* (?P<brk>[\d\.]+) \s+
            v \s* = \s* (?P<vel>[\d\.]+) \s+
            (?P<route>\w+) \s*
            $").map_err(|e| ParseError::RegexError(format!("{:?}", e)))?;
    for line in input.lines() {
        if let Some(groups) = wait_time_re.captures(line) {
            let time = groups[1].parse::<f64>().map_err(|_e| ParseError::NumberError)?;
            actions.push(DispatchAction::Wait(Some(time)));
            continue;
        }
        if let Some(_) = wait_re.captures(line) {
            actions.push(DispatchAction::Wait(None));
            continue;
        }
        if let Some(groups) = route_re.captures(line) {
            actions.push(DispatchAction::Route(groups[1].to_string()));
            continue;
        }
        if let Some(groups) = train_re.captures(line) {
            actions.push(DispatchAction::Train(groups["name"].to_string(),
                                               TrainParams {
                                                   length: groups["len"].parse::<f64>()
                                                       .map_err(|_e| ParseError::NumberError)?,
                                                   max_acc: groups["acc"].parse::<f64>()
                                                       .map_err(|_e| ParseError::NumberError)?,
                                                   max_brk: groups["brk"].parse::<f64>()
                                                       .map_err(|_e| ParseError::NumberError)?,
                                                   max_vel: groups["vel"].parse::<f64>()
                                                       .map_err(|_e| ParseError::NumberError)?,
                                               },
                                               groups["route"].to_string()));
            continue;
        }
        return Err(ParseError::Unrecognized(line.to_string()));
    }

    Ok(Dispatch { actions: actions })
}
