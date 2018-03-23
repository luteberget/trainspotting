use super::staticinfrastructure::*;
use super::parser_utils::*;

use std::collections::HashMap;

type Map = HashMap<String,usize>;




//
// PARSER
//
//


pub fn parse(t: &[Token], inf :&StaticInfrastructure) -> Result<Routes, ParseError> {
    let mut i = 0;
    let mut routes = HashMap::new();
    while t[i] != Token::EOF {
        match parse_route(&mut i, t, &inf.object_names, &inf.node_names)? {
            Some((name,route)) => { routes.insert(name,route); },
            _ => {},
        }
    }
    Ok(routes)
}

fn lookup(names :&HashMap<String,usize>, name :&str) -> Result<usize, ParseError>{
    names.get(name).cloned().ok_or(ParseError::UnknownName(name.to_string(), "infrastructure".to_string()))
}

pub fn parse_route(i :&mut usize, t:&[Token], objnames:&Map, nodenames:&Map) -> Result<Option<(String,Route)>, ParseError> {
    alt(i,t,&[
        &|i,t| {
            symbol(i,t,"modelentry")?;
            let _b = identifier(i,t)?;
            let _n = identifier(i,t)?;
            let _d = number(i,t)?;
            Ok(None) // Ignoring model boundaries in this program
        },
        &|i,t| {
            symbol(i,t,"modelexit")?;
            let _b = identifier(i,t)?;
            let _n = identifier(i,t)?;
            let _d = number(i,t)?;
            Ok(None) // Ignoring model boundaries in this program
        },
        &|i,t| {
            symbol(i,t,"route")?;
            let route_name = identifier(i,t)?;
            must_match(i,t,Token::BraceOpen)?;
            symbol(i,t,"entry")?;
            let entry = lookup(objnames, &identifier(i,t)?)?;
            symbol(i,t,"exit")?;
            let _exit = identifier(i,t)?;
            symbol(i,t,"entrysection")?;
            let entrysection = lookup(objnames, &identifier(i,t)?)?;
            symbol(i,t,"length")?;
            let length = number(i,t)?;
            symbol(i,t,"sections")?;
            let sections = list(i,t,|i,t| lookup(objnames,&identifier(i,t)?))?;
            symbol(i,t,"switches")?;
            let switches = list(i,t,|i,t| {
                must_match(i,t,Token::ParensOpen)?;
                let sw = lookup(objnames, &identifier(i,t)?)?;
                let pos = alt(i,t,&[
                              &|i,t| { symbol(i,t,"left")?; Ok(SwitchPosition::Left) },
                              &|i,t| { symbol(i,t,"right")?; Ok(SwitchPosition::Right) },
                ])?;
                must_match(i,t,Token::ParensClose)?;
                Ok((sw,pos))
            })?;
            symbol(i,t,"contains")?;
            let _contains = list(i,t,|i,t| lookup(nodenames, &identifier(i,t)?))?;
            must_match(i,t,Token::BraceClose)?;
            let mut releases = Vec::new();
            while matches(i,t,Token::Identifier("release".to_string())) {
                must_match(i,t,Token::BraceOpen)?;
                symbol(i,t,"trigger")?;
                let trigger = lookup(objnames, &identifier(i,t)?)?;
                symbol(i,t,"resources")?;
                let resources = list(i,t,|i,t| lookup(objnames, &identifier(i,t)?))?;
                releases.push(Release { trigger: trigger, resources: resources.into() });
            }
            Ok(Some((route_name, Route {
                signal: entry,
                signal_trigger: entrysection,
                sections: sections.into(),
                switch_positions: switches.into(),
                length: length,
                releases: releases.into(),
            })))
        },
    ])
}

pub fn list<F,O>(i :&mut usize, t: &[Token], f:F) -> Result<Vec<O>, ParseError>
  where F : Fn(&mut usize, &[Token]) -> Result<O, ParseError> {
    must_match(i,t,Token::ListOpen)?;
    let mut v = Vec::new();
    if matches(i,t, Token::ListClose) { return Ok(v); }
    loop {
        v.push(f(i,t)?);
        if !matches(i, t, Token::ListSep) {
            break;
        }
    }
    must_match(i,t,Token::ListClose)?;
    Ok(v)
  }

pub fn symbol(i :&mut usize, t :&[Token], s:&str) -> Result<(), ParseError> {
    if identifier(i,t)? != s { Err(ParseError::UnexpectedToken(*i, format!("{:?}",s))) } else {Ok(())}
}

pub fn identifier(i: &mut usize, tokens: &[Token]) -> Result<String, ParseError> {
    let r = match tokens[*i] {
        Token::Identifier(ref s) => s.clone(),
        ref x => return Err(ParseError::UnexpectedToken(*i, format!("{:?}",x.clone()))),
    };
    *i += 1;
    Ok(r)
}

pub fn number(i: &mut usize, tokens: &[Token]) -> Result<f64, ParseError> {
    let r = match tokens[*i] {
        Token::Number(x) => x.clone(),
        ref x => return Err(ParseError::UnexpectedToken(*i, format!("{:?}",x.clone()))),
    };
    *i += 1;
    Ok(r)
}


//
// LEXER
//
//
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    BraceOpen, BraceClose, ListOpen, ListClose, ListSep, ParensOpen, ParensClose,
    Number(f64),
    Identifier(String),
    EOF,
}

pub fn lexer(x: &mut Iterator<Item = char>) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut input = x.peekable();
    let mut line = 0;
    loop {
        match input.peek() {
            Some(&ch) => {
                match ch {
                    x if x.is_numeric() => {
                        let num: String = consume_while(&mut input, |a| {
                                a.is_numeric() || a == '-' || a == 'e' || a == 'E' || a == '.'
                            })
                            .into_iter()
                            .collect();
                        tokens.push(Token::Number(num.parse::<f64>().unwrap()));
                    }
                    x if x.is_alphabetic() => {
                        let s: String = consume_while(&mut input, |a| a.is_alphanumeric())
                            .into_iter()
                            .collect();
                        tokens.push(Token::Identifier(s));
                    }
                    '[' => {
                        input.next().unwrap();
                        tokens.push(Token::ListOpen);
                    }
                    ']' => {
                        input.next().unwrap();
                        tokens.push(Token::ListClose);
                    }
                    ',' => {
                        input.next().unwrap();
                        tokens.push(Token::ListSep);
                    }
                    '(' => {
                        input.next().unwrap();
                        tokens.push(Token::ParensOpen);
                    }
                    ')' => {
                        input.next().unwrap();
                        tokens.push(Token::ParensClose);
                    }
                    '{' => {
                        input.next().unwrap();
                        tokens.push(Token::BraceOpen);
                    }
                    '}' => {
                        input.next().unwrap();
                        tokens.push(Token::BraceClose);
                    }
                    ' ' | '\r' | '\t' => {
                        input.next().unwrap();
                    }
                    '\n' => {
                        input.next().unwrap();
                        line += 1;
                    }
                    c => {
                        return Err(LexerError::UnexpectedChar {
                            i: line, c: c.to_string()});
                    }
                }
            }
            None => {
                break;
            }
        }
    }
    tokens.push(Token::EOF);
    Ok(tokens)
}
