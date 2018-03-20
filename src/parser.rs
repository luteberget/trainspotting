use smallvec::SmallVec;
//
// AST
//
//
type NodeName = String;
type Dist = f64;
#[derive(Debug)]
pub enum Statement {
    DoubleNode(PartNode,PartNode),
    Boundary(NodeName),
    Linear(NodeName, NodeName, Dist),
    Switch(String, NodeName, Vec<(NodeName, Dist)>),
}

#[derive(Debug)]
pub struct PartNode {
    name :String,
    contents: Vec<Object>,
}

#[derive(Debug)]
pub enum Object {
    Sight(String,Dist),
    Signal(String),
    Exit(String),
    Enter(String),
}

//
// LEXER
//
//
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Node, Linear, Switch,
    OpenList, CloseList, Arrow, ListSep,
    Signal, Exit, Enter, Sight, Boundary,
    Number(f64),
    Identifier(String),
    EOF,
}

#[derive(Debug,Clone)]
pub enum LexerError {
UnexpectedChar(usize,String),
UnexpectedEOF,
}


use std::iter::Peekable;
pub fn lexer(x: &mut Iterator<Item=char>) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut input = x.peekable();
    let mut line = 0;
    loop {
        match input.peek() {
            Some(&ch) => {
                match ch {
                    x if x.is_numeric() => {
                        let num: String =
                            consume_while(&mut input, |a| a.is_numeric() || a == '-' ||  a == 'e'
                                          || a == 'E' || a == '.' ).into_iter().collect();
						tokens.push(Token::Number(num.parse::<f64>().unwrap()));
                    }
                    x if x.is_alphabetic() => {
                        let s: String = consume_while(&mut input, |a| a.is_alphanumeric())
                            .into_iter()
                            .collect();
                        tokens.push(match s.as_ref() {
                            "node" => Token::Node,
                            "linear" => Token::Linear,
                            "switch" => Token::Switch,
                            "boundary" => Token::Boundary,
                            "signal" => Token::Signal,
                            "sight" => Token::Sight,
                            "exit" => Token::Exit,
                            "enter" => Token::Enter,
                            _ => Token::Identifier(s),
                        });
                    },
                    '(' => {
                        input.next().unwrap();
                        tokens.push(Token::OpenList);
                    }
                    ')' => {
                        input.next().unwrap();
                        tokens.push(Token::CloseList);
                    }
                    ',' => {
                        input.next().unwrap();
                        tokens.push(Token::ListSep);
                    }
                    '-' => {
                        input.next().unwrap();
                        tokens.push(Token::Arrow);
                    }
                    ' ' | '\r' | '\t' => {
                        input.next().unwrap();
                    }
                    '\n' => {
                        input.next().unwrap();
						line += 1;
					}
                    c => {
                        return Err(LexerError::UnexpectedChar(line, c.to_string()));
                    }
                }
            },
            None => { break; }
        }
    }
    tokens.push(Token::EOF);
    Ok(tokens)
}

fn consume_while<F>(it: &mut Peekable<&mut Iterator<Item = char>>, x: F) -> Vec<char>
    where F: Fn(char) -> bool
{

    let mut v: Vec<char> = vec![];

    while let Some(&ch) = it.peek() {
        if x(ch) {
            it.next().unwrap();
            v.push(ch);
        } else {
            break;
        }
    }

    v
}


//
// PARSER
//
//

#[derive(Debug,Clone)]
pub enum ParseError {
    UnexpectedToken(usize, Token),
    UnexpectedEOF,
    Many(Vec<ParseError>),
}

pub fn parse(t :&Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let mut i = 0;
    let mut statements = Vec::new();
    while t[i] != Token::EOF {
        statements.push(parse_statement(&mut i, t)?);
    }
    Ok(statements)
}

pub fn identifier(i: &mut usize, tokens: &[Token]) -> Result<String, ParseError> {
    let r = match tokens[*i] {
        Token::Identifier(ref s) => s.clone(),
        ref x => return Err(ParseError::UnexpectedToken(*i, x.clone())),
    };
    *i += 1;
    Ok(r)
}

pub fn number(i: &mut usize, tokens: &[Token]) -> Result<f64, ParseError> {
    let r = match tokens[*i] {
        Token::Number(x) => x.clone(),
        ref x => return Err(ParseError::UnexpectedToken(*i, x.clone())),
    };
    *i += 1;
    Ok(r)
}


pub fn parse_statement(i :&mut usize, t :&[Token]) -> Result<Statement, ParseError> {
    alt(i,t,&[
        &|i,t| {
            must_match(i,t,Token::Boundary)?;
            Ok(Statement::Boundary(identifier(i,t)?))
        },
        &|i,t| {
            must_match(i,t,Token::Node)?;
            let n1 = identifier(i,t)?;
            let n1_objects = parse_opt_object_list(i,t)?;
            must_match(i,t,Token::Arrow)?;
            let n2 = identifier(i,t)?;
            let n2_objects = parse_opt_object_list(i,t)?;
            Ok(Statement::DoubleNode(PartNode { name: n1, contents: n1_objects },
                                     PartNode { name: n2, contents: n2_objects }))
        },
        &|i,t| {
            must_match(i,t,Token::Linear)?;
            let n1 = identifier(i,t)?;
            must_match(i,t,Token::Arrow)?;
            let n2 = identifier(i,t)?;
            let dist = number(i,t)?;
            Ok(Statement::Linear(n1,n2,dist))
        },
        &|i,t| {
            must_match(i,t,Token::Switch)?;
            let name = identifier(i,t)?;
            let facenode = identifier(i,t)?;
            must_match(i,t,Token::Arrow)?;
            let branches = parse_switch_branches(i,t)?;
            Ok(Statement::Switch(name, facenode, branches))
        }
    ])
}

pub fn parse_switch_branches(i :&mut usize, t: &[Token]) -> Result<Vec<(NodeName,Dist)>, ParseError> {
    let mut branches = Vec::new();
    must_match(i,t,Token::OpenList)?;
    loop {
        let name = identifier(i,t)?;
        let dist = number(i,t)?;
        branches.push((name,dist));
        if !matches(i,t,Token::ListSep) { break; }
    }
    must_match(i,t,Token::CloseList)?;
    Ok(branches)
}

pub fn parse_opt_object_list(i :&mut usize, t: &[Token]) -> Result<Vec<Object>, ParseError> {
    let mut objs = Vec::new();
    if !matches(i,t,Token::OpenList) { return Ok(objs); }
    loop {
        objs.push(parse_object(i,t)?);
        if !matches(i,t,Token::ListSep) { break; }
    }
    must_match(i,t,Token::CloseList)?;
    Ok(objs)
}

pub fn parse_object(i :&mut usize, t: &[Token]) -> Result<Object, ParseError> {
    alt(i,t,&[
        &|i,t| {
            must_match(i,t, Token::Sight)?;
            let name = identifier(i,t)?;
            let dist = number(i,t)?;
            Ok(Object::Sight(name, dist))
        },
        &|i,t| {
            must_match(i,t, Token::Signal)?;
            Ok(Object::Signal(identifier(i,t)?))
        },
        &|i,t| {
            must_match(i,t, Token::Exit)?;
            Ok(Object::Exit(identifier(i,t)?))
        },
        &|i,t| {
            must_match(i,t, Token::Enter)?;
            Ok(Object::Enter(identifier(i,t)?))
        },
    ])
}


pub fn alt<T>(i :&mut usize, tokens: &[Token], alts :&[&Fn(&mut usize, &[Token]) -> Result<T,ParseError>]) -> Result<T, ParseError> {
    let start = *i;
    let mut errs = Vec::new();
    for alt in alts {
        *i = start;
        match alt(i,tokens) {
            Ok(x) => return Ok(x),
            Err(y) => errs.push(y),
        }
    }
    Err(ParseError::Many(errs))
}

pub fn must_match(i: &mut usize, tokens: &[Token], tok: Token) -> Result<(), ParseError> {
    if matches(i, tokens, tok) {
        Ok(())
    } else {
        Err(ParseError::UnexpectedToken(*i,tokens[*i].clone()))
    }
}

pub fn matches(i: &mut usize, tokens: &[Token], tok: Token) -> bool {
    let r = tokens[*i] == tok;
    if r {
        *i += 1;
    }
    r
}



//
// CONVERT AST TO MODEL
//
//


use railway;

#[derive(Debug,Clone)]
pub enum ModelError {
    SwitchLegs(String),
    Other,
}

use std::collections::HashMap;
fn id(nodes :&mut HashMap<String, usize>, next :usize, name :&str) -> usize {
    if nodes.contains_key(name) {
        nodes[name]
    } else {
        nodes.insert(name.to_string(), next);
        next
    }
}

fn insert_object(list :&mut Vec<railway::StaticObject>, names: &mut HashMap<String, usize>, obj :railway::StaticObject, name:&str) -> usize {
    if names.contains_key(name) {
        list[names[name]] = obj;
        names[name]
    } else {
        let idx = list.len();
        list.push(obj);
        names.insert(name.to_string(), idx);
        idx
    }
}

fn get_or_create_node(mnodes :&mut Vec<railway::Node>, nodes :&mut HashMap<String,usize>, name :&str) -> usize {
    let next = mnodes.len();
    let idx = id(nodes, next, name);
    if next == idx {
        mnodes.push(railway::Node { 
            other_node: 0,
            edges: railway::Edges::Nothing,
            objects: SmallVec::new() });
    }
    idx
}

pub fn model_from_ast(stmts :&[Statement]) -> Result<railway::StaticInfrastructure, ModelError> {
    use railway::{StaticInfrastructure,Edges};
    let mut model = StaticInfrastructure { nodes: Vec::new(),
                              objects: Vec::new() };

    let mut nodes :HashMap<String,usize> = HashMap::new();
    let mut objects :HashMap<String, usize> = HashMap::new();
    for s in stmts {
        use Statement::*;
        match s {
            &Boundary(ref name) => {
                let node_idx = get_or_create_node(&mut model.nodes, &mut nodes, name);
                model.nodes[node_idx].edges = Edges::ModelBoundary;
            },
            &Linear(ref name1, ref name2, dist) => {
                let n1 = get_or_create_node(&mut model.nodes, &mut nodes, name1);
                let n2 = get_or_create_node(&mut model.nodes, &mut nodes, name2);
                model.nodes[n1].edges = Edges::Single(n2, dist);
                model.nodes[n2].edges = Edges::Single(n1, dist);
            },
            &Switch(ref name, ref node, ref legs) => {
                let node_idx = get_or_create_node(&mut model.nodes, &mut nodes, node);
                if legs.len() != 2 {
                    return Err(ModelError::SwitchLegs(name.clone()));
                }
                let (ref l1name, l1dist) = legs[0];
                let l1idx = get_or_create_node(&mut model.nodes, &mut nodes, &l1name);
                let (ref l2name, l2dist) = legs[1];
                let l2idx = get_or_create_node(&mut model.nodes, &mut nodes, &l2name);
                let switch = railway::StaticObject::Switch {
                    left_link: (l1idx,l1dist),
                    right_link: (l2idx, l2dist),
                };
                let sw_idx = insert_object(&mut model.objects, &mut objects, switch, name);
                model.nodes[node_idx].edges = Edges::Switchable(sw_idx);
            },
            &DoubleNode(ref n1,ref n2) =>  {
                // Create cross references
                //
                let n1_idx = get_or_create_node(&mut model.nodes, &mut nodes, &n1.name);
                let n2_idx = get_or_create_node(&mut model.nodes, &mut nodes, &n2.name);

                model.nodes[n1_idx].other_node = n2_idx;
                model.nodes[n2_idx].other_node = n1_idx;

                // Create objects and insert references in node
                let mut ins_objs = |objs:&[Object],node| {
                    for obj in objs.iter() {
                    match obj {
                    &Object::Sight(ref name,d) => {
                        let signal = *objects.entry(d.to_string()).or_insert_with(|| {
                            let idx = model.objects.len();
                            model.objects.push(railway::StaticObject::Signal);
                            idx
                        });

                        let object = railway::StaticObject::Sight {
                            distance: d, signal: signal };
                        //let idx = insert_object(&mut model.objects, &mut objects, object, name);
                        let idx = model.objects.len();
                        model.objects.push(object);
                        let m :&mut railway::Node = &mut model.nodes[node];
                        m.objects.push(idx);
                    }
                    &Object::Signal(ref name) => {
                        let idx = insert_object(&mut model.objects, &mut objects, 
                                      railway::StaticObject::Signal, name);
                        let m :&mut railway::Node = &mut model.nodes[node];
                        m.objects.push(idx);
                    },
                    &Object::Exit(ref name) => {
                        // Create TVD if necessary
                        let tvd = *objects.entry(name.to_string()).or_insert_with(|| {
                            let idx = model.objects.len();
                            model.objects.push(railway::StaticObject::TVDSection);
                            idx
                        });

                        let idx = model.objects.len();
                        model.objects.push(railway::StaticObject::TVDLimit { exit: Some(tvd), enter: None });

                        let m :&mut railway::Node = &mut model.nodes[node];
                        m.objects.push(idx);
                    }
                    &Object::Enter(ref name) => {
                        // Create TVD if necessary
                        let tvd = *objects.entry(name.to_string()).or_insert_with(|| {
                            let idx = model.objects.len();
                            model.objects.push(railway::StaticObject::TVDSection);
                            idx
                        });

                        let idx = model.objects.len();
                        model.objects.push(railway::StaticObject::TVDLimit { enter: Some(tvd), exit: None });

                        let m :&mut railway::Node = &mut model.nodes[node];
                        m.objects.push(idx);
                    }
                    }
                    }
                };

                ins_objs(&n1.contents, n1_idx);
                ins_objs(&n2.contents, n2_idx);
            },
        }
    }
    println!("Object names: {:?}", objects);
    println!("Node names: {:?}", nodes);
    Ok(model)
}

