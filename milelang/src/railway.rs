// Railway model suitable for conversion to railML

use ast;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
pub enum Dir { Up, Down }

impl Dir {
    pub fn factor(&self) -> f64 {
        match self {
            Dir::Up => 1.0,
            Dir::Down => -1.0,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Dir::Up => format!("up"),
            Dir::Down => format!("down"),
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub enum Side { Left, Right }

impl Side {
    pub fn to_string(&self) -> String {
        match self {
            Side::Left => format!("left"),
            Side::Right => format!("right"),
        }
    }

    pub fn opposite(&self) -> Self {
        match self {
            Side::Left => Side::Right,
            Side::Right => Side::Left,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Stop,
    Connection(RConnection),
}

#[derive(Debug, Clone)]
pub enum RConnection {
    Maybe(String),
    External(String),
    Internal(String,String),
}

#[derive(Debug)]
pub struct Track {
    pub id :String,
    pub name :String,
    pub begin :Node,
    pub end: Node,
    pub length :f64,
    pub switches: Vec<Switch>,
    pub objects: Vec<Object>,
}

#[derive(Debug)]
pub struct Object {
    pub pos :f64,
    pub data :ObjectData,
    pub name :String,
    pub id :String,
}

#[derive(Debug)]
pub enum ObjectData {
    Signal { dir: Dir, },
    Detector { },
}

#[derive(Debug)]
pub struct Switch {
    pub id: String,
    pub name: String,
    pub pos : f64,
    pub dir: Dir,
    pub side: Side,
    pub connection: RConnection,
}


use std::collections::BTreeMap;
struct Fresh(BTreeMap<String,usize>);
impl Fresh {
    pub fn mk(&mut self, s :String) -> String {
        format!("{}{}", s, self.0.entry(s.clone()).and_modify(|e| { *e += 1 }).or_insert(1))
    }
    pub fn new() -> Self { Fresh(BTreeMap::new()) }
}

struct Converter {
    fresh: Fresh,
    tracks: Vec<Track>,
    curr: Option<usize>,
    par: Vec<(usize, f64)>,
}

pub fn convert(input :Vec<ast::Statement>) -> Result<Vec<Track>, String> {
    let mut converter = Converter {
        fresh: Fresh::new(),
        tracks: Vec::new(),
        curr: None,
        par: Vec::new(),
    };

    for stmt in input {
        eval(&mut converter, stmt)?;
    }

    let mut tracks = converter.tracks;
    resolve_conns(&mut tracks)?;
    Ok(tracks)
}

fn eval(ctx :&mut Converter, stmt :ast::Statement) -> Result<(), String> {
    match stmt.action.as_ref() {
        "track" => track(ctx, stmt)?,
        "siding" => siding(ctx, stmt)?,
        "loop" => mkloop(ctx, stmt)?,
        "signal" => signal(ctx, stmt)?,
        "detector" => detector(ctx, stmt)?,
        _ => return Err(format!("Unrecognized statement {:?}", stmt)),
    }
    Ok(())
}

fn signal(ctx :&mut Converter, stmt :ast::Statement) -> Result<(), String> {
    let pos = require_param_float(&stmt, "x")?;
    let dir = optional_param_enum(&stmt, "dir", 
       &vec![ ("up", Dir::Up), ("down", Dir::Down)], Dir::Up)?;
    let track_idx = ctx.curr.ok_or(format!("Cannot insert object outside track context."))?;
    let id = ctx.fresh.mk(format!("sig"));
    let name = ctx.fresh.mk(format!("Signal "));
    ctx.tracks[track_idx].objects.push(Object {
        id, name, pos, data: ObjectData::Signal { dir},
    });
    Ok(())
}

fn detector(ctx :&mut Converter, stmt :ast::Statement) -> Result<(), String> {
    let pos = require_param_float(&stmt, "x")?;
    let track_idx = ctx.curr.ok_or(format!("Cannot insert object outside track context."))?;
    let id = ctx.fresh.mk(format!("det"));
    let name = ctx.fresh.mk(format!("Det "));
    ctx.tracks[track_idx].objects.push(Object {
        id, name, pos, data: ObjectData::Detector { },
    });
    Ok(())
}

use std::fmt::Debug;
fn require_select_mod<T:Debug+Clone>(stmt :&ast::Statement, choice :&Vec<(&str,T)>) -> Result<T,String> {
    let mut value = None;
    for m in &stmt.mods {
        for (c,v) in choice {
            if c == m {
                if value.is_some() {
                        return Err(format!("Statement {} requires exactly one modifier from this set: {:?}", 
                        stmt.action, choice));
                } else {
                    value = Some(v.clone());
                }
            }
        }
    }
    value.ok_or(format!("Statement {} requires exactly one modifier from this set: {:?}", 
                        stmt.action, choice))
}

fn optional_param_enum<T:Debug+Clone>(stmt :&ast::Statement, name :&str, choice :&Vec<(&str,T)>, default :T) -> Result<T,String> {
    use ast::*;
    stmt.args.iter().find(|(k,_v)| k == name)
        .map(|(_k,v)| if let Expr::Var(ref x) = **v { 
              choice.iter().find(|(k,v)| k == x).ok_or(format!("Enum")).map(|(k,v)| v.clone())
            } else { Err(format!("Enum")) } )
        .unwrap_or(Ok(default))
}

fn mkloop(ctx: &mut Converter, mut stmt :ast::Statement) -> Result<(), String> {
    let track_idx = ctx.curr.ok_or(format!("Cannot start siding outside track context."))?;
    let pos = require_param_float(&stmt, "x")?;
    let length = require_param_float(&stmt, "l")?;
    let side = require_select_mod(&stmt, &vec![ ("left", Side::Left), ("right", Side::Right)])?;

    let i = ctx.tracks.len();
    let id = ctx.fresh.mk(format!("tr"));
    let name = stmt.name.unwrap_or_else(|| ctx.fresh.mk(format!("Loop ")));

    // Switches on trunk track
    let sw_a_id = ctx.fresh.mk(format!("sw"));
    let sw_a_name = ctx.fresh.mk(format!("Spv"));
    let conn_a_name = ctx.fresh.mk(format!("swconn"));
    ctx.tracks[track_idx].switches.push(Switch { id: sw_a_id, pos,
        name: sw_a_name, dir: Dir::Up, side: side, 
        connection: RConnection::Maybe(conn_a_name.clone()) });
    let sw_b_id = ctx.fresh.mk(format!("sw"));
    let sw_b_name = ctx.fresh.mk(format!("Spv"));
    let conn_b_name = ctx.fresh.mk(format!("swconn"));
    ctx.tracks[track_idx].switches.push(Switch { id: sw_b_id, pos: pos +length,
        name: sw_b_name, dir: Dir::Down, side: side.opposite(), 
        connection: RConnection::Maybe(conn_b_name.clone()) });



    let begin = Node::Connection(RConnection::Maybe(conn_a_name.clone()));
    let end =   Node::Connection(RConnection::Maybe(conn_b_name.clone()));

    ctx.tracks.push(Track { id, name, begin, end , length, 
        objects: Vec::new(), switches: Vec::new() });

    let old_par = ctx.par.clone();
    if let Some(stmts) = stmt.block {
        ctx.curr = Some(i);
        ctx.par = vec![(i,0.0)];
        for s in stmts {
            eval(ctx, s)?;
        }
    }

    ctx.curr = Some(track_idx);
    ctx.par = old_par;
    ctx.par.push((i, pos));
    Ok(())
}

fn siding(ctx :&mut Converter, mut stmt :ast::Statement) -> Result<(), String> {
    let track_idx = ctx.curr.ok_or(format!("Cannot start siding outside track context."))?;
    let pos = require_param_float(&stmt, "x")?;
    let end = conv_tracknode(ctx, stmt.conns.pop()
        .ok_or(format!("Statement {} requires exactly one connection.",stmt.action))?);
    let length = require_param_float(&stmt, "l")?;
    let dir  = require_select_mod(&stmt, &vec![ ("out", Dir::Up), ("in", Dir::Down)])?;
    let side = require_select_mod(&stmt, &vec![ ("left", Side::Left), ("right", Side::Right)])?;
    let i = ctx.tracks.len();
    let id = ctx.fresh.mk(format!("tr"));
    let name = stmt.name.unwrap_or_else(|| ctx.fresh.mk(format!("Siding ")));

    // create switch on main track
    let sw_id = ctx.fresh.mk(format!("sw"));
    let sw_name = ctx.fresh.mk(format!("Spv"));
    let conn_name = ctx.fresh.mk(format!("swconn"));
    ctx.tracks[track_idx].switches.push(Switch { id: sw_id, pos,
        name: sw_name, dir: dir, side: side, connection: RConnection::Maybe(conn_name.clone()) });

    let begin = match dir {
        Dir::Up => Node::Connection(RConnection::Maybe(conn_name.clone())),
        Dir::Down => end.clone(),
    };
    let end = match dir {
        Dir::Up => end.clone(),
        Dir::Down => Node::Connection(RConnection::Maybe(conn_name.clone())),
    };


    ctx.tracks.push(Track { id, name, begin, end , length, 
        objects: Vec::new(), switches: Vec::new() });

    let old_par = ctx.par.clone();
    if let Some(stmts) = stmt.block {
        ctx.curr = Some(i);
        ctx.par = vec![(i,0.0)];
        for s in stmts {
            eval(ctx, s)?;
        }
    }

    ctx.curr = Some(track_idx);
    ctx.par = old_par;
    ctx.par.push((i, match dir {
        Dir::Up => pos,
        Dir::Down => pos - length,
    }));
    Ok(())
}

fn track(ctx :&mut Converter, mut stmt :ast::Statement) -> Result<(),String> {
    let length = require_param_float(&stmt, "l")?;
    let end = conv_tracknode(ctx, stmt.conns.pop()
        .ok_or(format!("Statement {} requires exactly two connections.",stmt.action))?);
    let begin = conv_tracknode(ctx, stmt.conns.pop()
        .ok_or(format!("Statement {} requires exactly two connections.",stmt.action))?);

    let i = ctx.tracks.len();
    let id = ctx.fresh.mk(format!("tr"));
    let name = stmt.name.unwrap_or_else(|| ctx.fresh.mk(format!("Track ")));
    ctx.tracks.push(Track { id, name, begin, end, length, 
        switches: Vec::new(), objects: Vec::new(),
    });
    if let Some(stmts) = stmt.block {
        ctx.curr = Some(i);
        ctx.par = vec![(i, 0.0)];
        for s in stmts {
            eval(ctx, s)?;
        }
    }
    ctx.curr = None;
    ctx.par = Vec::new();
    Ok(())
}

fn conv_tracknode(ctx :&mut Converter, c :ast::Connection) -> Node {
    match c {
        ast::Connection::Anonymous => Node::Connection(RConnection::Maybe(ctx.fresh.mk(format!("node")))),
        ast::Connection::Stop => Node::Stop,
        ast::Connection::Named(s) => Node::Connection(RConnection::Maybe(s)),
    }
}

fn require_param_float(stmt :&ast::Statement, name :&str) -> Result<f64, String> {
    use ast::*;
    stmt.args.iter().find(|(k,_v)| k == name)
        .ok_or(format!("Statement {} requires numerical parameter \"{}\".", stmt.action ,name))
        .and_then(|(_k,v)| if let Expr::Value(Value::Int(x)) = **v { Ok(x as f64) }
                          else if let Expr::Value(Value::Float(x)) = **v { Ok(x) }
                            else { Err(format!("Statement {} requires numerical parameter \"{}\".", stmt.action ,name)) })
}


pub fn resolve_conns(model :&mut Vec<Track>) -> Result<(),String> {
    let mut internal :HashMap<String,bool>= HashMap::new();

    // First pass: find which are internal which are external
    {
        let mut upd = |c:&RConnection| {
            if let RConnection::Maybe(ref s) = *c {
                internal.entry(s.clone()).and_modify(|e| { 
                    if *e == true { panic!("Connection name in three places."); }
                    *e = true; } ).or_insert(false);
            } else { panic!("Should be maybe"); }
        };
        for t in model.iter() {
            if let Node::Connection(ref c) = t.begin { upd(c); }
            if let Node::Connection(ref c) = t.end { upd(c); }
            for sw in &t.switches { upd(&sw.connection); }
        }
    }

    // Second pass: mark external/internal and split names for id/ref.
    {
        let mut marked :HashSet<String> = HashSet::new();
        let mut upd = |c:&mut RConnection| {
            let n = if let RConnection::Maybe(ref s) = *c { s.clone() } else { panic!() };
            if internal[&n] {
                if marked.insert(n.clone()) {
                    *c = RConnection::Internal(n.clone(), format!("{}ref",n));
                } else {
                    *c = RConnection::Internal(format!("{}ref",n),n.clone());
                }
            } else {
                *c = RConnection::External(n);
            }
        };
        for t in model.iter_mut() {
            if let Node::Connection(ref mut c) = t.begin { upd(c); }
            if let Node::Connection(ref mut c) = t.end { upd(c); }
            for sw in t.switches.iter_mut() { upd(&mut sw.connection); }
        }
    }

    Ok(())
}
