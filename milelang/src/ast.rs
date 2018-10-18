#[derive(Debug)]
pub struct Statement {
    pub action :String,
    pub name :Option<String>,
    pub mods :Vec<String>,
    pub conns: Vec<Connection>,
    pub args: Vec<(String, Box<Expr>)>,
    pub block: Option<Vec<Statement>>,
}

#[derive(Debug)]
pub enum Connection {
    Anonymous,
    Stop,
    Named(String),
}

#[derive(Debug)]
pub enum Expr {
    Var(String),
    Value(Value),
    // Add(Box<Expr>,Box<Expr>),
    // [...]
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(f64),
    Text(String),
}



//
//
//#[derive(Debug)]
//pub enum Statement {
//    RailmlFile(String),
//    Line(Vec<LineStmt>),
//}
//
//#[derive(Debug)]
//pub enum Expr {
//    Var(String),
//}
//
//#[derive(Debug)]
//pub struct LineStmt {
//    pub id :String,
//    pub mods :Vec<String>,
//    pub args :Vec<(String,Box<Expr>)>,
//    pub block :Option<Vec<LineStmt>>,
//}


//
// sym par signal(x=500.0,dir=up);
// 
// loop #l1 (x=500.0, l=100.0) {
//   signal(x=10.0);
// };
//
// left outgoing crossover <cross1> (x=200.0,l=100.0);
// right incoming crossover <cross1> (x=300.0);
//
// loop(x=500.0,l=100.0);
//
// line <a,_> (l=1000.0) {
//   
// };
//
// double line <a, b> (l=2)  {
//   
// };
//
// railml(file="test.railml");
//
// line #ext1 <b,!> (l=200.0);
// left siding <!> (x=60.0,l=20);
//
// add (to=ext1) {
//   sym par signal(x=500.0, dir=up);
// };
//
