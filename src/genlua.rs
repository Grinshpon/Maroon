use std::fmt;

use crate::parser::*;

pub enum GenError {
  Unknown(i32),
}
use GenError::*;
pub type GResult = Result<Module, GenError>;

// simplified grammar for basic lua module
pub struct Module {
  name: String,
  stmts: Vec<Stmt>,
}

pub enum Stmt {
  FnCall(App),
  VarDef(Def),
  VarAssign(Assign),
  FnRet(Ret),
}

pub struct App { //function application
  ident: String,
  args: Vec<Expr>,
}

pub struct Def {
  ident: String,
  val: Expr,
}

pub struct Assign {
  ident: String,
  val: Expr,
}

pub struct Ret {
  val: Expr,
}

pub enum Expr {
  FnCall(App),
  Ident(String),
  Lit(Literal),
  FnDef(Func),
  Access(Index),
}

pub struct Func {
  params: Vec<String>,
  body: Vec<Stmt>,
}

pub struct Index {
  ident: String,
  field: Box<Expr>,
}

pub enum Literal {
  Int(i64),
  Float(f64),
  Bool(bool),
  Str(String),
}

impl fmt::Display for Module {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut module = String::new();
    for s in &self.stmts {
      module = format!("{}{}", module, s);
      module.push('\n');
    }
    write!(f,"{}",module)
  }
}
impl fmt::Display for Stmt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Stmt::FnCall(app) => write!(f, "{}", app),
      Stmt::VarDef(def) => write!(f, "{}", def),
      Stmt::VarAssign(a)=> write!(f, "{}", a),
      Stmt::FnRet(r) => write!(f, "{}", r),
    }
  }
}
impl fmt::Display for App {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut args = String::new();
    for a in &self.args {
      args = format!("{}{}",args,a);
      args.push(',');
    }
    args.pop(); // remove last comma
    write!(f, "{}({})", self.ident, args)
  }
}
impl fmt::Display for Def {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "local {} = {}", self.ident, self.val)
  }
}
impl fmt::Display for Assign {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.ident, self.val)
  }
}
impl fmt::Display for Ret {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "return {}", self.val)
  }
}
impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Expr::FnCall(app) => write!(f, "{}", app),
      Expr::Ident(s) => write!(f, "{}", s),
      Expr::Lit(l) => write!(f, "{}", l),
      Expr::FnDef(func) => write!(f, "{}", func),
      Expr::Access(i) => write!(f, "{}", i),
    }
  }
}
impl fmt::Display for Func {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut s = String::from("function(");
    for p in &self.params {
      s = [s,p.to_string()].concat();
      s.push(',');
    }
    s.pop();
    s.push_str(") ");
    for b in &self.body {
      s = format!("{}{}",s,b);
      s.push_str("; ");
    }
    s.push_str("end\n");
    write!(f, "{}", s)
  }
}
impl fmt::Display for Index {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &*self.field {
      Expr::Ident(s) => write!(f, "{}.{}", self.ident, s),
      _ => write!(f, "{}[{}]", self.ident, self.field),
    }
  }
}
impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //let mut s = format!("");
    match self {
      Literal::Int(n) => write!(f,"{}",n),
      Literal::Float(n) => write!(f, "{}", n),
      Literal::Bool(b) => write!(f, "{}", b),
      Literal::Str(s) => write!(f, "\"{}\"", s),
    }
  }
}

pub fn gen_lua(ast: SExpr) -> GResult {
  Err(Unknown(1))
}
