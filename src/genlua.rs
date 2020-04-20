use std::fmt;

use crate::parser::*;

pub enum GenError {
  Unknown(i32),
}
use GenError::*;
pub type GResult = Result<Module, GenError>;

// simplified grammar for basic lua module //TODO: if, while, do, for
pub struct Module {
  name: String,
  stmts: Vec<Stmt>,
}

pub enum Stmt {
  FnCall(App),
  VarDef(Def),
  VarAssign(Assign),
  FnRet(Ret),
  CondIf(If),
  CondWhile(While),
  //CondFor(For), //TODO
  DoBlock(Do),
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
  Op(Infix),
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

pub struct If {
  cond: Expr,
  then: Vec<Stmt>,
  elseif: Vec<(Expr, Vec<Stmt>)>,
  lelse: Vec<Stmt>,
}

pub struct While {
  cond: Expr,
  body: Vec<Stmt>,
}

//pub struct For {
//}

pub struct Do {
  body: Vec<Stmt>,
}

pub struct Infix {
  lhs: Box<Expr>,
  rhs: Box<Expr>,
  op: String,
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
      Stmt::CondIf(c) => write!(f, "{}", c),
      Stmt::CondWhile(c) => write!(f, "{}", c),
      Stmt::DoBlock(d) => write!(f, "{}", d),
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
      Expr::Op(op) => write!(f, "{}", op),
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
impl fmt::Display for If {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut s = String::from("if ");
    s = format!("{}{} then\n", s, self.cond);
    for t in &self.then {
      s = format!("{}{}\n",s,t);
    }
    if self.elseif.len() > 0 {
      for (c,st) in &self.elseif {
        s = format!("{}elseif {} then\n",s, c);
        for t in st {
          s = format!("{}{}\n",s,t);
        }
      }
    }
    if self.lelse.len() > 0 {
      for t in &self.lelse {
        s = format!("{}{}\n",s,t);
      }
    }
    write!(f,"{}end ", s)
  }
}
impl fmt::Display for While {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut s = String::from("while ");
    s = format!("{}{} do\n", s, self.cond);
    for t in &self.body {
      s = format!("{}{}\n", s, t);
    }
    write!(f,"{}end",s)
  }
}
impl fmt::Display for Do {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut s = String::from("do\n");
    for t in &self.body {
      s = format!("{}{}\n", s, t);
    }
    write!(f,"{}end",s)
  }
}
impl fmt::Display for Infix {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, " ({}) {} ({}) ", self.lhs, self.op, self.rhs)
  }
}

pub fn gen_lua(ast: SExpr) -> GResult {
  Err(Unknown(6))
}
