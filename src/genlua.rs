use std::fmt;

use crate::parser::*;

#[derive(Debug)]
pub enum GenError {
  Unknown(i32),
  UnimplementedFeature(usize, i32),
  FunctionBodyError(usize),
}
use GenError::*;
pub type GResult = Result<LuaCode, GenError>;

// simplified grammar for basic lua module //TODO: if, while, do, for
pub enum LuaCode {
  LuaModule(Module),
  LuaStmt(Stmt),
  LuaApp(App),
  LuaDef(Def),
  LuaAssign(Assign),
  LuaRet(Ret),
  LuaExpr(Expr),
  LuaFunc(Func),
  LuaIndex(Index),
  LuaLiteral(Literal),
  LuaList(Table<i64>),
  LuaIf(If),
  LuaWhile(While),
  //LuaFor(For),
  LuaDo(Do),
  LuaInfix(Infix),
  LuaEmpty,
}
use LuaCode::*;

impl LuaCode {
  fn stmt(&self) -> Stmt {
    match self {
      LuaStmt(stmt) => stmt.clone(),
      _ => panic!("code gen error"),
    }
  }
  fn expr(&self) -> Expr {
    match self {
      LuaExpr(expr) => expr.clone(),
      _ => panic!("code gen error"),
    }
  }
}


#[derive(Clone)]
pub struct Module {
  name: String,
  stmts: Vec<Stmt>,
}

#[derive(Clone)]
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


#[derive(Clone)]
pub struct App { //function application
  ident: String,
  args: Vec<Expr>,
}


#[derive(Clone)]
pub struct Def {
  ident: String,
  val: Expr,
}


#[derive(Clone)]
pub struct Assign {
  ident: String,
  val: Expr,
}


#[derive(Clone)]
pub struct Ret {
  val: Expr,
}


#[derive(Clone)]
pub enum Expr {
  FnCall(App),
  Ident(String),
  Lit(Literal),
  FnDef(Func),
  Access(Index),
  Op(Infix),
}


#[derive(Clone)]
pub struct Func {
  params: Vec<String>,
  body: Vec<Stmt>,
}


#[derive(Clone)]
pub struct Index {
  ident: String,
  field: Box<Expr>,
}


#[derive(Clone)]
pub enum Literal {
  Int(i64),
  Float(f64),
  Bool(bool),
  Str(String),
  List(Table<i64>),
}

#[derive(Clone)]
pub struct Table<K> {
  pairs: Vec<(K,Expr)>,
}

#[derive(Clone)]
pub struct If {
  cond: Expr,
  then: Vec<Stmt>,
  elseif: Vec<(Expr, Vec<Stmt>)>,
  lelse: Vec<Stmt>,
}


#[derive(Clone)]
pub struct While {
  cond: Expr,
  body: Vec<Stmt>,
}


//#[derive(Clone)]
//pub struct For {
//}


#[derive(Clone)]
pub struct Do {
  body: Vec<Stmt>,
}


#[derive(Clone)]
pub struct Infix {
  lhs: Box<Expr>,
  rhs: Box<Expr>,
  op: String,
}


impl fmt::Display for LuaCode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LuaModule(x) => x.fmt(f),
      LuaStmt(x) => x.fmt(f),
      LuaApp(x) => x.fmt(f),
      LuaDef(x) => x.fmt(f),
      LuaAssign(x) => x.fmt(f),
      LuaRet(x) => x.fmt(f),
      LuaExpr(x) => x.fmt(f),
      LuaFunc(x) => x.fmt(f),
      LuaIndex(x) => x.fmt(f),
      LuaLiteral(x) => x.fmt(f),
      LuaList(x) => x.fmt(f),
      LuaIf(x) => x.fmt(f),
      LuaWhile(x) => x.fmt(f),
      LuaDo(x) => x.fmt(f),
      LuaInfix(x) => x.fmt(f),
      LuaEmpty => write!(f, "")
    }
  }
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
      Literal::List(ls) => write!(f, "{}", ls),
    }
  }
}
impl<K: fmt::Display> fmt::Display for Table<K> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut s = String::from("{");
    for (k,v) in &self.pairs {
      s = format!("{}[{}] = {},",s,k,v);
    }
    s.push('}');
    write!(f,"{}",s)
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

pub fn gen_lua_module(name: String, sexpr: SExpr) -> GResult {
  match sexpr {
    SExpr::Module(sexprs) => {
      let stmts = {
        let mut vec: Vec<Stmt> = vec![];
        for s in sexprs {
          vec.push(gen_lua_stmt(*s)?.stmt());
        }
        vec
      };
      Ok(LuaModule(Module{name: name, stmts: stmts}))
    },
    SExpr::Main(sexprs) => Err(UnimplementedFeature(0,1)),
    _ => Err(Unknown(7)),
  }
}

pub fn gen_lua_stmt(sexpr: SExpr) -> GResult {
  match sexpr {
    SExpr::Stmt(line, sexprs) => {
      match &*sexprs[0] {
        SExpr::Ident(line1, id) => {
          let mut args: Vec<Expr> = vec![];
          for s in &sexprs[1..] {
            args.push(gen_lua_expr(*s.clone())?.expr());
          }
          Ok(LuaStmt(Stmt::FnCall(App{ident: id.clone(), args: args})))
        },
        SExpr::Builtin(line1, std) => match std {
          Std::Var => {
            let id = sexprs[1].get_ident().to_string();
            let expr = gen_lua_expr(*sexprs[2].clone())?.expr();
            Ok(LuaStmt(Stmt::VarDef(Def{ident: id, val: expr}))) 
          },
          _ => Err(UnimplementedFeature(*line1,2)),
        },
        _ => Err(UnimplementedFeature(line,3)),
      }
    },

    SExpr::Func(line, args, body) => Err(UnimplementedFeature(line,4)),
    SExpr::EOF => Ok(LuaEmpty),
    _ => Err(Unknown(8)),
  }
}

pub fn gen_lua_expr(sexpr: SExpr) -> GResult {
  match sexpr {
    SExpr::Int(line, i) => Ok(LuaExpr(Expr::Lit(Literal::Int(i)))),
    SExpr::Float(line, f) => Ok(LuaExpr(Expr::Lit(Literal::Float(f)))),
    SExpr::Bool(line, b) => Ok(LuaExpr(Expr::Lit(Literal::Bool(b)))),
    SExpr::Str(line, s) => Ok(LuaExpr(Expr::Lit(Literal::Str(s)))),
    SExpr::Char(line, s) => Ok(LuaExpr(Expr::Lit(Literal::Str(s)))),
    SExpr::Ident(line, s) => Ok(LuaExpr(Expr::Ident(s))),
    SExpr::List(line, sexprs) => {
      let mut ls: Vec<(i64, Expr)> = vec![];
      let mut ix = 1;
      for s in sexprs {
        ls.push((ix,gen_lua_expr(*s.clone())?.expr()));
      }
      Ok(LuaExpr(Expr::Lit(Literal::List(Table{pairs: ls}))))
    },
    SExpr::Stmt(line, sexprs) => {
      Err(UnimplementedFeature(line,5))
    },
    SExpr::Func(line, args, body) => {
      let mut params: Vec<String> = vec![];
      for arg in &args.get_list() {
        params.push(arg.get_ident().to_string());
      }
      let mut fbody: Vec<Stmt> = vec![];
      // TODO
      match &*body {
        SExpr::Stmt(_,_) => fbody.push(gen_lua_stmt(*body)?.stmt()),
        SExpr::StLs(_, stmts) => {
          for s in stmts {
            fbody.push(gen_lua_stmt(*s.clone())?.stmt());
          }
        },
        _ => return Err(FunctionBodyError(line)),
      }
      Ok(LuaExpr(Expr::FnDef(Func{params: params, body: fbody})))
    },
    _ => Err(Unknown(9)),
  }
}

pub fn gen_lua(sexpr: SExpr) -> GResult {
  match sexpr {
    SExpr::Int(line, i) => Ok(LuaLiteral(Literal::Int(i))),
    SExpr::Float(line, f) => Ok(LuaLiteral(Literal::Float(f))),
    SExpr::Bool(line, b) => Ok(LuaLiteral(Literal::Bool(b))),
    SExpr::Str(line, s) => Ok(LuaLiteral(Literal::Str(s))),
    SExpr::Char(line, s) => Ok(LuaLiteral(Literal::Str(s))),
    SExpr::Ident(line, s) => Ok(LuaExpr(Expr::Ident(s))),
    SExpr::Symbol(line, s) => Err(Unknown(6)), //todo
    SExpr::List(line, sexprs) => Err(UnimplementedFeature(line,6)),
    SExpr::Stmt(line, sexprs) => {
      Err(UnimplementedFeature(line,7))
    },

    SExpr::Func(line, args, body) => Err(UnimplementedFeature(line,8)),
    SExpr::Builtin(line, std) => Err(UnimplementedFeature(line,9)),
    SExpr::EOF => Ok(LuaEmpty),
    _ => Err(Unknown(8)),
  }
}
