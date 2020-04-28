use std::iter::*;
use std::collections::HashMap;

use crate::lexer::*;

#[derive(Debug)]
pub enum ParseError {
  Unknown(i32), // here for debug purposes, should never be seen normally
  MissingCloseParen(usize),
  ExtraCloseParen(usize),
  UnimplementedFeature(usize),
  NotInScope(usize, String),
}

const KEYWORDS: &'static [&'static str] = &[
  "fn", "require", "set", "let", "var", "macro" //macros are on the todo list
];
const BUILTINS: &'static [&'static str] = &[
  ".", ":", "+", "-", "*", "/", "//", "^", "pow", "sqrt",
  "and", "or", "not", "==", "!=", "&&", "||", "!",
  "print", "type",
  "do", "for", "while", "if", "then", "else", "end", // 'then', 'else', 'end' are not KEYWORDS but cannot be used for lua compat
];

#[derive(Debug, Clone)]
pub enum Std {
  Add,
  Sub,
  Mul,
  Div,
  Pow,
  Sqrt,
  And,
  Or,
  Not,
  Eq,
  Neq,
  Print,
  Type,
  Do,
  For,
  While,
  If,
  Let,
  Set,
  Var,
  Dot, // . accessor
  Col, // : accessor //TODO: transform idents with dot or color into accessors so `(print x.i)` => `(print (. x i))` =lua> `print(x[i])`
}

#[derive(Debug, Clone)]
pub enum SExpr {
  //AST
  Int(usize, i64),
  Float(usize, f64),
  Bool(usize, bool),
  Str(usize, String),
  Char(usize, String),
  Ident(usize, String),
  Symbol(usize, String),
  List(usize, Vec<Box<SExpr>>), // A list that is treated as a list/table/vector: [ ... ]
  Stmt(usize, Vec<Box<SExpr>>), // A list that is treated as a function invocation: ( ... )

  StLs(usize, Vec<Box<SExpr>>), // A statement list, used in function bodies
  Func(usize, Box<SExpr>,Box<SExpr>),
  Builtin(usize, Std),
  Module(Vec<Box<SExpr>>), // A list representing a program module
  Main(Vec<Box<SExpr>>), // A list representing the main module
  EOF,
}

impl SExpr {
  pub fn is_ident(&self) -> bool {
    match self {
      Ident(_,_) => true,
      _ => false,
    }
  }
  pub fn get_ident(&self) -> &str {
    match self {
      Ident(_,id) => id.as_str(),
      _ => panic!("Not ident"),
    }
  }
  pub fn is_list(&self) -> bool {
    match self {
      List(_,_) => true,
      _ => false,
    }
  }
  pub fn get_list(&self) -> Vec<Box<SExpr>> {
    match self {
      List(_,v) => v.to_vec(),
      _ => panic!("Not list"),
    }
  }
}

pub type PResult = Result<SExpr, ParseError>;

use SExpr::*;

//macro_rules! hashmap {
//  ( $( $key:expr => $val:expr ),* ) => {{
//    let mut temp = HashMap::new();
//    $(
//      temp.insert($key,$val);
//    )*
//    temp
//  }}
//}
pub struct Env<'a> {
  data: HashMap<String, ()>,
  outer: Option<&'a Env<'a>>,
}
impl <'a> Env<'a> {
  fn default_env() -> Self {
    let mut data = HashMap::new();
    for i in BUILTINS {
      data.insert(i.to_string(), ());
    }
    Env{data: data, outer: None}
  }
  fn in_scope(&self, id: &String) -> bool {
    if self.data.contains_key(id) {
      true
    }
    else {
      let mut outer = &self.outer;
      while outer.is_some() {
        if outer.unwrap().data.contains_key(id) {
          return true;
        }
        else {
          outer = &outer.unwrap().outer;
        }
      }
      false
    }
  }
}

pub fn parse(tokens: Tokens) -> PResult {
  let mut sexprs: Vec<Box<SExpr>> = vec![]; // top-level s-expressions
  let mut iter = tokens.iter().peekable();
  //let mut token = iter.next();
  while iter.peek().is_some() {
    match parse_sexpr(&mut iter)? {
      EOF => {},
      sexpr => sexprs.push(Box::new(sexpr)),
    };
  }

  Ok(Module(sexprs))
}

pub fn parse_sexpr<'a,I>(tokens: &mut Peekable<I>) -> PResult
where I: Iterator<Item=&'a Token>,
{
  let sexpr = {match tokens.next() {
    Some(token) => match token {
      Token::OPAREN(line) => parse_stmt(*line, tokens), //stmt_parse(parse_stmt(tokens)),
      Token::OBRACKET(line) => parse_list(*line, tokens),
      Token::INT(line, n) => Ok(Int(*line, *n)),
      Token::FLOAT(line, n) => Ok(Float(*line, *n)),
      Token::IDENT(line, i) => parse_ident(*line, i.to_string()),
      Token::BOOL(line, b) => Ok(Bool(*line, *b)),
      Token::STRLIT(line, s) => Ok(Str(*line, s.to_string())),
      Token::CHLIT(line, c) => Ok(Char(*line, c.to_string())),
      Token::SYMBOL(line, s) => Ok(Symbol(*line, s.to_string())),
      Token::CPAREN(line) => Err(ParseError::ExtraCloseParen(*line)),
      Token::CBRACKET(line) => Err(ParseError::ExtraCloseParen(*line)),
      _ => Err(ParseError::Unknown(1)),
    },
    None => Ok(EOF),
  }}?;
  Ok(first_analysis(sexpr)?)
}

fn parse_list<'a,I>(line: usize, tokens: &mut Peekable<I>) -> PResult
where I: Iterator<Item=&'a Token>,
{
  let mut list: Vec<Box<SExpr>> = vec![];
  loop {
    match tokens.peek() {
      Some(Token::CBRACKET(line)) => {
        tokens.next();
        return Ok(List(*line, list));
      },
      Some(_) => {
        let sexpr = parse_sexpr(tokens)?;
        list.push(Box::new(sexpr));
      },
      None => return Err(ParseError::MissingCloseParen(line)),
    };
  }
  Err(ParseError::Unknown(2))
}
fn parse_stmt<'a,I>(line: usize, tokens: &mut Peekable<I>) -> PResult
where I: Iterator<Item=&'a Token>,
{
  let mut list: Vec<Box<SExpr>> = vec![];
  loop {
    match tokens.peek() {
      Some(Token::CPAREN(line)) => {
        tokens.next();
        return Ok(Stmt(*line, list));
      },
      Some(_) => {
        let sexpr = parse_sexpr(tokens)?;
        list.push(Box::new(sexpr));
      },
      None => return Err(ParseError::MissingCloseParen(line)),
    };
  }
  Err(ParseError::Unknown(3))
}

fn parse_ident(line: usize, id: String) -> PResult {
  match &id[..] {
    "let" => Ok(Builtin(line, Std::Let)),
    "var" => Ok(Builtin(line, Std::Var)),
    "set" => Ok(Builtin(line, Std::Set)),
    _ => Ok(Ident(line, id)),
  }
}

fn first_analysis(ostmt: SExpr) -> PResult { // translate things like function declaration into their proper type
  match ostmt {
    Stmt(line, stmt) => {
      if stmt[0].is_ident() && KEYWORDS.contains(&stmt[0].get_ident()) {
        match stmt[0].get_ident() {
          "fn" => {
            if stmt[1].is_ident() {
              let var = Box::new(Builtin(line, Std::Var));
              let name = stmt[1].clone();
              //let fndec = stmt[0].clone();
              let args = stmt[2].clone();
              let body = {
                if stmt.len() == 4 {
                  stmt[3].clone()
                }
                else {
                  let mut l = vec![];
                  for s in &stmt[3..] {
                    l.push(s.clone());
                  }
                  Box::new(StLs(line, l))
                }
              };
              let func = Box::new(Func(line, args, body));
              let res = Stmt(line, vec![var, name, func]);
              Ok(res)
            }
            else {
              //let fndec = stmt[0].clone();
              let args = stmt[1].clone();
              let body  = stmt[2].clone();
              let func = Func(line, args, body);
              Ok(func)
            }
          },
          id => {println!("{}",id); Err(ParseError::UnimplementedFeature(line))},
        }
      }
      else {
        Ok(Stmt(line, stmt))
      }
    },
    _ => Ok(ostmt),
  }
}
