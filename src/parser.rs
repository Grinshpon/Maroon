use std::iter::*;

use crate::lexer::*;

#[derive(Debug)]
pub enum ParseError {
  Unknown(i32), // here for debug purposes, should never be seen normally
  MissingCloseParen,
  ExtraCloseParen,
  UnimplementedFeature,
}

const keywords: &'static [&'static str] = &[
  "fn", "require", "set", "let", "var", "macro" //macros are on the todo list
];
const builtins: &'static [&'static str] = &[
  ".", ":", "+", "-", "*", "/", "//", "^", "pow", "sqrt",
  "and", "or", "not", "==", "!=", "&&", "||", "!",
  "print", "type"
];

#[derive(Debug, Clone)]
pub enum SExpr {
  Int(i64),
  Float(f64),
  Bool(bool),
  Str(String),
  Char(String),
  Ident(String),
  Symbol(String),
  List(Vec<Box<SExpr>>), // A list that is treated as a list/table/vector: [ ... ]
  Stmt(Vec<Box<SExpr>>), // A list that is treated as a function invocation: ( ... )
  Func(Box<SExpr>,Box<SExpr>),
  Module(Vec<Box<SExpr>>), // A list representing a program module
  Main(Vec<Box<SExpr>>), // A list representing the main module
  EOF,
}

impl SExpr {
  fn is_ident(&self) -> bool {
    match self {
      Ident(_) => true,
      _ => false,
    }
  }
  fn get_ident(&self) -> &str {
    match self {
      Ident(id) => id.as_str(),
      _ => panic!("Not ident"),
    }
  }
}

pub type PResult = Result<SExpr, ParseError>;

use SExpr::*;

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
      Token::OPAREN => parse_stmt(tokens), //stmt_parse(parse_stmt(tokens)),
      Token::OBRACKET => parse_list(tokens),
      Token::INT(n) => Ok(Int(*n)),
      Token::FLOAT(n) => Ok(Float(*n)),
      Token::IDENT(i) => Ok(Ident(i.to_string())), // parse_ident (var or fn or whatever)
      Token::BOOL(b) => Ok(Bool(*b)),
      Token::STRLIT(s) => Ok(Str(s.to_string())),
      Token::CHLIT(c) => Ok(Char(c.to_string())),
      Token::SYMBOL(s) => Ok(Symbol(s.to_string())),
      Token::CPAREN => Err(ParseError::ExtraCloseParen),
      Token::CBRACKET => Err(ParseError::ExtraCloseParen),
      _ => Err(ParseError::Unknown(1)),
    },
    None => Ok(EOF),
  }}?;
  Ok(first_analysis(sexpr)?)
}

fn parse_list<'a,I>(tokens: &mut Peekable<I>) -> PResult
where I: Iterator<Item=&'a Token>,
{
  let mut list: Vec<Box<SExpr>> = vec![];
  loop {
    match tokens.peek() {
      Some(Token::CBRACKET) => {
        tokens.next();
        return Ok(List(list));
      },
      Some(_) => {
        let sexpr = parse_sexpr(tokens)?;
        list.push(Box::new(sexpr));
      },
      None => return Err(ParseError::MissingCloseParen),
    };
  }
  Err(ParseError::Unknown(2))
}
fn parse_stmt<'a,I>(tokens: &mut Peekable<I>) -> PResult
where I: Iterator<Item=&'a Token>,
{
  let mut list: Vec<Box<SExpr>> = vec![];
  loop {
    match tokens.peek() {
      Some(Token::CPAREN) => {
        tokens.next();
        return Ok(Stmt(list));
      },
      Some(_) => {
        let sexpr = parse_sexpr(tokens)?;
        list.push(Box::new(sexpr));
      },
      None => return Err(ParseError::MissingCloseParen),
    };
  }
  Err(ParseError::Unknown(3))
}

fn parse_ident<'a,I>(tokens: &mut Peekable<I>) -> PResult
where I: Iterator<Item=&'a Token>,
{
  Err(ParseError::Unknown(4))
}

fn first_analysis(ostmt: SExpr) -> PResult { // translate things like function declaration into their proper type
  match ostmt {
    Stmt(stmt) => {
      if stmt[0].is_ident() && keywords.contains(&stmt[0].get_ident()) {
        match stmt[0].get_ident() {
          "fn" => {
            if stmt[1].is_ident() {
              let var = Box::new(Ident("var".to_string()));
              let name = stmt[1].clone();
              let fndec = stmt[0].clone();
              let args = stmt[2].clone();
              let body  = stmt[3].clone();
              let func = Box::new(Func(args, body));
              let res = Stmt(vec![var, name, func]);
              Ok(res)
            }
            else {
              //let fndec = stmt[0].clone();
              let args = stmt[1].clone();
              let body  = stmt[2].clone();
              let func = Func(args, body);
              Ok(func)
            }
          },
          id => {println!("{}",id); Err(ParseError::UnimplementedFeature)},
        }
      }
      else {
        Ok(Stmt(stmt))
      }
    },
    _ => Ok(ostmt),
  }
}
