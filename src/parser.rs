use std::iter::*;

use crate::lexer::*;

#[derive(Debug)]
pub enum ParseError {
  Unknown(i32),
  MissingCloseParen,
  ExtraCloseParen,
}

const keywords: &'static [&'static str] = &[
  "fn", "require", "set", "let", "var", "macro" //macros are on the todo list
];
const builtins: &'static [&'static str] = &[
  ".", ":", "+", "-", "*", "/", "//", "^", "pow", "sqrt",
  "and", "or", "not", "==", "!=", "&&", "||", "!",
  "print", "type"
];

#[derive(Debug)]
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
  EOF,
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
  Ok(List(sexprs))
}

pub fn parse_sexpr<'a,I>(tokens: &mut Peekable<I>) -> PResult
where I: Iterator<Item=&'a Token>,
{
  match tokens.next() {
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
  }
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

fn stmt_parse(ostmt: PResult) -> PResult {
  match ostmt {
    Ok(Stmt(stmt)) => {
      match &*stmt[0] {
        Ident(id) => {
          if keywords.contains(&id.as_str()) {
            match id.as_str() {
              "fn" => {},
              _ => {},
            }
          }
        },
        _ => {},
      };
      Ok(Stmt(stmt))
    },
    _ => ostmt,
  }
}
