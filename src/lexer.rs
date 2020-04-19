use std::iter::*;
use std::str::Chars;

#[derive(Debug)]
pub enum LexError {
  UnexpectedChar(usize, char),
  InvalidIdentifier(usize),
  InvalidNumber(usize),
  IncompleteChar(usize),
}

#[derive(Debug)]
pub enum Token {
  OPAREN(usize),
  CPAREN(usize),
  OBRACKET(usize),
  CBRACKET(usize),
  OBRACE(usize),
  CBRACE(usize),
  INT(usize,i64),
  FLOAT(usize,f64),
  IDENT(usize,String),
  BOOL(usize,bool),
  STRLIT(usize,String),
  CHLIT(usize,String),
  SYMBOL(usize,String),
}

pub type Tokens = Vec<Token>;

pub fn lex(src: &mut String) -> Result<Tokens, LexError> {
  let mut tokens: Tokens = vec![];
  let mut line = 1;

  let mut chars = src.chars().peekable();

  let mut oc = chars.next();
  //let mut c = '';
  while oc.is_some() {
    match oc.unwrap() {
      '\n'=> {line+=1;},
      ';' => {lex_comment(line, &mut chars);},
      '(' => {tokens.push(Token::OPAREN(line));},
      ')' => {tokens.push(Token::CPAREN(line));},
      '[' => {tokens.push(Token::OBRACKET(line));},
      ']' => {tokens.push(Token::CBRACKET(line));},
      '{' => {tokens.push(Token::OBRACE(line));},
      '}' => {tokens.push(Token::CBRACE(line));},
      //'.' => {tokens.push(Token::SYMBOL(".");}
      '\''=> {lex_char(line, &mut chars, &mut tokens);},
      '"' => {lex_str(line, &mut chars, &mut tokens);},
      c   => {
        if c.is_numeric() {
          lex_num(line, c, &mut chars, &mut tokens);
        }
        else if c.is_alphabetic() {
          lex_ident(line, c, &mut chars, &mut tokens);
        }
        else if !c.is_whitespace() {
          lex_symbol(line, c, &mut chars, &mut tokens);
        }
      },
    }
    oc = chars.next();
  }
  Ok(tokens)
}

fn lex_num(line: usize, ic: char, chars: &mut Peekable<Chars>, tokens: &mut Tokens) -> Option<LexError> {
  let mut acc = String::new();
  let mut is_float = false;
  let mut is_hex = false;
  acc.push(ic);
  let mut oc = chars.peek();
  while oc.is_some() {
    let c = oc.unwrap();
    if c.is_numeric() {
      acc.push(*c);
    }
    else if c == &'x' {
      if is_hex || ic != '0' {
        return Some(LexError::InvalidNumber(line));
      }
      else {
        is_hex = true;
        acc.push(*c);
      }
    }
    else if c == &'.' {
      if is_float {
        return Some(LexError::InvalidNumber(line));
      }
      else {
        is_float = true;
        acc.push(*c);
      }
    }
    else if c.is_alphabetic() {
      return Some(LexError::InvalidNumber(line));
    }
    else {
      oc = None
    }
    if oc.is_some() {
      chars.next();
      oc = chars.peek();
    }
  }
  if is_float {
    tokens.push(Token::FLOAT(line, acc.parse::<f64>().unwrap()));
  }
  else {
    tokens.push(Token::INT(line, acc.parse::<i64>().unwrap()));
  }
  None
}
fn lex_ident(line: usize, ic: char, chars: &mut Peekable<Chars>, tokens: &mut Tokens) -> Option<LexError> {
  let mut acc = String::new();
  acc.push(ic);
  let mut oc = chars.peek();
  while oc.is_some() {
    let c = oc.unwrap();
    if c.is_alphanumeric() || c == &'.' {
      acc.push(*c);
    }
    else {
      oc = None;
    }
    if oc.is_some() {
      chars.next();
      oc = chars.peek();
    }
  }
  tokens.push(Token::IDENT(line, acc));
  None
}
fn lex_char(line: usize, chars: &mut Peekable<Chars>, tokens: &mut Tokens) -> Option<LexError> {
  let mut acc = String::new();
  let mut oc = chars.next();
  while oc.is_some() {
    let c = oc.unwrap();
    if c == '\'' {
      oc = None;
    }
    else if c == '\\' {
      acc.push(c);
      oc = chars.next();
      match oc {
        None => {return Some(LexError::IncompleteChar(line));},
        Some(c) => {acc.push(c);},
      }
    }
    else {
      acc.push(c);
    }
    if oc.is_some() {
      oc = chars.next();
    }
  }
  tokens.push(Token::CHLIT(line, acc));
  None
}
fn lex_str(line: usize, chars: &mut Peekable<Chars>, tokens: &mut Tokens) -> Option<LexError> {
  let mut acc = String::new();
  let mut oc = chars.next();
  while oc.is_some() {
    let c = oc.unwrap();
    if c == '\"' {
      oc = None;
    }
    else if c == '\\' {
      acc.push(c);
      oc = chars.next();
      match oc {
        None => {return Some(LexError::IncompleteChar(line));},
        Some(c) => {acc.push(c);},
      }
    }
    else {
      acc.push(c);
    }
    if oc.is_some() {
      oc = chars.next();
    }
  }
  if acc == "true" {
    tokens.push(Token::BOOL(line, true));
  }
  else if acc == "false" {
    tokens.push(Token::BOOL(line, false));
  }
  else {
    tokens.push(Token::STRLIT(line, acc));
  }


  None
}
fn lex_comment(_line: usize, chars: &mut Peekable<Chars>) -> Option<LexError> {
  let mut oc = chars.next();
  while oc.is_some() {
    if oc.unwrap() == '\n' {
      oc = None
    }
    if oc.is_some() {
      oc = chars.next();
    }
  }
  None
}
fn lex_symbol(line: usize, ic: char, chars: &mut Peekable<Chars>, tokens: &mut Tokens) -> Option<LexError> {
  let mut acc = String::new();
  acc.push(ic);
  let mut oc = chars.peek();
  while oc.is_some() {
    let c = oc.unwrap();
    if !c.is_alphanumeric() && !c.is_whitespace() {
      acc.push(*c);
    }
    else {
      oc = None;
    }
    if oc.is_some() {
      chars.next();
      oc = chars.peek();
    }
  }
  tokens.push(Token::SYMBOL(line, acc));
  None
}
