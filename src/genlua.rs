use std::fmt;

use crate::parser::*;

pub enum GenError {
  Unknown(i32),
}
use GenError::*;
pub type GResult = Result<LuaModule, GenError>;

pub struct LuaModule {
  name: String,
}

impl fmt::Display for LuaModule {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "TODO")
  }
}

pub fn gen_lua(ast: SExpr) -> GResult {
  Err(Unknown(1))
}
