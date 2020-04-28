use std::fs::File;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::env;

pub mod lexer;
use crate::lexer::*;

pub mod parser;
use crate::parser::*;

pub mod genlua;
use crate::genlua::*;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() > 1 {
    let mut file = File::open(args[1].clone()).expect("Unable to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Unable to read file");
    //println!("{}\n", contents);
    match lex(&mut contents) {
      Ok(tokens) => {
        //println!("{:?}\n", tokens);
        match parse(tokens) {
          Ok(program) => {
            //println!("{:?}\n",program);
            match gen_lua_module(String::from("result"), program) {
              Ok(lua) => {
                //println!("{}",lua)
                let mut result = OpenOptions::new()
                              .write(true)
                              .create(true)
                              .truncate(true)
                              .open("result.lua")
                              .expect("Error writing file");
                result.write_all(format!("{}",lua).as_bytes()).expect("Error writing file");
              },
              Err(err) => println!("Code Gen Error: {:?}", err),
            }
          },
          Err(err) => {println!("Parse Error: {:?}", err);},
        }
      },
      Err(err) => {println!("Lex Error: {:?}", err);}
    }
  }
  else {
    println!("No file given"); //TODO: REPL
  }
}
