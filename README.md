# Maroon

Maroon is a scheme-inspired lisp with a (WIP) backend that compiles to Lua.

## TODO List:

- [x] Lexing
- [ ] Parsing
  - [x] First Pass Parser (forming AST)
  - [ ] Semantic Analysis (forming SAST or Semantically checked AST)
    - [ ] Scope Checking
    - [ ] Type Checking
- [ ] Code Generation
  - [ ] Lua Backend
    - [ ] AST Conversion
    - [x] Printing
- [ ] Error Handling
  - [x] Error types
  - [ ] Displaying Errors
