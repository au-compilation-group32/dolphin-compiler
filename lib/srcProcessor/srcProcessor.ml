module Ast = Lib.Ast
module Pretty = Lib.Pretty
module Location = Lib.Location
module Lexer = Lexer
module Parser = Parser
module Errors = Lib.Errors
 
exception Unimplemented

type lexResult =
| LexSuccess of Ast.statement list
| LexFailure of Errors.error

let get_loc_from_stm = function 
  | Ast.ExprStm {expr = _; loc} -> loc
  | Ast.VarDeclStm (Ast.DeclBlock{declarations = _; loc}) -> loc
  | Ast.IfThenElseStm {cond =  _; thbr = _; elbro = _; loc} -> loc
  | Ast.WhileStm{cond =  _; body = _; loc} -> loc
  | Ast.ForStm{init = _; cond =  _; update = _; body= _; loc} -> loc
  | Ast.BreakStm{loc} -> loc
  | Ast.ContinueStm{loc} -> loc
  | Ast.CompoundStm{stms = _; loc} -> loc
  | Ast.ReturnStm{ret = _; loc} -> loc

let print_loc stm = PrintBox_text.output stdout (Location.location_to_tree (get_loc_from_stm stm))

let rec print_loc_list = function
  | [] -> ()
  | h::t -> 
    let _ = print_loc h in
    print_loc_list t

let src_file_to_ast file_name = 
  let file = open_in file_name in
  let buffer = Lexing.from_channel file in
  try
    let result = Parser.prog Lexer.token buffer in
    LexSuccess result
  with
  | Lexer.UnexpectedCharacter (loc, c) -> LexFailure (Errors.LexerUnexpectedCharacter {loc = loc; c = c})
  | Lexer.IntegerOutOfRange(loc, str) -> LexFailure (Errors.LexerIntegerOutOfRange{loc = loc; str = str})
  | Parser.Error ->
    let loc = Location.{start_pos = (Lexing.lexeme_start_p buffer); end_pos = (Lexing.lexeme_end_p buffer)} in
    let c = Lexing.lexeme_char buffer 0 in
    LexFailure (Errors.ParserSyntaxError{loc = loc; c = c})
