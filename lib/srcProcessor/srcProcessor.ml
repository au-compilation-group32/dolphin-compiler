module Ast = Lib.Ast
module Pretty = Lib.Pretty
module Location = Lib.Location
module Lexer = Lexer
module Parser = Parser
 
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
  Parser.prog Lexer.token buffer

(* let _ = 
  let file = open_in "lib/srcProcessor/test.dolphin" in
  let buffer = Lexing.from_channel file in
  let prog = Parser.prog Lexer.token buffer in
  let _ = PrintBox_text.output stdout (Pretty.program_to_tree prog) in
  let _ = Printf.printf "\n==========================\n" in
  let _ = Printf.printf "Location of stms :\n" in
  let _ = print_loc_list prog in
  Printf.printf "\n" *)
