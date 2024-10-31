module Ast = Lib.Ast
module Pretty = Lib.Pretty
 
let _ = 
  let file = open_in "lib/srcProcessor/test.dolphin" in
  let buffer = Lexing.from_channel file in
  let prog = Parser.prog Lexer.token buffer in
  let _ = PrintBox_text.output stdout (Pretty.program_to_tree prog) in
  Printf.printf "\n"
