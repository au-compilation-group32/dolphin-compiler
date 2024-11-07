module Ast = Lib.Ast
module Semant = Lib.Semant
module Errors = Lib.Errors
module Pretty = Lib.Pretty
module TPretty = Lib.TypedPretty
module SrcProcessor = SrcProcessor
module CodeGen = Lib.CodeGenerator
module Ll = Lib.Ll

let sprint_err e = Printf.sprintf "%s\n" (Errors.error_to_string e)
let sprint_err_list el = List.fold_left ( ^ ) "" (List.rev(List.map sprint_err el))
let handle_errors errors =
  let err_list_str = sprint_err_list errors in
  let _ = Printf.printf "\nERROR LIST: \n" in
  let _ = Printf.printf "%s\n" err_list_str in
  exit 1
let compile_prog src_file = 
  let _ = Printf.printf "\n============================================================================\n" in
  let _ = Printf.printf "RUNNING TEST ON %s\n" src_file in
  match SrcProcessor.src_file_to_ast src_file with
  | LexFailure error -> handle_errors [error]
  | LexSuccess prog -> 
    let _ = Printf.printf "\nAST:\n" in
    let _ = PrintBox_text.output stdout (Pretty.program_to_tree prog) in
    let tprog, errors = Semant.typecheck_prog prog in
    let _ = Printf.printf "\nTYPED PROGRAM: \n" in
    let _ = PrintBox_text.output stdout (TPretty.program_to_tree tprog) in
    if List.length !errors <> 0
    then handle_errors !errors
    else 
      let llprog = CodeGen.codegen_prog tprog in
      let _ = Printf.printf "\nLLVM code: \n" in
      let _ = Printf.printf "\n%s\n" (Ll.string_of_prog llprog) in
      exit 0

let _ = compile_prog  Sys.argv.(1)
