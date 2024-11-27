module Ast = Lib.Ast
module Semant = Semant
module Errors = Lib.Errors
module Pretty = Lib.Pretty
module TPretty = Lib.TypedPretty
module SrcProcessor = SrcProcessor
module CodeGen = CodeGenerator
module Ll = Lib.Ll

let sprint_err e = Printf.sprintf "%s\n" (Errors.error_to_string e)
let sprint_err_list el = List.fold_left ( ^ ) "" (List.rev(List.map sprint_err el))
let handle_errors test_dir errors =
  let err_list_str = sprint_err_list errors in
  let _ = Printf.printf "\nERROR LIST: \n" in
  let _ = Printf.printf "%s\n" err_list_str in
  let oc = open_out (test_dir ^ "/output_actual.txt") in
  output_string oc err_list_str;
  close_out oc;
  exit 1
let run_testcase test_dir = 
  let _ = Printf.printf "\n============================================================================\n" in
  let _ = Printf.printf "RUNNING TEST ON %s/main.dlp \n" test_dir in
  match SrcProcessor.src_file_to_ast (test_dir ^ "/main.dlp") with
  | LexFailure error -> handle_errors test_dir [error]
  | LexSuccess prog -> 
    let _ = Printf.printf "\nAST:\n" in
    let _ = PrintBox_text.output stdout (Pretty.program_to_tree prog) in
    let tprog, rec_names, errors = Semant.typecheck_prog prog in
    let _ = Printf.printf "\nTYPED PROGRAM: \n" in
    let _ = PrintBox_text.output stdout (TPretty.program_to_tree tprog) in
    if List.length !errors <> 0
    then handle_errors test_dir !errors
    else 
      let llprog = CodeGen.codegen_prog tprog rec_names in
      let _ = Printf.printf "\nLLVM code: \n" in
      let _ = Printf.printf "\n%s\n" (Ll.string_of_prog llprog) in
      let oc = open_out (test_dir ^ "/dolphin_main.ll") in
      output_string oc (Ll.string_of_prog llprog);
      close_out oc;
      exit 0
