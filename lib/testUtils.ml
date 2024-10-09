open Ast
open Semant
open Errors
module Pretty = Pretty
(* open Dolphin_compiler.TypedPretty *)

(*test name - test description - test program*)
type test_case = string * string * Ast.program
let print_integer = Ident {name = "print_integer"}
let read_integer = Ident {name = "read_integer"}
let int_x = Ident {name = "int_x"}
let int_y = Ident {name = "int_y"}
let int_z = Ident {name = "int_z"}
let int_t = Ident {name = "int_t"}
let int_u = Ident {name = "int_u"}
let int_v = Ident {name = "int_v"}
let int_p = Ident {name = "int_p"}
let int_q = Ident {name = "int_q"}

let bool_a = Ident {name = "bool_a"}
let bool_b = Ident {name = "bool_b"}
let bool_c = Ident {name = "bool_c"}
let bool_d = Ident {name = "bool_d"}

let sprint_err e = Printf.sprintf "%s" (error_to_string e)
let sprint_err_list el = List.fold_left ( ^ ) "" (List.map sprint_err el)

let run_testcase (name, desc, p) =
  let tprog, errors = typecheck_prog p in
  let _ = Printf.printf "======================================\n" in
  let _ = Printf.printf "RUNNING TEST ON %s\n" name in
  let _ = Printf.printf "TEST DESCRIPTION: %s\n" desc in
  let _ = PrintBox_text.output stdout (Pretty.program_to_tree p) in
  let _ = Printf.printf "\nTYPED PROGRAM: \n" in
  let _ = PrintBox_text.output stdout (TPretty.program_to_tree tprog) in
  if List.length !errors <> 0
  then 
    let err_list_str = sprint_err_list !errors in
    let _ = Printf.printf "\nERROR LIST: \n" in
    let _ = Printf.printf "%s\n" err_list_str in
    let oc = open_out ("bin/" ^ name ^ "/" ^ "output_actual.txt") in
    output_string oc err_list_str;
    close_out oc;
    exit 1
  else 
    let llprog = CodeGenerator.codegen_prog tprog in
    let _ = Printf.printf "\nLLVM code: \n" in
    let _ = Printf.printf "\n%s\n" (Ll.string_of_prog llprog) in
    let oc = open_out ("bin/" ^ name ^ "/" ^ "dolphin_main.ll") in
    output_string oc (Ll.string_of_prog llprog);
    close_out oc;
    exit 0
