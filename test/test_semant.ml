open Lib.Ast
open Lib.Semant
open Lib.Errors
module Pretty = Lib.Pretty
(* open Dolphin_compiler.TypedPretty *)

type test_case = string * Lib.Ast.program

let prog_1: test_case = "prog_1: Empty program" , []
let prog_2: test_case = "prog_2: Return bool", [ReturnStm {ret = Boolean {bool = true}}]
let prog_3: test_case = "prog_3: ",
  [
    ExprStm {expr = Some (BinOp {left = Integer {int = 0L}; op = Plus; right = Boolean {bool = false}})};
    ReturnStm {ret = Lval (Var (Ident {name = "x"}))}
  ]

let print_err e = let _ = Printf.printf "%s\n" (error_to_string e) in ()
let test_semant (name, p) =
  let tprog, errors = typecheck_prog p in
  let _ = Printf.printf "======================================\n" in
  let _ = Printf.printf "RUNNING TEST ON %s\n" name in
  let _ = PrintBox_text.output stdout (Pretty.program_to_tree p) in
  let _ = Printf.printf "\nTYPED PROGRAM: \n" in
  let _ = PrintBox_text.output stdout (TPretty.program_to_tree tprog) in
  let _ = Printf.printf "\nERROR LIST: \n" in
  let _ = List.map print_err !errors in
  let _ = Printf.printf "\n" in ()

let progs = [prog_1; prog_2; prog_3]
let _ = List.map test_semant progs
