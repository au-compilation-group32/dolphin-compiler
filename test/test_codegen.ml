open Lib.Ast
open Lib.Semant
open Lib.Errors
module Pretty = Lib.Pretty
(* open Dolphin_compiler.TypedPretty *)

type test_case = string * Lib.Ast.program

let prog_1: test_case = "prog_1: Return 0", [ReturnStm {ret = Integer {int = 0L}}]
let prog_2: test_case = "prog_2: Return 1", [ReturnStm {ret = Integer {int = 1L}}]
let prog_3: test_case = "prog_3: ",
  let x = Ident {name = "x"} in
  let y = Ident {name = "y"} in
  let z = Ident {name = "z"} in
  [
    VarDeclStm {name = x; tp = None; body = Integer {int = 1L}};
    VarDeclStm {name = y; tp = None; body = Lval (Var x)};
    VarDeclStm {name = z; tp = None; body = BinOp {left = Lval (Var x); op = Plus; right = Lval (Var y)}};
    ReturnStm {ret = Lval (Var z)}
  ]

let print_err e = let _ = Printf.printf "%s\n" (error_to_string e) in ()
let test_codegen (name, p) =
  let tprog, errors = typecheck_prog p in
  let _ = Printf.printf "======================================\n" in
  let _ = Printf.printf "RUNNING TEST ON %s\n" name in
  let _ = PrintBox_text.output stdout (TPretty.program_to_tree tprog) in
  if List.length !errors <> 0
  then 
    let _ = Printf.printf "\nERROR LIST: \n" in
    let _ = List.map print_err !errors in
    let _ = Printf.printf "\n" in
    ()
  else 
    let llprog = Lib.CodeGenerator.codegen_prog tprog in
    let _ = Printf.printf "\nLLVM code: \n" in
    let _ = Printf.printf "\n%s\n" (Lib.Ll.string_of_prog llprog) in
    ()

let progs = [prog_1; prog_2; prog_3]
let _ = List.map test_codegen progs
