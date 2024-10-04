open Dolphin_compiler.Ast
open Dolphin_compiler.Semant
open Dolphin_compiler.Errors

let prog_1 = "prog_1: Empty program" , []
let prog_2 = "prog_2: Return bool", [ReturnStm {ret = Boolean {bool = true}}]
let prog_3 = "prog_3: ",
  [
    ExprStm {expr = Some (BinOp {left = Integer {int = 0L}; op = Plus; right = Boolean {bool = false}})};
    ReturnStm {ret = Lval (Var (Ident {name = "x"}))}
  ]

let print_err e = let _ = Printf.printf "%s\n" (error_to_string e) in ()
let test_semant (name, p) =
  let errors = typecheck_prog p in
  let _ = Printf.printf "Running test on %s\n" name in
  let _ = List.map print_err !errors in
  let _ = Printf.printf "\n" in ()

let progs = [prog_1; prog_2; prog_3]
let _ = List.map test_semant progs
