open Lib.TestUtils
open Lib.Ast

let test10: test_case = 
  "test10",
  "nested function call",
  [
    ExprStm {expr = Some (Call {fname = print_integer; args = [Call {fname = read_integer; args = []}]})};
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test10
