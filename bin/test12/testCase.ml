open Lib.TestUtils
open Lib.Ast

let test12: test_case = 
  "test12",
  "None ExprStm",
  [
    ExprStm {expr = None};
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test12
