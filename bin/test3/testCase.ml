open Lib.TestUtils

let test3: test_case = 
  "test3",
  "return bool",
  [
    ReturnStm {ret = Boolean {bool = true}}
  ]

let _ = run_testcase test3
