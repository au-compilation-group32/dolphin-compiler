open Lib.TestUtils
open Lib.Ast

let test11: test_case = 
  "test11",
  "nested function call",
  [
    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};

    ExprStm {expr = Some (
      Assignment {
        lvl = Var int_x; 
        rhs = Assignment {
          lvl = Var int_y; 
          rhs = Assignment {
            lvl = Var int_z; 
            rhs = Lval (Var int_t)}}})};


    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_t)]})};
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test11
