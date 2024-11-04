open Lib.TestUtils
open Lib.Ast

let test11: test_case = 
  "test11",
  "nested function call",
  let delc11 = Declaration {name = int_x; tp = None; body = Integer {int = 100L}} in
  let delc21 = Declaration {name = int_y; tp = None; body = Integer {int = 69L}} in
  let delc31 = Declaration {name = int_z; tp = None; body = Integer {int = 419L}} in
  let delc41 = Declaration {name = int_t; tp = None; body = Integer {int = 10L}} in
  let delc_block4 = [delc11; delc21; delc31; delc41] in
  let declar_block4 : declaration_block = DeclBlock delc_block4 in
  [
    VarDeclStm declar_block4;

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
