open Lib.TestUtils
open Lib.Ast

let test9: test_case = 
  "test9",
  "mics negative test",
  let delc11 = Declaration {name = int_x; tp = None; body = Integer {int = 419L}} in
  let delc_block1 = [delc11] in
  let declar_block1 : declaration_block = DeclBlock delc_block1 in
  let delc21 = Declaration {name = int_y; tp = None; body = Call {fname = print_integer; args = [Lval(Var int_x)]}} in
  let delc31 = Declaration {name = int_y; tp = Some Int; body = Call {fname = print_integer; args = [Lval(Var int_x)]}} in
  let delc_block3 = [delc21; delc31] in
  let declar_block3 : declaration_block = DeclBlock delc_block3 in
  [
    VarDeclStm declar_block1;
    ExprStm {expr = Some (Assignment {lvl = Var int_x; rhs = Call {fname = print_integer; args = [Lval(Var int_x)]}})};
    VarDeclStm declar_block3;

    ExprStm {expr = Some (Assignment {lvl = Var int_x; rhs = BinOp{left = Call {fname = print_integer; args = [Lval(Var int_x)]}; op = Eq; right = Lval(Var int_x)}})};

    ReturnStm {ret = Boolean {bool = true}}
  ]

let _ = run_testcase test9
