open Lib.TestUtils
open Lib.Ast

let test9: test_case = 
  "test9",
  "mics negative test",
  [
    VarDeclStm {name = int_x; tp = None; body = Integer {int = 419L}};
    ExprStm {expr = Some (Assignment {lvl = Var int_x; rhs = Call {fname = print_integer; args = [Lval(Var int_x)]}})};
    VarDeclStm {name = int_y; tp = None; body = Call {fname = print_integer; args = [Lval(Var int_x)]}};
    VarDeclStm {name = int_y; tp = Some Int; body = Call {fname = print_integer; args = [Lval(Var int_x)]}};

    ExprStm {expr = Some (Assignment {lvl = Var int_x; rhs = BinOp{left = Call {fname = print_integer; args = [Lval(Var int_x)]}; op = Eq; right = Lval(Var int_x)}})};

    ReturnStm {ret = Boolean {bool = true}}
  ]

let _ = run_testcase test9
