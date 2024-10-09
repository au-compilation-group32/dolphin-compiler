open Lib.TestUtils
open Lib.Ast

let test1: test_case = 
  "test1",
  "all positive functionalities",
  [
    VarDeclStm {name = x; tp = None; body = Integer {int = 1L}};
    VarDeclStm {name = y; tp = None; body = Assignment {lvl = Var x; rhs = BinOp {left = Lval(Var x); op = Plus; right = Integer {int = 2L}}}};
    VarDeclStm {name = z; tp = None; body = BinOp {left = Lval (Var x); op = Plus; right = Lval (Var y)}};
    VarDeclStm {name = t; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var z)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var t)]})};
    IfThenElseStm {
      cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var y)})};
      elbro = None;
    };
    IfThenElseStm {
      cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var y)})};
      elbro = Some (ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var z)})});
    };
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var z)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var t)]})};
    ReturnStm {ret = Lval (Var t)}
  ]

let _ = run_testcase test1
