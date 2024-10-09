open Lib.TestUtils
open Lib.Ast

let test1: test_case = 
  "test1",
  "basic positive functionalities",
  [
    VarDeclStm {name = int_x; tp = None; body = Integer {int = 1L}};
    VarDeclStm {name = int_y; tp = None; body = Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}};
    VarDeclStm {name = int_z; tp = None; body = BinOp {left = Lval (Var int_x); op = Plus; right = Lval (Var int_y)}};
    VarDeclStm {name = int_t; tp = None; body = Call {fname = read_integer; args = []}};
    IfThenElseStm {
      cond = BinOp {left = Lval (Var int_x); op = Eq; right = Integer {int = 3L}};
      thbr = ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_t)]})};
      elbro = None;
    };
    VarDeclStm {name = int_u; tp = None; body = Integer {int = 0L}};
    IfThenElseStm {
      cond = BinOp {left = Lval (Var int_t); op = Le; right = Lval (Var int_x)};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var int_u; rhs = Lval (Var int_z)})};
      elbro = Some (ExprStm {expr = Some (Assignment {lvl = Var int_u; rhs = Lval (Var int_t)})});
    };
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_t)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_u)]})};
    ReturnStm {ret = Lval (Var int_u)}
  ]

let _ = run_testcase test1
