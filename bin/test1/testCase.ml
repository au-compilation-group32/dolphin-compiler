open Lib.TestUtils
open Lib.Ast

let test1: test_case = 
  "test1",
  "basic positive functionalities",
  let delc1 = Declaration {name = int_x; tp = None; body = Integer {int = 1L}} in
  let delc2 = Declaration {name = int_y; tp = None; body = Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}} in
  let delc3 = Declaration {name = int_z; tp = None; body = BinOp {left = Lval (Var int_x); op = Plus; right = Lval (Var int_y)}} in
  let delc4 = Declaration {name = int_t; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block = [delc1; delc2; delc3; delc4] in
  let declar_block : declaration_block = DeclBlock delc_block in
  let delc5 = Declaration {name = int_u; tp = None; body = Integer {int = 0L}} in
  let delc_block2 = [delc5] in
  let declar_block2 : declaration_block = DeclBlock delc_block2 in
  [
    VarDeclStm declar_block;
    IfThenElseStm {
      cond = BinOp {left = Lval (Var int_x); op = Eq; right = Integer {int = 3L}};
      thbr = ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_t)]})};
      elbro = None;
    };
    VarDeclStm declar_block2;
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
