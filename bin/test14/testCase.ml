open Lib.TestUtils
open Lib.Ast

let test14: test_case = 
  "test14",
  "full for loop",
  let delc1 = Declaration {name = int_x; tp = None; body = Integer {int = 0L}} in
  let delc_block = [delc1] in
  let declar_block1 = DeclBlock delc_block in
  [
    ForStm {init = Some (FIDecl declar_block1);
            cond = Some (BinOp{left = Lval (Var int_x); op = Lt; right = Integer {int = 5L}});
            update = Some (Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 1L}}});
            body = ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})}};

    VarDeclStm declar_block1;
    ForStm {init = Some (FIExpr (Assignment {lvl = Var int_x; rhs = Integer {int = 0L}}));
            cond = Some (BinOp{left = Lval (Var int_x); op = Lt; right = Integer {int = 5L}});
            update = Some (Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 1L}}});
            body = ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test14
