open Lib.TestUtils
open Lib.Ast

let test16: test_case = 
  "test16",
  "while loop with always true",
  let delc1 = Declaration {name = int_i; tp = None; body = Integer {int = 0L}} in
  let delc_block = [delc1] in
  let declar_block1 = DeclBlock delc_block in
  [
    VarDeclStm declar_block1;

    WhileStm {cond = Boolean {bool = true};
            body = CompoundStm {stms = [
              IfThenElseStm{cond = BinOp{left = Lval (Var int_i); op = Ge; right = Integer {int = 5L}}; thbr = BreakStm; elbro = None};
              ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
              ExprStm{expr = Some (Assignment{lvl = Var int_i; rhs = BinOp{left = Lval (Var int_i); op = Plus; right = Integer {int = 1L}}})};
            ]}
    };

    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test16
