open Lib.TestUtils
open Lib.Ast

let test13: test_case = 
  "test13",
  "while loop",
  let delc1 = Declaration {name = int_i; tp = None; body = Integer {int = 0L}} in
  let delc_block = [delc1] in
  let declar_block1 = DeclBlock delc_block in

  [
    VarDeclStm declar_block1;
    WhileStm { 
      cond = BinOp{left = Lval (Var int_i); op = Lt; right = Integer {int = 5L}};
      body = CompoundStm {stms = [
        ExprStm{expr = Some (Assignment{lvl = Var int_i; rhs = BinOp{left = Lval (Var int_i); op = Plus; right = Integer {int = 1L}}})};
        ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})}
      ]}
    };

    WhileStm { 
      cond = BinOp{left = Lval (Var int_i); op = Lt; right = Integer {int = 10L}};
      body = 
        ExprStm{expr = Some (Call {fname = print_integer; args = [Assignment{lvl = Var int_i; rhs = BinOp{left = Lval (Var int_i); op = Plus; right = Integer {int = 1L}}}]})}
    };

    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test13
