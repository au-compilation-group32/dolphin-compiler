open Lib.TestUtils
open Lib.Ast

let test17: test_case =
  "test17",
  "loops with continue and break",
  let delc1 = Declaration {name = int_i; tp = None; body = Integer {int = 0L}} in
  let delc_block = [delc1] in
  let declar_block1 = DeclBlock delc_block in
  [
    ForStm {
      init = Some (FIDecl declar_block1);
      cond = None;
      update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
      body = CompoundStm {stms = [
        IfThenElseStm{
          cond = BinOp{left = Lval (Var int_i); op = Ge; right = Integer {int = 10L}};
          thbr = BreakStm;
          elbro = Some(
            IfThenElseStm {
              cond = BinOp{left = BinOp {left = Lval (Var int_i); op = Rem; right = Integer {int = 2L}}; op = Eq; right = Integer {int = 0L}};
              thbr = CompoundStm {stms = [
                ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
              ]};
              elbro = Some ( CompoundStm { stms = [
                ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_i); op = Mul; right = Integer{int = 10L}}]})};
                ContinueStm;
              ]});
            }
        )};
        ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
      ]}
    };

    ForStm {
      init = Some (FIDecl declar_block1);
      cond = Some (BinOp{left = Lval (Var int_i); op = Lt; right = Integer {int = 10L}});
      update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
      body = CompoundStm {stms = [
        IfThenElseStm {
          cond = BinOp{left = BinOp {left = Lval (Var int_i); op = Rem; right = Integer {int = 2L}}; op = Eq; right = Integer {int = 0L}};
          thbr = CompoundStm {stms = [
            ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
          ]};
          elbro = Some ( CompoundStm { stms = [
            ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_i); op = Mul; right = Integer{int = 10L}}]})};
            ContinueStm;
          ]});
        };
        ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
      ]}
    };

    VarDeclStm declar_block1;
    WhileStm {
      cond = Boolean {bool = true};
      body = CompoundStm {stms = [
        IfThenElseStm{
          cond = BinOp{left = Lval (Var int_i); op = Ge; right = Integer {int = 10L}};
          thbr = BreakStm;
          elbro = Some(
            IfThenElseStm {
              cond = BinOp{left = BinOp {left = Lval (Var int_i); op = Rem; right = Integer {int = 2L}}; op = Eq; right = Integer {int = 0L}};
              thbr = CompoundStm {stms = [
                ExprStm{expr = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}})};
                ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
              ]};
              elbro = Some ( CompoundStm { stms = [
                ExprStm{expr = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}})};
                ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_i); op = Mul; right = Integer{int = 10L}}]})};
                ContinueStm;
              ]});
            }
        )};
        ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
      ]}
    };
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test17
