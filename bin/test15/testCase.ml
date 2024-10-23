open Lib.TestUtils
open Lib.Ast

let test15: test_case = 
  "test15",
  "for loop without some clauses",
  let delc1 = Declaration {name = int_i; tp = None; body = Integer {int = 0L}} in
  let delc_block = [delc1] in
  let declar_block1 = DeclBlock delc_block in
  [
    VarDeclStm declar_block1;
    ForStm {init = None;
            cond = Some (BinOp{left = Lval (Var int_i); op = Lt; right = Integer {int = 5L}});
            update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
            body = ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})}};

    ForStm {init = Some (FIDecl declar_block1);
            cond = None;
            update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
            body = CompoundStm {stms = [
              IfThenElseStm{cond = BinOp{left = Lval (Var int_i); op = Ge; right = Integer {int = 5L}}; thbr = BreakStm; elbro = None};
              ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
            ]}
    };

    ForStm {init = Some (FIDecl declar_block1);
            cond = Some (BinOp{left = Lval (Var int_i); op = Lt; right = Integer {int = 5L}});
            update = None;
            body = CompoundStm {stms = [
              ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
              ExprStm{expr = Some (Assignment{lvl = Var int_i; rhs = BinOp{left = Lval (Var int_i); op = Plus; right = Integer {int = 1L}}})};
            ]}
    };

    ForStm {init = Some (FIDecl declar_block1);
            cond = None;
            update = None;
            body = CompoundStm {stms = [
              IfThenElseStm{cond = BinOp{left = Lval (Var int_i); op = Ge; right = Integer {int = 5L}}; thbr = BreakStm; elbro = None};
              ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
              ExprStm{expr = Some (Assignment{lvl = Var int_i; rhs = BinOp{left = Lval (Var int_i); op = Plus; right = Integer {int = 1L}}})};
            ]}
    };

    ExprStm {expr = Some (Assignment {lvl = Var int_i; rhs = Integer {int = 0L}})};
    ForStm {init = None;
            cond = Some (BinOp{left = Lval (Var int_i); op = Lt; right = Integer {int = 5L}});
            update = None;
            body = CompoundStm {stms = [
              ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
              ExprStm{expr = Some (Assignment{lvl = Var int_i; rhs = BinOp{left = Lval (Var int_i); op = Plus; right = Integer {int = 1L}}})};
            ]}
    };

    ExprStm {expr = Some (Assignment {lvl = Var int_i; rhs = Integer {int = 0L}})};
    ForStm {init = None;
            cond = None;
            update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
            body = CompoundStm {stms = [
              IfThenElseStm{cond = BinOp{left = Lval (Var int_i); op = Ge; right = Integer {int = 5L}}; thbr = BreakStm; elbro = None};
              ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
            ]}
    };

    ExprStm {expr = Some (Assignment {lvl = Var int_i; rhs = Integer {int = 0L}})};
    ForStm {init = None;
            cond = None;
            update = None;
            body = CompoundStm {stms = [
              IfThenElseStm{cond = BinOp{left = Lval (Var int_i); op = Ge; right = Integer {int = 5L}}; thbr = BreakStm; elbro = None};
              ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
              ExprStm{expr = Some (Assignment{lvl = Var int_i; rhs = BinOp{left = Lval (Var int_i); op = Plus; right = Integer {int = 1L}}})};
            ]}
    };

    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test15
