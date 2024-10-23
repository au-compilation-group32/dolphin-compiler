open Lib.TestUtils
open Lib.Ast

let test20: test_case =
  "test20",
  "Negative: Loops with wrond cond type",
  let declar_block_i = DeclBlock [Declaration {name = int_i; tp = None; body = Integer {int = 0L}}] in
  let declar_block_j = DeclBlock [Declaration {name = int_j; tp = None; body = Integer {int = 0L}}] in
  let declar_block_k = DeclBlock [Declaration {name = int_k; tp = None; body = Integer {int = 0L}}] in
  [
    ForStm {
      init = Some (FIDecl declar_block_i);
      cond = Some (BinOp{left = Lval (Var int_i); op = Minus; right = Integer {int = 2L}});
      update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
      body = 
        ForStm {
          init = Some (FIDecl declar_block_j);
          cond = Some (BinOp{left = Lval (Var int_j); op = Minus; right = Integer {int = 2L}});
          update = Some (Assignment {lvl = Var int_j; rhs = BinOp {left = Lval(Var int_j); op = Plus; right = Integer {int = 1L}}});
          body = 
            ForStm {
              init = Some (FIDecl declar_block_k);
              cond = Some (BinOp{left = Lval (Var int_k); op = Minus; right = Integer {int = 2L}});
              update = Some (Assignment {lvl = Var int_k; rhs = BinOp {left = Lval(Var int_k); op = Plus; right = Integer {int = 1L}}});
              body = CompoundStm {stms = [
                IfThenElseStm {
                  cond = BinOp{left = BinOp {left = Lval (Var int_k); op = Rem; right = Integer {int = 2L}}; op = Eq; right = Integer {int = 0L}};
                  thbr = CompoundStm {stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_j)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_k)]})};
                  ]};
                  elbro = Some ( CompoundStm { stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_i); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_j); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_k); op = Mul; right = Integer{int = 10L}}]})};
                    ContinueStm;
                  ]});
                };
              ]}
            }}
    };

    VarDeclStm declar_block_i;
    WhileStm {
      cond = BinOp{left = Lval (Var int_i); op = Minus; right = Integer {int = 2L}};
      body = CompoundStm {stms =[
        ForStm {
          init = Some (FIDecl declar_block_j);
          cond = Some (BinOp{left = Lval (Var int_j); op = Minus; right = Integer {int = 2L}});
          update = Some (Assignment {lvl = Var int_j; rhs = BinOp {left = Lval(Var int_j); op = Plus; right = Integer {int = 1L}}});
          body = 
            ForStm {
              init = Some (FIDecl declar_block_k);
              cond = Some (BinOp{left = Lval (Var int_k); op = Minus; right = Integer {int = 2L}});
              update = Some (Assignment {lvl = Var int_k; rhs = BinOp {left = Lval(Var int_k); op = Plus; right = Integer {int = 1L}}});
              body = CompoundStm {stms = [
                IfThenElseStm {
                  cond = BinOp{left = BinOp {left = Lval (Var int_k); op = Rem; right = Integer {int = 2L}}; op = Eq; right = Integer {int = 0L}};
                  thbr = CompoundStm {stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_j)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_k)]})};
                  ]};
                  elbro = Some ( CompoundStm { stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_i); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_j); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_k); op = Mul; right = Integer{int = 10L}}]})};
                    ContinueStm;
                  ]});
                };
              ]}
            }
        };
        ExprStm {expr = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}})};
      ]}
    };

    ForStm {
      init = Some (FIDecl declar_block_i);
      cond = Some (BinOp{left = Lval (Var int_i); op = Minus; right = Integer {int = 2L}});
      update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
      body = CompoundStm {stms =[
        VarDeclStm declar_block_j;
        WhileStm {
          cond = BinOp{left = Lval (Var int_j); op = Minus; right = Integer {int = 2L}};
          body = CompoundStm {stms = [
            ForStm {
              init = Some (FIDecl declar_block_k);
              cond = Some (BinOp{left = Lval (Var int_k); op = Minus; right = Integer {int = 2L}});
              update = Some (Assignment {lvl = Var int_k; rhs = BinOp {left = Lval(Var int_k); op = Plus; right = Integer {int = 1L}}});
              body = CompoundStm {stms = [
                IfThenElseStm {
                  cond = BinOp{left = BinOp {left = Lval (Var int_k); op = Rem; right = Integer {int = 2L}}; op = Eq; right = Integer {int = 0L}};
                  thbr = CompoundStm {stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_j)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_k)]})};
                  ]};
                  elbro = Some ( CompoundStm { stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_i); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_j); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_k); op = Mul; right = Integer{int = 10L}}]})};
                    ContinueStm;
                  ]});
                };
              ]}
            };
            ExprStm{expr = Some(Assignment {lvl = Var int_j; rhs = BinOp {left = Lval(Var int_j); op = Plus; right = Integer {int = 1L}}})};
          ]}
        }]}
    };

    ForStm {
      init = Some (FIDecl declar_block_i);
      cond = Some (BinOp{left = Lval (Var int_i); op = Minus; right = Integer {int = 2L}});
      update = Some (Assignment {lvl = Var int_i; rhs = BinOp {left = Lval(Var int_i); op = Plus; right = Integer {int = 1L}}});
      body = 
        ForStm {
          init = Some (FIDecl declar_block_j);
          cond = Some (BinOp{left = Lval (Var int_j); op = Minus; right = Integer {int = 2L}});
          update = Some (Assignment {lvl = Var int_j; rhs = BinOp {left = Lval(Var int_j); op = Plus; right = Integer {int = 1L}}});
          body = CompoundStm {stms = [
            VarDeclStm declar_block_k;
            WhileStm {
              cond = BinOp{left = Lval (Var int_k); op = Minus; right = Integer {int = 2L}};
              body = CompoundStm {stms = [
                IfThenElseStm {
                  cond = BinOp{left = BinOp {left = Lval (Var int_k); op = Rem; right = Integer {int = 2L}}; op = Eq; right = Integer {int = 0L}};
                  thbr = CompoundStm {stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_i)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_j)]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [Lval(Var int_k)]})};
                    ExprStm {expr = Some (Assignment {lvl = Var int_k; rhs = BinOp {left = Lval(Var int_k); op = Plus; right = Integer {int = 1L}}})};
                  ]};
                  elbro = Some ( CompoundStm { stms = [
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_i); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_j); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm{expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_k); op = Mul; right = Integer{int = 10L}}]})};
                    ExprStm {expr = Some (Assignment {lvl = Var int_k; rhs = BinOp {left = Lval(Var int_k); op = Plus; right = Integer {int = 1L}}})};
                    ContinueStm;
                  ]});
                };
              ]}
            }
          ]}
        }
    };

    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test20
