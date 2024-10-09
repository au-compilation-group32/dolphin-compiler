open Lib.TestUtils
open Lib.Ast

let test6: test_case = 
  "test6",
  "test unop",
  [
    VarDeclStm {name = int_x; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    VarDeclStm {name = int_y; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

    ExprStm {expr = Some (Call {fname = print_integer; args = [UnOp{op = Neg; operand = Lval(Var int_x)}]})};

    IfThenElseStm {
      cond = UnOp {op = Lnot; operand = BinOp {left = Lval (Var int_x); op = Ge; right = Lval (Var int_y)}};
      thbr = ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
      elbro = Some (ExprStm {expr = Some (Call {fname = print_integer; args = [UnOp{op = Neg; operand = Lval(Var int_x)}]})});
    };
    IfThenElseStm {
      cond = UnOp {op = Lnot; operand = BinOp {left = Lval (Var int_x); op = Le; right = Lval (Var int_y)}};
      thbr = ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
      elbro = Some (ExprStm {expr = Some (Call {fname = print_integer; args = [UnOp{op = Neg; operand = Lval(Var int_x)}]})});
    };
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test6
