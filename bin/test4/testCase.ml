open Lib.TestUtils
open Lib.Ast

let test4: test_case = 
  "test4",
  "test arithmetic binop",
  [
    VarDeclStm {name = int_x; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    VarDeclStm {name = int_y; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
    VarDeclStm {name = int_z; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
    VarDeclStm {name = int_t; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_t)]})};

    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Plus; right = Lval(Var int_y)}]})};

    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Minus; right = Lval(Var int_y)}]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Minus; right = Lval(Var int_z)}]})};

    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Mul; right = Lval(Var int_y)}]})};

    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Div; right = Lval(Var int_y)}]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Rem; right= Lval(Var int_y)}]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Div; right = Lval(Var int_z)}]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Rem; right = Lval(Var int_z)}]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Div; right = Lval(Var int_t)}]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp{left = Lval(Var int_x); op = Rem; right = Lval(Var int_t)}]})};
    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test4
