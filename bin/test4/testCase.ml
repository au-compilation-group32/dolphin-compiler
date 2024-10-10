open Lib.TestUtils
open Lib.Ast

let test4: test_case = 
  "test4",
  "test arithmetic binop",
  let delc1 = Declaration {name = int_x; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block = [delc1] in
  let declar_block : declaration_block = DeclBlock delc_block in
  let delc2 = Declaration {name = int_y; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block2 = [delc2] in
  let declar_block2 : declaration_block = DeclBlock delc_block2 in
  let delc3 = Declaration {name = int_z; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block3 = [delc3] in
  let declar_block3 : declaration_block = DeclBlock delc_block3 in
  let delc4 = Declaration {name = int_t; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block4 = [delc4] in
  let declar_block4 : declaration_block = DeclBlock delc_block4 in
  [
    VarDeclStm declar_block;
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    VarDeclStm declar_block2;
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
    VarDeclStm declar_block3;
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
    VarDeclStm declar_block4;
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
