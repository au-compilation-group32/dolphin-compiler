open Lib.TestUtils
open Lib.Ast

let test5: test_case = 
  "test5",
  "test short-circuiting",
  let delc11 = Declaration {name = int_x; tp = None; body = Integer {int = 100L}} in
  let delc12 = Declaration {name = int_y; tp = None; body = Integer {int = 69L}} in
  let delc13 = Declaration {name = int_z; tp = None; body = Integer {int = 419L}} in
  let delc14 = Declaration {name = int_t; tp = None; body = Integer {int = 10L}} in
  let delc_block1 = [delc11; delc12; delc13; delc14] in
  let declar_block1 : declaration_block = DeclBlock delc_block1 in
  [
    VarDeclStm declar_block1;
    (*false || false*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Le; right = Lval(Var int_y)}; op = Lor; right = BinOp {left = Lval (Var int_x); op = Le; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    VarDeclStm declar_block1;
   
    (*false || true*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Le; right = Lval(Var int_y)}; op = Lor; right = BinOp {left = Lval (Var int_x); op = Ge; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    VarDeclStm declar_block1;
 
    (*true || false*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Ge; right = Lval(Var int_y)}; op = Lor; right = BinOp {left = Lval (Var int_x); op = Le; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    VarDeclStm declar_block1;
 
    (*true || true*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Ge; right = Lval(Var int_y)}; op = Lor; right = BinOp {left = Lval (Var int_x); op = Ge; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    VarDeclStm declar_block1;
 
    (*false && false*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Le; right = Lval(Var int_y)}; op = Land; right = BinOp {left = Lval (Var int_x); op = Le; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    VarDeclStm declar_block1;
 
    (*false && true*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Le; right = Lval(Var int_y)}; op = Land; right = BinOp {left = Lval (Var int_x); op = Ge; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    VarDeclStm declar_block1;
 
    (*true && false*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Ge; right = Lval(Var int_y)}; op = Land; right = BinOp {left = Lval (Var int_x); op = Le; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    VarDeclStm declar_block1;
 
    (*true && true*)
    IfThenElseStm {
      cond = BinOp {left = BinOp {left = Lval(Var int_x); op = Ge; right = Lval(Var int_y)}; op = Land; right = BinOp {left = Lval (Var int_x); op = Ge; right= Assignment {lvl = Var int_z; rhs = Lval (Var int_t)}}};
      thbr = CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]};
      elbro = Some ( CompoundStm {stms = [
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
        ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
      ]});
    };

    ReturnStm {ret = Integer {int = 0L}}
  ]

let _ = run_testcase test5
