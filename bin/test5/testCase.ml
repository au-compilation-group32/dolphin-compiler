open Lib.TestUtils
open Lib.Ast

let test5: test_case = 
  "test5",
  "test short-circuiting",
  [
    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};

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

    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};
   
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

    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};
 
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

    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};
 
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

    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};
 
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

    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};
 
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

    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};
 
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

    VarDeclStm {name = int_x; tp = None; body = Integer {int = 100L}};
    VarDeclStm {name = int_y; tp = None; body = Integer {int = 69L}};
    VarDeclStm {name = int_z; tp = None; body = Integer {int = 419L}};
    VarDeclStm {name = int_t; tp = None; body = Integer {int = 10L}};
 
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
