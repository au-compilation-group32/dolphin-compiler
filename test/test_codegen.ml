open Lib.Ast
open Lib.Semant
open Lib.Errors
module Pretty = Lib.Pretty
(* open Dolphin_compiler.TypedPretty *)

type test_case = string * Lib.Ast.program

let prog_1: test_case = "prog_1: Return 0", [ReturnStm {ret = Integer {int = 0L}}]
let prog_2: test_case = "prog_2: Return 1", [ReturnStm {ret = Integer {int = 1L}}]
let prog_3: test_case = "prog_3: ",
  let print_integer = Ident {name = "print_integer"} in
  let read_integer = Ident {name = "read_integer"} in
  let x = Ident {name = "x"} in
  let y = Ident {name = "y"} in
  let z = Ident {name = "z"} in
  let t = Ident {name = "t"} in
  [
    VarDeclStm {name = x; tp = None; body = Integer {int = 1L}};
    VarDeclStm {name = y; tp = None; body = Assignment {lvl = Var x; rhs = BinOp {left = Lval(Var x); op = Plus; right = Integer {int = 2L}}}};
    VarDeclStm {name = z; tp = None; body = BinOp {left = Lval (Var x); op = Plus; right = Lval (Var y)}};
    VarDeclStm {name = t; tp = None; body = Call {fname = read_integer; args = []}};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var z)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var t)]})};
    IfThenElseStm {
      cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var y)})};
      elbro = None;
    };
    IfThenElseStm {
      cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var y)})};
      elbro = Some (ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var z)})});
    };
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var z)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var t)]})};
    ReturnStm {ret = Lval (Var t)}
  ]
let prog_4: test_case = "prog_4: Return x", 
  [
    VarDeclStm {name = Ident {name = "x"}; tp = None; body =Integer {int = 1L}};
    ReturnStm {ret = Lval (Var (Ident {name = "x"}))}
  ]
let prog_5: test_case = "prog_5: Return x", 
  let read_integer = Ident {name = "read_integer"} in
  let x = Ident {name = "x"} in
  let t = Ident {name = "t"} in
  [
    VarDeclStm {name = x; tp = None; body =Integer {int = 1L}};
    VarDeclStm {name = t; tp = None; body = Call {fname = read_integer; args = []}};
    IfThenElseStm {
      cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var x)})};
      elbro = None;
    };
    ReturnStm {ret = Lval (Var t)}
  ]
let prog_6: test_case = "prog_6: CompoundStm ", 
  let read_integer = Ident {name = "read_integer"} in
  let x = Ident {name = "x"} in
  let t = Ident {name = "t"} in
  let lst: statement list = [] in
  let arg1 = VarDeclStm {name = x; tp = None; body =Integer {int = 1L}} in
  let arg2 = VarDeclStm {name = t; tp = None; body = Call {fname = read_integer; args = []}} in
  let arg3 = 
    IfThenElseStm {
      cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var x)})};
      elbro = None;
    } 
  in
  let lst = arg3 :: lst in
  let lst = arg2 :: lst in
  let lst = arg1 :: lst in
  [
    VarDeclStm {name = x; tp = None; body =Integer {int = 3L}};
    CompoundStm{stms = lst};
    ReturnStm {ret = Lval (Var x)}
  ]
let prog_7: test_case = "prog_7: ExprStm None", 
  [
    VarDeclStm {name = Ident {name = "x"}; tp = None; body =Integer {int = 1L}};
    ExprStm {expr = None};
    ReturnStm {ret = Lval (Var (Ident {name = "x"}))}
  ]
let prog_8: test_case = "prog_8: CompoundStm change x", 
let read_integer = Ident {name = "read_integer"} in
let x = Ident {name = "x"} in
let t = Ident {name = "t"} in
let lst: statement list = [] in
let arg2 = VarDeclStm {name = t; tp = None; body = Call {fname = read_integer; args = []}} in
let arg3 = 
  IfThenElseStm {
    cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
    thbr = ExprStm {expr = Some (Assignment {lvl = Var t; rhs = Lval (Var x)})};
    elbro = None;
  } 
in
let lst = arg3 :: lst in
let lst = arg2 :: lst in
[
  VarDeclStm {name = x; tp = None; body =Integer {int = 3L}};
  CompoundStm{stms = lst};
  ReturnStm {ret = Lval (Var x)}
]
let prog_9: test_case = "prog_9: Change x", 
  [
    VarDeclStm {name = Ident {name = "x"}; tp = None; body =Integer {int = 1L}};
    VarDeclStm {name = Ident {name = "x"}; tp = None; body =Integer {int = 5L}};
    ReturnStm {ret = Lval (Var (Ident {name = "x"}))}
  ]
let prog_10: test_case = "prog_10: Return x", 
  let x = Ident {name = "x"} in
  [
    VarDeclStm {name = x; tp = None; body =Integer {int = 1L}};
    IfThenElseStm {
      cond = BinOp {left = Lval (Var x); op = Eq; right = Integer {int = 1L}};
      thbr = ExprStm {expr = Some (Assignment {lvl = Var x; rhs = UnOp {op = Lnot; operand =Lval (Var x)}})};
      elbro = None;
    };
    ReturnStm {ret = Lval (Var x)}
  ]

let print_err e = let _ = Printf.printf "%s\n" (error_to_string e) in ()
let test_codegen (name, p) =
  let tprog, errors = typecheck_prog p in
  let _ = Printf.printf "======================================\n" in
  let _ = Printf.printf "RUNNING TEST ON %s\n" name in
  let _ = PrintBox_text.output stdout (TPretty.program_to_tree tprog) in
  if List.length !errors <> 0
  then 
    let _ = Printf.printf "\nERROR LIST: \n" in
    let _ = List.map print_err !errors in
    let _ = Printf.printf "\n" in
    ()
  else 
    let llprog = Lib.CodeGenerator.codegen_prog tprog in
    let _ = Printf.printf "\nLLVM code: \n" in
    let _ = Printf.printf "\n%s\n" (Lib.Ll.string_of_prog llprog) in
    ()

let progs = [prog_1; prog_2; prog_3; prog_4; prog_5; prog_6; prog_7; prog_8; prog_9; prog_10]
let _ = List.map test_codegen progs
