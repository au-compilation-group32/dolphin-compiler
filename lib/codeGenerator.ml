module Sym = Symbol
module TAst = TypedAst
module CfgBuilder = CfgBuilder

exception Unimplemented (* your code should eventually compile without this exception *)

let string_of_sym (name, i) = name ^ (string_of_int i)

let rec codegen_expr builder expr =
  match expr with
  | TAst.Integer {int} -> raise Unimplemented
  | TAst.Boolean {bool} -> raise Unimplemented
  | TAst.BinOp {left; op; right; tp} -> raise Unimplemented
  | TAst.UnOp {op; operand; tp} -> raise Unimplemented
  | TAst.Lval lvl ->  raise Unimplemented
  | TAst.Assignment {lvl; rhs; tp} -> raise Unimplemented
  | TAst.Call {fname; args; tp} ->  raise Unimplemented
and codegen_lval builder lvl =
  match lvl with
  | TAst.Var {ident; tp}-> raise Unimplemented

let rec codegen_statement builder stm =
  match stm with
  | TAst.VarDeclStm {name; tp; body} -> raise Unimplemented
  | TAst.ExprStm {expr} -> raise Unimplemented
  | TAst.IfThenElseStm {cond; thbr; elbro} -> raise Unimplemented
  | TAst.CompoundStm {stms} -> raise Unimplemented
  | TAst.ReturnStm {ret} -> raise Unimplemented
and codegen_statement_seq builder stms = raise Unimplemented

let codegen_prog prg =
  let open Sym in
  let open Ll in
  let open CfgBuilder in
  let builder = empty_cfg_builder in
  let cfg = get_cfg builder in
  { tdecls    = []
  ; extgdecls = []
  ; gdecls    = []
  ; extfuns   = [ (symbol "print_integer",  ([I64], Void))
                ; (symbol "read_integer", ([], I64))]
  ; fdecls = [ (Sym.symbol "dolphin_main", 
                { fty = ([],  I64); param = []; cfg}  
               )] 
  }