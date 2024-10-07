module Sym = Symbol
module TAst = TypedAst
module CfgBuilder = CfgBuilder

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnexpectedErrorType

let string_of_sym (name, i) = name ^ (string_of_int i)

let ll_type_of = function
  | TAst.Void -> Ll.Void
  | TAst.Int -> Ll.I64
  | TAst.Bool -> Ll.I1
  | TAst.ErrorType -> raise UnexpectedErrorType

let rec codegen_expr expr =
  match expr with
  | TAst.Integer {int} -> ([],Ll.I64, Ll.IConst64 int)
  | TAst.Boolean {bool} -> ([], Ll.I1, Ll.BConst bool)
  | TAst.BinOp {left; op; right; tp} -> codegen_binop left op right tp
  | TAst.UnOp {op; operand; tp} -> raise Unimplemented
  | TAst.Lval lvl ->  raise Unimplemented
  | TAst.Assignment {lvl; rhs; tp} -> codegen_assignment lvl rhs tp
  | TAst.Call {fname; args; tp} ->  raise Unimplemented
and codegen_binop left op right tp = raise Unimplemented
and codegen_assignment lvl rhs tp =
  let rhs_buildlets, rhs_tp, rhs_op = codegen_expr rhs in
  let _ = assert (rhs_tp = ll_type_of tp) in
  let tmp_sym = Sym.symbol "tmp" in
  let lvl_buildlets, lvl_tp, lvl_op = codegen_lval lvl in
  let _ = assert (lvl_tp = rhs_tp) in
  let insn = CfgBuilder.add_insn (Some tmp_sym, Ll.Store(rhs_tp, rhs_op, lvl_op)) in
  (rhs_buildlets @ lvl_buildlets @ [insn], lvl_tp, rhs_op)
and codegen_lval lvl =
  match lvl with
  | TAst.Var {ident; tp}->
    match ident with TAst.Ident {sym} ->
      ([], ll_type_of tp, Ll.Id sym)

let rec codegen_statement stm =
  match stm with
  | TAst.VarDeclStm {name; tp; body} ->
    let expr_buildlets, expr_tp, expr_op = codegen_expr body in
    let ll_type = ll_type_of tp in
    let _ = assert (expr_tp = ll_type) in
    begin match name with TAst.Ident {sym} ->
      let i1 = CfgBuilder.add_alloca (sym, ll_type) in
      expr_buildlets @ [i1]
    end
  | TAst.ExprStm {expr} -> raise Unimplemented
  | TAst.IfThenElseStm {cond; thbr; elbro} -> raise Unimplemented
  | TAst.CompoundStm {stms} -> raise Unimplemented
  | TAst.ReturnStm {ret} ->
    let buildlets, ret_tp, ret_operand = codegen_expr ret in
    let tr = CfgBuilder.term_block (Ll.Ret (ret_tp, Some ret_operand)) in
    buildlets @ [tr]
and codegen_statement_seq stms =
  let buildlet_lists = List.map codegen_statement stms in
  List.fold_left ( @ ) [] buildlet_lists

let codegen_prog prg =
  let open Sym in
  let open Ll in
  let open CfgBuilder in
  let builder = empty_cfg_builder in
  let buildets = seq_buildlets (codegen_statement_seq prg) in
  let cfg = get_cfg (buildets builder) in
  { tdecls    = []
  ; extgdecls = []
  ; gdecls    = []
  ; extfuns   = [ (symbol "print_integer",  ([I64], Void))
                ; (symbol "read_integer", ([], I64))]
  ; fdecls = [ (Sym.symbol "dolphin_main", 
                { fty = ([],  I64); param = []; cfg}  
               )] 
  }