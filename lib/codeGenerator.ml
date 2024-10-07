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

let ptr_operand_of_lval = function
  | TAst.Var {ident; tp} ->
      match ident with TAst.Ident {sym} -> Ll.Id sym

let rec codegen_expr expr =
  match expr with
  | TAst.Integer {int} -> ([],Ll.I64, Ll.IConst64 int)
  | TAst.Boolean {bool} -> ([], Ll.I1, Ll.BConst bool)
  | TAst.BinOp {left; op; right; tp} -> codegen_binop left op right tp
  | TAst.UnOp {op; operand; tp} -> raise Unimplemented
  | TAst.Lval lvl ->  codegen_lval lvl
  | TAst.Assignment {lvl; rhs; tp} -> codegen_assignment lvl rhs tp
  | TAst.Call {fname; args; tp} ->  raise Unimplemented
and codegen_binop left op right tp = raise Unimplemented
and codegen_assignment lvl rhs tp =
  let rhs_buildlets, rhs_tp, rhs_op = codegen_expr rhs in
  let _ = assert (rhs_tp = ll_type_of tp) in
  let lvl_op = ptr_operand_of_lval lvl in
  let insn = CfgBuilder.add_insn (None, Ll.Store(rhs_tp, rhs_op, lvl_op)) in
  (rhs_buildlets @ [insn], rhs_tp, rhs_op)
and codegen_lval lvl =
  match lvl with
  | TAst.Var {ident; tp}->
    let lvl_op = ptr_operand_of_lval lvl in
    let ll_typ = ll_type_of tp in
    let tmp_sym = Sym.symbol "tmp" in
    let insn = CfgBuilder.add_insn (Some tmp_sym, Ll.Load(ll_typ, lvl_op)) in
    ([insn], ll_typ, Ll.Id tmp_sym)

let rec codegen_statement stm =
  match stm with
  | TAst.VarDeclStm {name; tp; body} ->
    let ll_type = ll_type_of tp in
    let TAst.Ident {sym} = name in
    let i1 = CfgBuilder.add_alloca (sym, ll_type) in
    let asgn_buildlets, asgn_tp, asgn_op = codegen_assignment (TAst.Var {ident = name; tp = tp}) body tp in
    let _ = assert (asgn_tp = ll_type) in
    [i1] @ asgn_buildlets
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