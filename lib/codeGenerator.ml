module Sym = Symbol
module TAst = TypedAst
module CfgBuilder = CfgBuilder
module Env = LlvmEnv

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnexpectedErrorType
exception UnexpectedVoidType

let string_of_sym (name, i) = name ^ (string_of_int i)

let ll_type_of = function
  | TAst.Void -> Ll.Void
  | TAst.Int -> Ll.I64
  | TAst.Bool -> Ll.I1
  | TAst.ErrorType -> raise UnexpectedErrorType

let ptr_operand_of_lval env = function
  | TAst.Var {ident; _} ->
    let TAst.Ident {sym} = ident in
    let lval_sym = Env.lookup env sym in
    Ll.Id lval_sym

(*Return add_insn of res_op = left_op op right_op *)
let get_binop_insn res_op left_op op right_op op_tp = 
  match op with
  | TAst.Plus -> CfgBuilder.add_insn(Some res_op, Ll.Binop(Ll.Add, Ll.I64, left_op, right_op))
  | TAst.Minus -> CfgBuilder.add_insn(Some res_op, Ll.Binop(Ll.Sub, Ll.I64, left_op, right_op))
  | TAst.Mul -> CfgBuilder.add_insn(Some res_op, Ll.Binop(Ll.Mul, Ll.I64, left_op, right_op))
  | TAst.Div -> CfgBuilder.add_insn(Some res_op, Ll.Binop(Ll.SDiv, Ll.I64, left_op, right_op))
  | TAst.Rem -> CfgBuilder.add_insn(Some res_op, Ll.Binop(Ll.SRem, Ll.I64, left_op, right_op))
  | TAst.Lt -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Slt, Ll.I64, left_op, right_op))
  | TAst.Le -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Sle, Ll.I64, left_op, right_op))
  | TAst.Gt -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Sgt, Ll.I64, left_op, right_op))
  | TAst.Ge -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Sge, Ll.I64, left_op, right_op))
  | TAst.Lor -> CfgBuilder.add_insn(Some res_op, Ll.Binop(Ll.Or, Ll.I1, left_op, right_op))
  | TAst.Land -> CfgBuilder.add_insn(Some res_op, Ll.Binop(Ll.And, Ll.I1, left_op, right_op))
  | TAst.Eq -> 
    begin match op_tp with
    | TAst.Int -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Eq, Ll.I64, left_op, right_op))
    | TAst.Bool -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Eq, Ll.I1, left_op, right_op))
    | TAst.Void -> raise UnexpectedVoidType
    | TAst.ErrorType -> raise UnexpectedErrorType
    end
  | TAst.NEq ->
    begin match op_tp with
    | TAst.Int -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Ne, Ll.I64, left_op, right_op))
    | TAst.Bool -> CfgBuilder.add_insn(Some res_op, Ll.Icmp(Ll.Ne, Ll.I1, left_op, right_op))
    | TAst.Void -> raise UnexpectedVoidType
    | TAst.ErrorType -> raise UnexpectedErrorType
    end

let rec codegen_expr env expr =
  match expr with
  | TAst.Integer {int} -> ([],Ll.I64, Ll.IConst64 int)
  | TAst.Boolean {bool} -> ([], Ll.I1, Ll.BConst bool)
  | TAst.BinOp {left; op; right; tp} -> codegen_binop env left op right tp
  | TAst.UnOp {op; operand; tp} -> raise Unimplemented
  | TAst.Lval lvl ->  codegen_lval env lvl
  | TAst.Assignment {lvl; rhs; tp} -> codegen_assignment env lvl rhs tp
  | TAst.Call {fname; args; tp} ->  raise Unimplemented
and codegen_binop env left op right tp =
  let ll_tp = ll_type_of tp in
  let left_buildlets, left_tp, left_op = codegen_expr env left in
  let _ = assert(left_tp = ll_tp) in
  let right_buildlets, right_tp, right_op = codegen_expr env right in
  let _ = assert(right_tp = ll_tp) in
  let tmp_sym = Env.insert_tmp_reg env in
  let tmp_op = Ll.Id tmp_sym in
  let binop_insn = get_binop_insn tmp_sym left_op op right_op tp in
  (left_buildlets @ right_buildlets @ [binop_insn], ll_tp, tmp_op)
and codegen_assignment env lvl rhs tp =
  let rhs_buildlets, rhs_tp, rhs_op = codegen_expr env rhs in
  let _ = assert (rhs_tp = ll_type_of tp) in
  let lvl_op = ptr_operand_of_lval env lvl in
  let insn = CfgBuilder.add_insn (None, Ll.Store(rhs_tp, rhs_op, lvl_op)) in
  (rhs_buildlets @ [insn], rhs_tp, rhs_op)
and codegen_lval env lvl =
  match lvl with
  | TAst.Var {ident = _; tp}->
    let lvl_op = ptr_operand_of_lval env lvl in
    let ll_typ = ll_type_of tp in
    let tmp_sym = Env.insert_tmp_reg env in
    let insn = CfgBuilder.add_insn (Some tmp_sym, Ll.Load(ll_typ, lvl_op)) in
    ([insn], ll_typ, Ll.Id tmp_sym)

let rec codegen_statement env stm =
  match stm with
  | TAst.VarDeclStm {name; tp; body} ->
    let ll_type = ll_type_of tp in
    let TAst.Ident {sym} = name in
    let var_sym = Env.insert_reg_by_sym env sym in
    let i1 = CfgBuilder.add_alloca (var_sym, ll_type) in
    let asgn_buildlets, asgn_tp, _ = codegen_assignment env (TAst.Var {ident = name; tp = tp}) body tp in
    let _ = assert (asgn_tp = ll_type) in
    [i1] @ asgn_buildlets
  | TAst.ExprStm {expr} -> raise Unimplemented
  | TAst.IfThenElseStm {cond; thbr; elbro} -> raise Unimplemented
  | TAst.CompoundStm {stms} -> raise Unimplemented
  | TAst.ReturnStm {ret} ->
    let buildlets, ret_tp, ret_operand = codegen_expr env ret in
    let tr = CfgBuilder.term_block (Ll.Ret (ret_tp, Some ret_operand)) in
    buildlets @ [tr]
and codegen_statement_seq env stms =
  let buildlet_lists = List.map (codegen_statement env) stms in
  List.fold_left ( @ ) [] buildlet_lists

let codegen_prog prg =
  let open Sym in
  let open Ll in
  let open CfgBuilder in
  let env = Env.make_empty_env in
  let builder = empty_cfg_builder in
  let buildets = seq_buildlets (codegen_statement_seq env prg) in
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
