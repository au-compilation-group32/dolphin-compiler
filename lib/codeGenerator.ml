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

let tast_type_of = function
  | Ll.Void -> TAst.Void
  | Ll.I64 -> TAst.Int
  | Ll.I1 -> TAst.Bool
  | _ -> TAst.ErrorType

let ptr_operand_of_lval env = function
  | TAst.Var {ident; _} ->
    let TAst.Ident {sym} = ident in
    let lval_sym = Env.get_alias_sym env sym in
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
  let right_buildlets, right_tp, right_op = codegen_expr env right in
  let _ = assert(left_tp = right_tp) in
  let _, tmp_alias_sym = Env.insert_tmp_reg env in
  let tmp_op = Ll.Id tmp_alias_sym in
  let binop_insn = get_binop_insn tmp_alias_sym left_op op right_op (tast_type_of right_tp) in
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
    let _, tmp_alias_sym = Env.insert_tmp_reg env in
    let insn = CfgBuilder.add_insn (Some tmp_alias_sym, Ll.Load(ll_typ, lvl_op)) in
    ([insn], ll_typ, Ll.Id tmp_alias_sym)

let rec codegen_statement env stm =
  match stm with
  | TAst.VarDeclStm {name; tp; body} ->
    let ll_type = ll_type_of tp in
    let TAst.Ident {sym} = name in
    let new_env, var_alias_sym = Env.insert_reg env sym in
    let i1 = CfgBuilder.add_alloca (var_alias_sym, ll_type) in
    let asgn_buildlets, asgn_tp, _ = codegen_assignment new_env (TAst.Var {ident = name; tp = tp}) body tp in
    let _ = assert (asgn_tp = ll_type) in
    ([i1] @ asgn_buildlets, new_env)
  | TAst.ExprStm {expr} ->
    begin match expr with
    | None -> ([], env)
    | Some e ->
      let buildlets, _, _ = codegen_expr env e in
      (buildlets, env)
    end
  | TAst.IfThenElseStm {cond; thbr; elbro} ->
    let cond_buildlets, cond_tp, cond_op = codegen_expr env cond in
    let _ = assert(cond_tp = Ll.I1) in
    let _, tmp_thbr_sym = Env.insert_label env in
    let _, tmp_elbro_sym = Env.insert_label env in
    let _, tmp_merge_sym = Env.insert_label env in
    let term_blk_cond = CfgBuilder.term_block(Ll.Cbr (cond_op, tmp_thbr_sym, tmp_elbro_sym)) in
    let start_blk_then = CfgBuilder.start_block(tmp_thbr_sym) in
    let buildlets_blk_then, _ = codegen_statement env thbr in
    let term_blk_then = CfgBuilder.term_block(Ll.Br (tmp_merge_sym)) in
    let start_blk_else = CfgBuilder.start_block(tmp_elbro_sym) in
    let buildlets_blk_elbro = 
      begin match elbro with
      | None -> []
      | Some e -> 
        let buildlets, _ = codegen_statement env e in buildlets
      end in
    let term_blk_elbro = CfgBuilder.term_block(Ll.Br (tmp_merge_sym)) in
    let start_blk_merge = CfgBuilder.start_block(tmp_merge_sym) in
    (cond_buildlets @ [term_blk_cond] @ [start_blk_then] @ buildlets_blk_then @ [term_blk_then] @ [start_blk_else] @ buildlets_blk_elbro @ [term_blk_elbro] @ [start_blk_merge], env)
  | TAst.CompoundStm {stms} ->
    let buildlets, _ = codegen_statement_seq env stms in
    (buildlets, env)
  | TAst.ReturnStm {ret} ->
    let buildlets, ret_tp, ret_operand = codegen_expr env ret in
    let tr = CfgBuilder.term_block (Ll.Ret (ret_tp, Some ret_operand)) in
    (buildlets @ [tr], env)
and codegen_statement_seq env stms =
  let merge ret stms = 
    let (current_buildlets, current_env) = ret in
    let stms_buildlets, new_env = codegen_statement current_env stms in
    (current_buildlets @ stms_buildlets, new_env)
  in
  List.fold_left merge ([], env) stms

let codegen_prog prg =
  let open Sym in
  let open Ll in
  let open CfgBuilder in
  let env = Env.make_empty_env in
  let builder = empty_cfg_builder in
  let buildlets, _ = codegen_statement_seq env prg in
  let seq_buildlets = seq_buildlets buildlets in
  let cfg = get_cfg (seq_buildlets builder) in
  { tdecls    = []
  ; extgdecls = []
  ; gdecls    = []
  ; extfuns   = [ (symbol "print_integer",  ([I64], Void))
                ; (symbol "read_integer", ([], I64))]
  ; fdecls = [ (Sym.symbol "dolphin_main", 
                { fty = ([],  I64); param = []; cfg}  
               )] 
  }
