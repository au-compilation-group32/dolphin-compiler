module Sym = Symbol
module TAst = TypedAst
module CfgBuilder = CfgBuilder
module Env = LlvmEnv

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnexpectedErrorType
exception UnexpectedVoidType
exception UnexpectedOperator

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
  | Ll.I8 | Ll.I32 | Ll.Ptr _| Ll.Struct _ | Ll.Array _ | Ll.Fun _ | Ll.Namedt _ -> TAst.ErrorType

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
  | TAst.Lor -> raise UnexpectedOperator
  | TAst.Land -> raise UnexpectedOperator
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

let get_short_circuit_insns env res_op left_buildlets left_op op right_buildlets right_op op_tp = 
  match op with
  | TAst.Lor | TAst.Land ->
    let _, left_label_sym = Env.insert_label env in
    let _, right_label_sym = Env.insert_label env in
    let _, merge_label_sym = Env.insert_label env in
    let term_curr_blk = CfgBuilder.term_block(Ll.Br(left_label_sym)) in
    let start_blk_left = CfgBuilder.start_block(left_label_sym) in
    let _, tmp_icmp_sym = Env.insert_tmp_reg env in
    let tmp_icmp_op = Ll.Id tmp_icmp_sym in
    let icmp_insn = 
      if op = TAst.Lor then CfgBuilder.add_insn(Some tmp_icmp_sym, Ll.Icmp(Ll.Eq, Ll.I1, left_op, Ll.BConst true)) 
      else if op = TAst.Land then CfgBuilder.add_insn(Some tmp_icmp_sym, Ll.Icmp(Ll.Eq, Ll.I1, left_op, Ll.BConst false)) 
      else raise UnexpectedOperator
    in
    let term_blk_left = CfgBuilder.term_block(Ll.Cbr (tmp_icmp_op, merge_label_sym, right_label_sym)) in
    let start_blk_right = CfgBuilder.start_block(right_label_sym) in
    let term_blk_right = CfgBuilder.term_block(Ll.Br(merge_label_sym)) in
    let start_merge_blk = CfgBuilder.start_block(merge_label_sym) in
    let phi_insn = 
      if op = TAst.Lor then CfgBuilder.add_insn(Some res_op, Ll.PhiNode(ll_type_of op_tp, [(Ll.BConst true, left_label_sym); (right_op, right_label_sym)]))
      else if op = TAst.Land then CfgBuilder.add_insn(Some res_op, Ll.PhiNode(ll_type_of op_tp, [(Ll.BConst false, left_label_sym); (right_op, right_label_sym)]))
      else raise UnexpectedOperator
    in
    [term_curr_blk; start_blk_left] @ left_buildlets @ [icmp_insn; term_blk_left; start_blk_right] @ right_buildlets @ [term_blk_right; start_merge_blk; phi_insn] 
  | TAst.Plus | TAst.Minus | TAst.Mul | TAst.Div | TAst.Rem | TAst.Gt | TAst.Ge | TAst.Lt | TAst.Le | TAst.Eq | TAst.NEq -> raise UnexpectedOperator

let rec codegen_expr env expr =
  match expr with
  | TAst.Integer {int} -> ([],Ll.I64, Ll.IConst64 int)
  | TAst.Boolean {bool} -> ([], Ll.I1, Ll.BConst bool)
  | TAst.BinOp {left; op; right; tp} -> codegen_binop env left op right tp
  | TAst.UnOp {op; operand; tp} -> codegen_unop env op operand tp
  | TAst.Lval lvl ->  codegen_lval env lvl
  | TAst.Assignment {lvl; rhs; tp} -> codegen_assignment env lvl rhs tp
  | TAst.Call {fname; args; tp} ->  codegen_call env fname args tp
and codegen_binop env left op right tp =
  let ll_tp = ll_type_of tp in
  let left_buildlets, left_tp, left_op = codegen_expr env left in
  let right_buildlets, right_tp, right_op = codegen_expr env right in
  let _ = assert(left_tp = right_tp) in
  let _, tmp_alias_sym = Env.insert_tmp_reg env in
  let tmp_op = Ll.Id tmp_alias_sym in
  let final_buildlets = begin match op with
  | TAst.Lor | TAst.Land -> get_short_circuit_insns env tmp_alias_sym left_buildlets left_op op right_buildlets right_op tp
  | TAst.Plus | TAst.Minus | TAst.Mul | TAst.Div | TAst.Rem | TAst.Gt | TAst.Ge | TAst.Lt | TAst.Le | TAst.Eq | TAst.NEq ->
    let binop_insn = get_binop_insn tmp_alias_sym left_op op right_op (tast_type_of right_tp) in
    left_buildlets @right_buildlets @ [binop_insn]
  end in
  (final_buildlets, ll_tp, tmp_op)
and codegen_unop env op operand tp =
  let ll_tp = ll_type_of tp in
  let op_buildlets, op_tp, res_op = codegen_expr env operand in
  let _ = assert(ll_tp = op_tp) in
  let _, tmp_alias_sym = Env.insert_tmp_reg env in
  let tmp_op = Ll.Id tmp_alias_sym in
  let insn = begin match op with
    | TAst.Lnot -> CfgBuilder.add_insn(Some tmp_alias_sym, Ll.Binop(Ll.Xor, Ll.I1, Ll.BConst true, res_op))
    | TAst.Neg -> CfgBuilder.add_insn(Some tmp_alias_sym, Ll.Binop(Ll.Sub, Ll.I64, Ll.IConst64 0L, res_op))
    end in
  (op_buildlets @ [insn], ll_tp, tmp_op)
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
and codegen_call env fname args tp =
  let TAst.Ident {sym = fsym} = fname in
  let ll_ret_tp = ll_type_of tp in
  let args_code = List.map (codegen_expr env) args in
  let get_buildlet (b, _, _) = b in
  let args_buildlets = List.map get_buildlet args_code in 
  let folded_buildlets = List.fold_left ( @ ) [] args_buildlets in
  let get_args_op (_, t, o) = (t, o) in
  let args_ops = List.map get_args_op args_code in
  let ret_op = match tp with
    | TAst.Int | TAst.Bool -> let _, tmp_alias_sym = Env.insert_tmp_reg env in tmp_alias_sym
    | TAst.Void | TAst.ErrorType -> Sym.symbol "dummy"
  in let call_insn = match tp with
    | TAst.Int | TAst.Bool -> CfgBuilder.add_insn (Some ret_op, Ll.Call(ll_ret_tp, Ll.Gid fsym, args_ops))
    | TAst.Void | TAst.ErrorType -> CfgBuilder.add_insn (None, Ll.Call(ll_ret_tp, Ll.Gid fsym, args_ops))
  in (folded_buildlets @ [call_insn], ll_ret_tp, Ll.Id ret_op)


  let codegen_var_delc env var = match var with
  | TAst.Declaration {name : TAst.ident; tp : TAst.typ; body : TAst.expr} -> 
    let ll_type = ll_type_of tp in
    let TAst.Ident {sym} = name in
    let new_env, var_alias_sym = Env.insert_reg env sym in
    let i1 = CfgBuilder.add_alloca (var_alias_sym, ll_type) in
    let asgn_buildlets, asgn_tp, _ = codegen_assignment new_env (TAst.Var {ident = name; tp = tp}) body tp in
    let _ = assert (asgn_tp = ll_type) in
    ([i1] @ asgn_buildlets, new_env)
  
    let rec codegen_var_delcs env vars = 
    match vars with
    | [] -> ([],env)
    | [h] -> 
      let d, e = codegen_var_delc env h in
      (d, e)
    | h :: t -> 
      let d, e1 = codegen_var_delc env h in
      let ds, e2 = codegen_var_delcs e1 t in
      (d @ ds, e2)

let rec codegen_statement env stm =
  match stm with
  | TAst.VarDeclStm declaration_block -> 
    begin match declaration_block with
    | TAst.DeclBlock h -> 
      let dlst, e = codegen_var_delcs env h in 
      dlst, e
      end
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
    (*TODO: don't add else block if it is None*)
    (cond_buildlets @ [term_blk_cond] @ [start_blk_then] @ buildlets_blk_then @ [term_blk_then] @ [start_blk_else] @ buildlets_blk_elbro @ [term_blk_elbro] @ [start_blk_merge], env)
  | TAst.CompoundStm {stms} ->
    let buildlets, _ = codegen_statement_seq env stms in
    (buildlets, env)
  | TAst.BreakStm -> 
    let is_inside = Env.get_loop_sym env in
    begin match is_inside with
    | None -> ([], env)
    | Some {conti = _; brea} -> 
      let term_blk_break = CfgBuilder.term_block(Ll.Br (brea)) in 
      ([term_blk_break], env)
    end
  | TAst.ContinueStm -> 
    let is_inside = Env.get_loop_sym env in
    begin match is_inside with
    | None -> ([], env)
    | Some {conti; brea=_} -> 
      let term_blk_continue = CfgBuilder.term_block(Ll.Br (conti)) in 
      ([term_blk_continue], env)
    end
  | TAst.WhileStm {cond : TAst.expr; body : TAst.statement} -> 
    let _, tmp_cond_sym = Env.insert_label env in
    let _, tmp_merge_sym = Env.insert_label env in 
    let newEnv = Env.set_loop_sym env tmp_cond_sym tmp_merge_sym in
    let cond_buildlets, cond_tp, cond_op = codegen_expr newEnv cond in
    let _ = assert(cond_tp = Ll.I1) in
    let _, tmp_body_sym = Env.insert_label newEnv in
    let term_blk_start = CfgBuilder.term_block(Ll.Br (tmp_cond_sym)) in
    let start_blk_cond = CfgBuilder.start_block(tmp_cond_sym) in
    let term_blk_cond = CfgBuilder.term_block(Ll.Cbr (cond_op, tmp_body_sym, tmp_merge_sym)) in
    let start_blk_body = CfgBuilder.start_block(tmp_body_sym) in
    let buildlets_blk_body, _ = codegen_statement newEnv body in
    let term_blk_body = CfgBuilder.term_block(Ll.Br (tmp_cond_sym)) in
    let start_blk_merge = CfgBuilder.start_block(tmp_merge_sym) in
    let result = [term_blk_start] @ [start_blk_cond] @ cond_buildlets @ [term_blk_cond] @ [start_blk_body] @ buildlets_blk_body @ [term_blk_body] @ [start_blk_merge] in
    (result, env)
  | TAst.ForStm { init : TAst.for_init option; cond : TAst.expr option; update : TAst.expr option; body : TAst.statement } -> 
    let init_buildlets, newEnv = begin match init with
    | None -> [], env
    | Some FIExpr i -> 
      let forExpr, _, _ = codegen_expr env i in
      forExpr, env
    | Some FIDecl declaration_block -> 
      begin match declaration_block with
      | DeclBlock h -> 
        let forDecl, newE = codegen_var_delcs env h in
        forDecl, newE
        end
      end in
    let _, tmp_update_sym = Env.insert_label newEnv in
    let _, tmp_merge_sym = Env.insert_label newEnv in 
    let newEnv2 = Env.set_loop_sym newEnv tmp_update_sym tmp_merge_sym in
    let cond_buildlets, cond_tp, cond_op = begin match cond with
    | None -> 
      let boo = TAst.Boolean {bool = true} in
      let co, ty, op = codegen_expr newEnv2 boo in co, ty, op
    | Some c -> 
      let co, ty, op = codegen_expr newEnv2 c in
      co, ty, op
      end in
    let _ = assert(cond_tp = Ll.I1) in
    let update_buildlets =begin match update with
    | None -> []
    | Some u -> 
      let up, _, _ = codegen_expr newEnv2 u in
      up
      end in
    let buildlets_blk_body, _ = codegen_statement newEnv2 body in
    
    let _, tmp_cond_sym = Env.insert_label newEnv2 in
    let _, tmp_body_sym = Env.insert_label newEnv2 in
    let term_blk_init = CfgBuilder.term_block(Ll.Br (tmp_cond_sym)) in
    let start_blk_cond = CfgBuilder.start_block(tmp_cond_sym) in
    let term_blk_cond = CfgBuilder.term_block(Ll.Cbr (cond_op, tmp_body_sym, tmp_merge_sym)) in
    let start_blk_body = CfgBuilder.start_block(tmp_body_sym) in
    let term_blk_body = CfgBuilder.term_block(Ll.Br (tmp_update_sym)) in
    let start_blk_update = CfgBuilder.start_block(tmp_update_sym) in
    let term_blk_update = CfgBuilder.term_block(Ll.Br (tmp_cond_sym)) in
    let start_blk_merge = CfgBuilder.start_block(tmp_merge_sym) in
    let result = init_buildlets @ [term_blk_init] @ [start_blk_cond] @ cond_buildlets @ [term_blk_cond] @ [start_blk_body] @ buildlets_blk_body @ [term_blk_body] @ [start_blk_update] @ update_buildlets @ [term_blk_update] @ [start_blk_merge] in
    (result, env)
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
