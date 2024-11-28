module Sym = Lib.Symbol
module TAst = Lib.TypedAst
module CfgBuilder = Lib.CfgBuilder
module Env = Lib.LlvmEnv
module Ll = Lib.Ll

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnexpectedErrorType
exception UnexpectedVoidType
exception UnexpectedOperator
exception UnexpectedControlFlow
exception FieldNotFound

let rec type_of_expr = function
  | TAst.Integer _ -> TAst.Int
  | TAst.Boolean _ -> TAst.Bool
  | TAst.BinOp {tp; _} -> tp
  | TAst.UnOp {tp; _} -> tp
  | TAst.Lval lvl -> type_of_lval lvl
  | TAst.Assignment {tp; _} -> tp
  | TAst.Call {tp; _} -> tp
and type_of_lval = function
  | TAst.Var {tp; _} -> tp
  | TAst.Fld{tp; _} -> tp

let string_of_sym (name, i) = name ^ (string_of_int i)

let ll_array = Ll.Ptr (Ll.Namedt (Sym.symbol "array_type"))
let ll_str_of_length len = Ll.Struct [Ll.I64; Ll.Array (len, Ll.I8)]
let ll_type_of ?(raw_records = false)= function
  | TAst.Int -> Ll.I64
  | TAst.Bool -> Ll.I1
  | TAst.Void -> Ll.Void
  | TAst.Byte -> Ll.I8
  | TAst.Str -> ll_array
  | TAst.Array _ -> ll_array
  | TAst.Record {recordname = TAst.RecordName {sym}} ->
    let raw_type = Ll.Namedt sym in
    if raw_records then raw_type else Ll.Ptr raw_type
  | TAst.ErrorType -> raise UnexpectedErrorType

let tast_type_of = function
  | Ll.Void -> TAst.Void
  | Ll.I64 -> TAst.Int
  | Ll.I1 -> TAst.Bool
  | Ll.I8 | Ll.I32 | Ll.Ptr _| Ll.Struct _ | Ll.Array _ | Ll.Fun _ | Ll.Namedt _ -> TAst.ErrorType

let rec heap_size_of env = function
  | TAst.Int -> 8
  | TAst.Bool -> 1
  | TAst.Void -> 1
  | TAst.Byte -> 1
  | TAst.Str -> 1
  | TAst.Array _ -> 1
  | TAst.Record {recordname = TAst.RecordName {sym}} ->
    let fields = Env.lookup_rec_type env sym in
    let size_of_fields = List.map (fun (TAst.RecordField {typ; _}) -> (heap_size_of env typ)) fields in
    List.fold_left ( + ) 0 size_of_fields
  | TAst.ErrorType -> raise UnexpectedErrorType

let rec find_field_index fieldname fields =
  match fields with
  | [] -> raise FieldNotFound
  | h::t ->
    let TAst.RecordField  {fieldname = h_name; _} = h in
    if h_name = fieldname then 0 else 1 + find_field_index fieldname t

let get_gep_path_of_field env tp (field:TAst.fieldname) =
  match tp with
  | TAst.Record {recordname = TAst.RecordName {sym}} ->
    let fields = Env.lookup_rec_type env sym in
    (* let TAst.FieldName {sym = expected_field } = field in *)
    let index = find_field_index field fields in
    [Ll.IConst64 0L; Ll.IConst32 (Int32.of_int index)]
  | _ -> raise UnexpectedControlFlow
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
  | TAst.String {str} -> codegen_string env str
  | TAst.RecordInitialization {rec_name; fields; tp} -> codegen_record_initialization env rec_name fields tp
  | TAst.BinOp {left; op; right; tp} -> codegen_binop env left op right tp
  | TAst.UnOp {op; operand; tp} -> codegen_unop env op operand tp
  | TAst.Lval lvl ->  codegen_lval_expr env lvl
  | TAst.Assignment {lvl; rhs; tp} -> codegen_assignment env lvl rhs tp
  | TAst.Call {fname; args; tp} ->  codegen_call env fname args tp
  | TAst.Comma {left; right; tp} -> codegen_comma env left right tp
and codegen_string env str =
  (*TODO: handle 2 string that is exactly the same*)
  let len = String.length str in
  let _, str_lit_sym = Env.insert_str_lit_reg env str in
  let _, conv_str_lit_packed_sym = Env.insert_conv_reg env in 
  let bitcast = Ll.Bitcast(Ll.Ptr(ll_str_of_length len), Ll.Gid str_lit_sym, ll_array) in
  let bitcast_insn = CfgBuilder.add_insn(Some conv_str_lit_packed_sym, bitcast) in
  ([bitcast_insn], ll_array, Ll.Id conv_str_lit_packed_sym)
and codegen_record_initialization env rec_name field_inits tp =
  (* TODO: refactor this using codegen assignment and codegen lval*)
  let new_env, ptr_sym = Env.insert_ptr_reg env in
  let ptr_op = Ll.Id ptr_sym in
  let ptr_ty = Ll.Ptr Ll.I8 in
  let mem_size = heap_size_of new_env tp in
  let call = Ll.Call(ptr_ty, Ll.Gid (Sym.symbol "allocate_record"), [Ll.I32, Ll.IConst32 (Int32.of_int mem_size)]) in
  let mem_allo_insn = CfgBuilder.add_insn (Some ptr_sym, call) in
  let new_env2, casted_ptr_sym = Env.insert_tmp_reg new_env in
  let casted_ptr_op = Ll.Id casted_ptr_sym in
  let casted_ty = ll_type_of tp in
  let bitcast = Ll.Bitcast(ptr_ty, Ll.Id ptr_sym, casted_ty) in
  let bitcast_insn = CfgBuilder.add_insn(Some casted_ptr_sym, bitcast) in
  let TAst.RecordName {sym = rec_name_sym} = rec_name in
  let fields = Env.lookup_rec_type new_env2 rec_name_sym in
  let init_insns = List.fold_left ( @ ) [] (List.map (codegen_record_field_init new_env2 tp casted_ptr_op) field_inits) in 
  ([mem_allo_insn; bitcast_insn] @ init_insns, casted_ty, casted_ptr_op)
and codegen_record_field_init env rec_tp rec_ptr field_init =
  let TAst.RecordFieldInit {fieldname; rhs; _} = field_init in
  let rhs_buildlets, rhs_tp, rhs_op = codegen_expr env rhs in
  let _, ptr_sym = Env.insert_ptr_reg env in
  let raw_tp = ll_type_of ~raw_records:true rec_tp in
  let gep_path = get_gep_path_of_field env rec_tp fieldname in
  let gep_insn = CfgBuilder.add_insn (Some ptr_sym, Ll.Gep (raw_tp, rec_ptr, gep_path)) in
  let load_insn = CfgBuilder.add_insn (None, Ll.Store(rhs_tp, rhs_op, Ll.Id ptr_sym)) in
  rhs_buildlets @ [gep_insn; load_insn]
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
  let lvl_op, lvl_tp, lvl_insns = codegen_lval env lvl in
  let insn = CfgBuilder.add_insn (None, Ll.Store(rhs_tp, rhs_op, lvl_op)) in
  (lvl_insns @ rhs_buildlets @ [insn], rhs_tp, rhs_op)
and codegen_lval env = function
  | TAst.Var {ident; tp} ->
    let TAst.Ident {sym} = ident in
    let lval_sym = Env.get_alias_sym env sym in
    (* let new_env, tmp_sym = Env.insert_tmp_reg env in
    let load_tmp_insn = CfgBuilder.add_insn (Some tmp_sym, Ll.Load (ll_type_of tp, Ll.Id lval_sym)) in *)
    (Ll.Id lval_sym, ll_type_of tp, [])
  | TAst.Idx _ -> raise Unimplemented
  | TAst.Fld {record; field; tp} ->
    let rec_insn, rec_ll_tp, rec_op = codegen_expr env record in
    (* let new_env, tmp_rec_sym = Env.insert_tmp_reg env in
    let load_rec_insn = CfgBuilder.add_insn (Some tmp_rec_sym, Ll.Load (rec_ll_tp, rec_op)) in *)
    let new_env2, ptr_sym = Env.insert_ptr_reg env in
    (*TODO: implement this path*)
    let raw_tp = ll_type_of ~raw_records:true (type_of_expr record) in
    let gep_path = get_gep_path_of_field env (type_of_expr record) field in
    let gep_insn = CfgBuilder.add_insn (Some ptr_sym, Ll.Gep (raw_tp, rec_op, gep_path)) in
  (Ll.Id ptr_sym, ll_type_of tp, rec_insn @ [gep_insn])
and codegen_lval_expr env lvl =
  (* let lvl_insns , ll_typ, lvl_op = codegen_lval env lvl in *)
  let lvl_op, lvl_tp, lvl_insns = codegen_lval env lvl in
  (* let ll_typ = ll_type_of tp in *)
  let _, tmp_alias_sym = Env.insert_tmp_reg env in
  let tmp_load_insn = CfgBuilder.add_insn (Some tmp_alias_sym, Ll.Load(lvl_tp, lvl_op)) in
  (lvl_insns @ [tmp_load_insn], lvl_tp, Ll.Id tmp_alias_sym)
(* and codegen_lval env lvl =
  match lvl with
  | TAst.Var {ident; tp} ->
    let lvl_op, lvl_insns = ptr_operand_of_lval env lvl in
    let ll_typ = ll_type_of tp in
    ([], ll_typ, lvl_op)
  | TAst.Idx _ -> raise Unimplemented
  | TAst.Fld {record; field; tp} ->
    let lvl_op, lvl_insns = ptr_operand_of_lval env lvl in
    let ll_typ = ll_type_of tp in
    (lvl_insns, ll_typ, lvl_op) *)
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
    | TAst.Int | TAst.Bool | TAst.Str | TAst.Record _-> let _, tmp_alias_sym = Env.insert_tmp_reg env in tmp_alias_sym
    | TAst.Void | TAst.ErrorType -> Sym.symbol "dummy"
  in let call_insn = match tp with
    | TAst.Int | TAst.Bool | TAst.Str | TAst.Record _ -> CfgBuilder.add_insn (Some ret_op, Ll.Call(ll_ret_tp, Ll.Gid fsym, args_ops))
    | TAst.Void | TAst.ErrorType -> CfgBuilder.add_insn (None, Ll.Call(ll_ret_tp, Ll.Gid fsym, args_ops))
  in (folded_buildlets @ [call_insn], ll_ret_tp, Ll.Id ret_op)
and codegen_comma env left right tp =
  let ll_tp = ll_type_of tp in
  let left_buildlets, _, _ = codegen_expr env left in
  let right_buildlets, right_tp, right_op = codegen_expr env right in
  let _ = assert(ll_tp = right_tp) in
  (left_buildlets @ right_buildlets, right_tp, right_op)

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
    (cond_buildlets @ [term_blk_cond] @ [start_blk_then] @ buildlets_blk_then @ [term_blk_then] @ [start_blk_else] @ buildlets_blk_elbro @ [term_blk_elbro] @ [start_blk_merge], env)
  | TAst.CompoundStm {stms} ->
    let buildlets, _ = codegen_statement_seq env stms in
    (buildlets, env)
  | TAst.BreakStm -> 
    let is_inside = Env.get_loop_sym env in
    begin match is_inside with
    | None -> ([], env)
    | Some Env.{conti = _; brea} -> 
      let term_blk_break = CfgBuilder.term_block(Ll.Br (brea)) in 
      let _, tmp_unreachable_sym = Env.insert_label env in 
      let start_blk_unreachable = CfgBuilder.start_block(tmp_unreachable_sym) in
      ([term_blk_break] @ [start_blk_unreachable], env)
    end
  | TAst.ContinueStm -> 
    let is_inside = Env.get_loop_sym env in
    begin match is_inside with
    | None -> ([], env)
    | Some Env.{conti; brea=_} -> 
      let term_blk_continue = CfgBuilder.term_block(Ll.Br (conti)) in 
      let _, tmp_unreachable_sym = Env.insert_label env in 
      let start_blk_unreachable = CfgBuilder.start_block(tmp_unreachable_sym) in
      ([term_blk_continue] @ [start_blk_unreachable], env)
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
    | Some TAst.FIExpr i -> 
      let forExpr, _, _ = codegen_expr env i in
      forExpr, env
    | Some TAst.FIDecl declaration_block -> 
      begin match declaration_block with
      | TAst.DeclBlock h -> 
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
    let buildlets, tr = match ret with
      | Some (r) ->
        let ret_buildlets, ret_tp, ret_operand = codegen_expr env r in
        let tr = CfgBuilder.term_block (Ll.Ret (ret_tp, Some ret_operand)) in
        ret_buildlets, tr
      | None -> [], CfgBuilder.term_block (Ll.Ret (Ll.Void, None))
    in
    let new_env, new_block_sym = Env.insert_label env in
    let start_new_blk = CfgBuilder.start_block(new_block_sym) in
    (buildlets @ [tr; start_new_blk], new_env)
and codegen_statement_seq env stms =
  let merge ret stms = 
    let (current_buildlets, current_env) = ret in
    let stms_buildlets, new_env = codegen_statement current_env stms in
    (current_buildlets @ stms_buildlets, new_env)
  in
  List.fold_left merge ([], env) stms

let codegen_field (TAst.RecordField {typ; _}) = ll_type_of typ
let codegen_rec_decl rd =
  let TAst.RecDecl {rec_name = TAst.RecordName {sym}; fields} = rd in
  let ll_fields = List.map codegen_field fields in
  (sym, Ll.Struct ll_fields)

let codegen_param env p =
  let TAst.Param {paramname; typ} = p in
  let TAst.Ident {sym} = paramname in
  let env, arg_alias_sym = Env.insert_arg env sym in
  let env, local_copy_arg_alias_sym = Env.insert_reg env sym in
  let ll_type = ll_type_of typ in
  let arg_op = Ll.Id arg_alias_sym in
  let local_copy_op = Ll.Id local_copy_arg_alias_sym in
  let alloca_insn = CfgBuilder.add_alloca (local_copy_arg_alias_sym, ll_type) in
  let copy_insn = CfgBuilder.add_insn (None, Ll.Store(ll_type, arg_op, local_copy_op)) in
  ([alloca_insn; copy_insn], arg_alias_sym, env)

let codegen_param_list env params =
  let merge ret param =
    let (current_buildlets, current_arg_uid_list, current_env) = ret in
    let buildlets, arg_uid, new_env = codegen_param current_env param in
    (current_buildlets @ buildlets, current_arg_uid_list @ [arg_uid], new_env)
  in
  List.fold_left merge ([], [], env) params

let ll_type_of_param (TAst.Param {typ; _}) = ll_type_of typ

let codegen_func_decl env fd = 
  let TAst.FuncDecl {name; fun_tp; body} = fd in
  let TAst.FunTyp {ret; params} = fun_tp in
  let TAst.Ident {sym = fname_sym} = name in
  let ll_param_tys = List.map ll_type_of_param params in
  let ll_ftyp = (ll_param_tys, ll_type_of ret) in
  let builder = CfgBuilder.empty_cfg_builder in
  let params_buildlets, params_uids, env_with_arg = codegen_param_list env params in
  let body_buildlets, final_env = codegen_statement_seq env_with_arg body in
  let final_term =
    if ret = TAst.Void
    then CfgBuilder.term_block (Ll.Ret (Ll.Void, None))
    else CfgBuilder.term_block (Ll.Unreachable) in
  let seq_buildlets = CfgBuilder.seq_buildlets (params_buildlets @ body_buildlets @ [final_term]) in
  let cfg = CfgBuilder.get_cfg (seq_buildlets builder) in
  let ll_fdecl = Ll.{fty = ll_ftyp; param = params_uids; cfg = cfg} in
  let renamed_fname_sym = if Sym.name fname_sym = "main" then Sym.symbol "dolphin_fun_main" else fname_sym in
  let Env.{str_lits; _} = final_env in
  (renamed_fname_sym, ll_fdecl)

let codegen_func_sig fs = 
  let (TAst.FuncSig {name = TAst.Ident {sym}; fun_tp = TAst.FunTyp {ret; params}}) = fs in
  let typed_params = List.map ll_type_of_param params in
  (sym, (typed_params, ll_type_of ret))

let codegen_external_decl =
  let stdlib_fun = Semant.library_header in
  List.map codegen_func_sig stdlib_fun

let str_lit_to_gdecl (s, sym) =
  let len = String.length s in
  let ll_str_type = Ll.Array (len, Ll.I8) in
  let gd = (ll_str_of_length len, Ll.GStruct [(Ll.I64, Ll.GInt len); (ll_str_type, Ll.GString s)]) in
  (sym, gd)

let filter_rec_decl tprog =
  List.filter_map (
    function
      | TAst.RecordDeclaration rd -> Some rd
      | _ -> None
  ) tprog

let filter_func_decl tprog =
  List.filter_map (
    function
      | TAst.FunctionDeclaration fd -> Some fd
      | _ -> None
  ) tprog

let codegen_prog tprog reg_names =
  let open Sym in
  let open Ll in
  let env = Env.make_empty_env reg_names in
  let rdecls = List.map (codegen_rec_decl ) (filter_rec_decl tprog) in
  let fdecls = List.map (codegen_func_decl env) (filter_func_decl tprog) in
  let str_lits = env.str_lits in
  let gdecls = List.map str_lit_to_gdecl !str_lits in
  { tdecls    = DlpStdLib.reserved_record_names @ rdecls
  ; extgdecls = []
  ; gdecls    = gdecls
  ; extfuns   = DlpStdLib.runtime_functions @ codegen_external_decl
  ; fdecls = fdecls
  }
