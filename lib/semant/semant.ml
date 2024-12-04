module Sym = Lib.Symbol
module TAst = Lib.TypedAst
module Ast = Lib.Ast
module Env = Lib.Env
module Errors = Lib.Errors
module Loc = Lib.Location

exception UnreachableControlFlow
exception UnexpectedErrorType
exception UnexpectedNilType

let rec check_if_typ_exist env = function
| Ast.Int _ -> ()
| Ast.Bool _ -> ()
| Ast.Void _ -> ()
| Ast.Byte _ -> ()
| Ast.Str _ -> ()
| Ast.Array {typ; _} -> check_if_typ_exist env typ
| Ast.Record {recordname = Ast.RecordName {name; loc}; _} -> 
  let sym = Sym.symbol name in
  match Env.lookup_rec_type env sym with
  | None -> Env.insert_error env (Errors.RecordUndeclared {loc = loc; rname = sym})
  | Some _ -> ()

let rec typecheck_typ env tp = match tp with
| Ast.Int _ -> TAst.Int
| Ast.Bool _ -> TAst.Bool
| Ast.Void _ -> TAst.Void
| Ast.Byte _ -> TAst.Byte
| Ast.Str _ -> TAst.Str
| Ast.Array {typ; _} ->
  let _ = check_if_typ_exist env typ in
  TAst.Array {typ = typecheck_typ env typ}
| Ast.Record {recordname = Ast.RecordName {name; _}; _} ->
  let _ = check_if_typ_exist env tp in
  TAst.Record {recordname = TAst.RecordName {sym = Sym.symbol name}}

let rec infertype_typ = function
| Ast.Int _ -> TAst.Int
| Ast.Bool _ -> TAst.Bool
| Ast.Void _ -> TAst.Void
| Ast.Byte _ -> TAst.Byte
| Ast.Str _ -> TAst.Str
| Ast.Array {typ; _} ->
  TAst.Array {typ = infertype_typ typ}
| Ast.Record {recordname = Ast.RecordName {name; _}; _} ->
  TAst.Record {recordname = TAst.RecordName {sym = Sym.symbol name}}

let typecheck_binop = function
| Ast.Plus _ -> TAst.Plus
| Ast.Minus _ -> TAst.Minus
| Ast.Mul _ -> TAst.Mul
| Ast.Div _ -> TAst.Div
| Ast.Rem _ -> TAst.Rem
| Ast.Lt _ -> TAst.Lt
| Ast.Le _ -> TAst.Le
| Ast.Gt _-> TAst.Gt
| Ast.Ge _-> TAst.Ge
| Ast.Lor _-> TAst.Lor
| Ast.Land _-> TAst.Land
| Ast.Eq _-> TAst.Eq
| Ast.NEq _ -> TAst.NEq

let typecheck_unop = function
| Ast.Neg _ -> TAst.Neg
| Ast.Lnot _ -> TAst.Lnot

let get_expected_binop_arg_typ op =
  let open Lib.Ast in match op with
  | Plus _ | Minus _ | Mul _ | Div _ | Rem _ -> TAst.Int
  | Lt _ | Le _ | Gt _ | Ge _ -> raise UnreachableControlFlow
  | Lor _ | Land _ -> TAst.Bool 
  | Eq _ | NEq _ -> raise UnreachableControlFlow
let get_expected_binop_res_typ op =
  let open Lib.Ast in match op with
  | Plus _ | Minus _ | Mul _ | Div _ | Rem _ -> TAst.Int
  | Lt _ | Le _ | Gt _ | Ge _ -> TAst.Bool
  | Lor _ | Land _ -> TAst.Bool
  | Eq _ | NEq _ -> TAst.Bool
let get_expected_unop_arg_typ = function 
  | Ast.Neg _ -> TAst.Int
  | Ast.Lnot _ -> TAst.Bool

let find_field_opt field_list query_sym =
  List.find_opt (fun (TAst.RecordField {fieldname= TAst.FieldName{sym}; _}) -> sym = query_sym) field_list
let find_field_init_opt field_init_list query_field =
  let TAst.RecordField {fieldname = TAst.FieldName{sym = query_sym}; _} = query_field in 
  List.find_opt (fun (TAst.RecordFieldInit {fieldname= TAst.FieldName{sym}; _}) -> sym = query_sym) field_init_list 
(* let reorder_field_init expected_fields field_inits =
  let opt_res = List.map (find_field_init_opt field_inits) expected_fields in
  List.filter_map (fun x->x) opt_res *)

let find_missing_field_init expected_fields field_inits =
  match expected_fields with
  | None -> []
  | Some ef -> 
      let missing_fields = 
        List.filter
          (fun query_field ->
            match find_field_init_opt field_inits query_field with
            | None -> true
            | Some _ -> false)
          ef in
      List.map (fun (TAst.RecordField{fieldname = TAst.FieldName{sym}; _}) -> sym) missing_fields

(* should return a pair of a typed expression and its inferred type. you can/should use typecheck_expr inside infertype_expr. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer {int; loc} -> (TAst.Integer {int}, TAst.Int, loc)
  | Ast.Boolean {bool; loc} -> (TAst.Boolean {bool}, TAst.Bool, loc)
  | Ast.Nil {loc} -> (TAst.Nil, TAst.Nil, loc)
  | Ast.String {str; loc} -> (TAst.String {str}, TAst.Str, loc)
  | Ast.ArrayInitialization {elem_tp; length_expr; loc} -> infertype_array_initialization env elem_tp length_expr loc
  | Ast.RecordInitialization {rec_name; fields; loc} -> infertype_record_initialization env rec_name fields loc
  | Ast.LengthOf {expr; loc} -> infertype_length_of env expr loc
  | Ast.BinOp {left; op; right; loc} -> infertype_binop env left op right loc
  | Ast.UnOp {op; operand; loc} -> infertype_unop env op operand loc
  | Ast.Lval lvl -> infertype_lval_expr env lvl
  | Ast.Assignment {lvl; rhs; loc} -> infertype_assignment env lvl rhs loc
  | Ast.Call {fname; args; loc} -> infertype_call env fname args loc
  | Ast.Comma {left; right; loc} -> infertype_comma env left right loc
and infertype_array_initialization env elem_tp length_expr loc =
  let _ =
    match elem_tp with
    | Ast.Void {loc = void_loc} -> Env.insert_error env (Errors.ArrayInvalidElemTypeVoid {loc = void_loc})
    | _ -> () in
  let typed_length_expr = typecheck_expr env length_expr TAst.Int in
  let typed_elem_tp = typecheck_typ env elem_tp in
  let arr_tp = TAst.Array {typ = typed_elem_tp} in
  (TAst.ArrayInitialization {elem_tp = typed_elem_tp; length_expr = typed_length_expr; tp = arr_tp}, arr_tp, loc)
and infertype_record_initialization env rec_name fields loc =
  let Ast.RecordName {name; loc = rec_name_loc} = rec_name in
  let rec_name_sym = Sym.symbol name in
  let expected_fields =
    match Env.lookup_rec_type env rec_name_sym with
    | None -> 
      let _ = Env.insert_error env (Errors.RecordUndeclared {loc = rec_name_loc; rname = rec_name_sym}) in
      None
    | Some f -> Some f in
  let _ =
    match Env.lookup_reserved_rec env rec_name_sym with
    | None -> ()
    | Some _ -> Env.insert_error env (Errors.InitReservedRecord {loc = loc; rname = rec_name_sym}) in
  let typed_rec_name = TAst.RecordName {sym = rec_name_sym} in
  let typed_fields_init = List.map (infertype_record_field_init env rec_name_sym expected_fields) fields in
  let missing_fieldnames = find_missing_field_init expected_fields typed_fields_init in
  let _ =
    if List.length missing_fieldnames > 0
    then Env.insert_error env (Errors.RecordInitMissingFields {loc = loc; field_syms = missing_fieldnames})
    else () in
  let initialized_fields =
    List.map 
    (fun (TAst.RecordFieldInit {fieldname = TAst.FieldName{sym}; _}) -> sym)
    typed_fields_init in
  let duplicated_inits = Sym.find_duplicates initialized_fields in
  let _ =
    if List.length duplicated_inits > 0
    then Env.insert_error env (Errors.RecordDuplicatedFieldnames {loc = loc; rname_sym = rec_name_sym; syms = duplicated_inits})
    else () in
  let tp = TAst.Record {recordname = typed_rec_name} in
  (TAst.RecordInitialization {rec_name = typed_rec_name; fields = typed_fields_init; tp = tp}, tp, loc)
and infertype_length_of env expr loc =
  let typed_expr, expr_tp, _ = infertype_expr env expr in
  let _ =
    match expr_tp with
    | TAst.Str | TAst.Array _ | TAst.ErrorType -> ()
    | _ -> Env.insert_error env (Errors.LengthOfExprInvalidType {loc = loc; expr_tp = expr_tp}) in
  (TAst.LengthOf {expr = typed_expr}, TAst.Int, loc)
and infertype_binop env left op right loc =
    match op with
    | Plus _ | Minus _ | Mul _ | Div _ | Rem _ | Lor _ | Land _ -> 
      let expected_arg_typ = get_expected_binop_arg_typ op in
      let expected_res_typ = get_expected_binop_res_typ op in
      let left_texpr = typecheck_expr env left expected_arg_typ in
      let right_texpr = typecheck_expr env right expected_arg_typ in
      (TAst.BinOp {left = left_texpr; op = typecheck_binop op; right = right_texpr; tp = expected_res_typ}, expected_res_typ, loc)
    | Lt _ | Le _ | Gt _ | Ge _ ->
      let expected_res_typ = get_expected_binop_res_typ op in
      let left_texpr, left_tp, left_loc = infertype_expr env left in
      let right_texpr, right_tp, right_loc = infertype_expr env right in
      let _ = match left_tp with
        | TAst.Int | TAst.Str | TAst.ErrorType -> ()
        | _ -> Env.insert_error env (Errors.TypeMismatchList {loc = left_loc; expected = [TAst.Int; TAst.Str]; actual = left_tp}) in
      let _ = match right_tp with
        | TAst.Int | TAst.Str | TAst.ErrorType -> ()
        | _ -> Env.insert_error env (Errors.TypeMismatchList {loc = right_loc; expected = [TAst.Int; TAst.Str]; actual = right_tp}) in
      let _ =
        if left_tp <> TAst.ErrorType && right_tp <> TAst.ErrorType && left_tp <> right_tp
        then Env.insert_error env (Errors.TypeMismatch {loc = loc; expected = left_tp; actual = right_tp}) else () in
      (TAst.BinOp {left = left_texpr; op = typecheck_binop op; right = right_texpr; tp = expected_res_typ}, expected_res_typ, loc)
    | Eq _ | NEq _ ->
      let right_texpr, right_tp, right_loc = infertype_expr env right in
      let left_texpr, left_tp, left_loc = infertype_expr env left in
      let _ = 
        if right_tp = TAst.Void
        then Env.insert_error env (Errors.InvalidVoidTypeOperand{loc = right_loc})
        else if left_tp = TAst.Void 
        then Env.insert_error env (Errors.InvalidVoidTypeOperand{loc = left_loc})
        else () in
      let _ =
        if right_tp = TAst.Nil then match left_tp with
          | TAst.Nil | TAst.Array _ | TAst.Record _ -> ()
          | _ -> Env.insert_error env (Errors.InvalidComparisonWithNil {loc = loc})
        else if left_tp = TAst.Nil then match right_tp with 
          | TAst.Nil | TAst.Array _ | TAst.Record _ -> ()
          | _ -> Env.insert_error env (Errors.InvalidComparisonWithNil {loc = loc}) in
      let _ =
        if right_tp <> TAst.Void && right_tp <> TAst.Nil && right_tp <> TAst.ErrorType &&
          left_tp <> TAst.Void && left_tp <> TAst.Nil && left_tp <> TAst.ErrorType &&
          left_tp <> right_tp
        then 
          let _ = Printf.printf "mismatch \n" in
          Env.insert_error env (Errors.TypeMismatch {loc = loc; actual = left_tp; expected = right_tp})
        else () in
      (TAst.BinOp {left = left_texpr; op = typecheck_binop op; right = right_texpr; tp = TAst.Bool}, TAst.Bool, loc)
and infertype_unop env op operand loc =
    let (operand_texpr, operand_tp, _) = infertype_expr env operand in 
    let expected_tp = get_expected_unop_arg_typ op in
    let _ = 
      if operand_tp <> expected_tp
      then Env.insert_error env (Errors.TypeMismatch {loc = loc; expected = expected_tp; actual = operand_tp})
      else () in 
    (TAst.UnOp {op = typecheck_unop op; operand = operand_texpr; tp = expected_tp}, expected_tp, loc)
and infertype_assignment env lvl rhs loc =
  let lvl_texpr , lvl_tp, _ = infertype_lval env lvl in
  let rhs_texpr, rhs_tp , _ = infertype_expr env rhs in
  let asgn_tp =
    if rhs_tp = TAst.ErrorType then lvl_tp
    else if lvl_tp = TAst.ErrorType then rhs_tp
    else if lvl_tp = rhs_tp then lvl_tp
    else 
      let err = Errors.TypeMismatch {loc = loc; expected = lvl_tp; actual = rhs_tp} in 
      let _ = Env.insert_error env err in lvl_tp in
  (TAst.Assignment{lvl = lvl_texpr; rhs = rhs_texpr; tp = asgn_tp}, asgn_tp, loc)
and infertype_lval_expr env lvl =
  let typed_lvl, lvl_tp, lvl_loc = infertype_lval env lvl in
  (TAst.Lval typed_lvl, lvl_tp, lvl_loc)
and infertype_lval env lvl : TAst.lval*TAst.typ*Loc.location=
  match lvl with 
  | Ast.Var Ast.Ident {name; loc} -> 
    let lvl_typ = Env.lookup_var_fun env (Sym.symbol name) in
    begin match lvl_typ with
    | None ->
      let _ = Env.insert_error env (Errors.LValueNotFound {loc = loc; sym = Sym.symbol name}) in
      (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = TAst.ErrorType}, TAst.ErrorType, loc)
    | Some Env.VarTyp vt ->
      (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = vt}, vt, loc)
    | Some Env.FunTyp _ ->
      let _ = Env.insert_error env (Errors.LValueInvalid {loc = loc; sym = Sym.symbol name}) in
      (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = TAst.ErrorType}, TAst.ErrorType, loc)
    end
  | Ast.Idx {arr; index; loc} ->
    let typed_arr, arr_tp, arr_loc = infertype_expr env arr in
    let elem_tp = 
      begin match arr_tp with
      | TAst.Array {typ;} -> typ
      | _ -> 
        let _ = Env.insert_error env (Errors.IndexAccessOfNonArray {loc = arr_loc}) in
        TAst.ErrorType
      end in
    let typed_index = typecheck_expr env index TAst.Int in
    (TAst.Idx {arr = typed_arr; index = typed_index; tp = elem_tp}, elem_tp, loc)
  | Ast.Fld {record; field; loc} ->
    let typed_record, rec_tp, rec_loc = infertype_expr env record in
    let Ast.FieldName {name = ast_fieldname; loc = fieldname_loc} = field in
    let typed_fieldname = TAst.FieldName {sym = Sym.symbol ast_fieldname} in
    let field_tp =
      match rec_tp with
      | TAst.Record {recordname;} ->
        let TAst.RecordName {sym = rname_sym} = recordname in
        let lookup_result = Env.lookup_rec_type env rname_sym in
        begin
          match lookup_result with
          | None ->
            let _ = Env.insert_error env (Errors.RecordUndeclared {loc = rec_loc; rname = rname_sym}) in
            TAst.ErrorType
          | Some fields ->
            let find_result = List.find_opt (fun (TAst.RecordField{fieldname; _}) -> fieldname = typed_fieldname) fields in
            begin
              match find_result with
              | None ->
                let _ = Env.insert_error env (Errors.FieldNotExist {expr_tp = rec_tp; sym = Sym.symbol ast_fieldname; loc = fieldname_loc}) in
                TAst.ErrorType
              | Some e ->
                let TAst.RecordField {typ; _} = e in
                typ
            end
        end
      | _ ->
        let _ = Env.insert_error env (Errors.FieldAccessOfNonRecord {expr_tp = rec_tp; loc = loc}) in
        TAst.ErrorType
    in
    ((TAst.Fld {record = typed_record; field = typed_fieldname; tp = field_tp}), field_tp, loc)
and infertype_call env fname args loc =
  match fname with Ast.Ident {name; loc = fname_loc} ->
    let fun_sym = Sym.symbol name in
    let fun_in_env = Env.lookup_var_fun env fun_sym in
    match fun_in_env with
    | None ->
      let _ = Env.insert_error env (Errors.FunctionUndeclared {loc = fname_loc; sym = fun_sym}) in
      (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = []; tp = TAst.ErrorType}, TAst.ErrorType, loc)
    | Some Env.VarTyp _ ->
      let _ = Env.insert_error env (Errors.FunctionNameInvalid {loc = fname_loc; sym = fun_sym}) in
      (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = []; tp = TAst.ErrorType}, TAst.ErrorType, loc)
    | Some Env.FunTyp TAst.FunTyp {ret; params} ->
      let params_count = List.length params in
      let args_count = List.length args in
      if params_count <> args_count
        then
          let _ = Env.insert_error env (Errors.FunctionParamCountMismatch{loc = loc; sym = fun_sym; expected = params_count; actual = args_count}) in
          (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = []; tp = TAst.ErrorType}, TAst.ErrorType, loc)
        else
          let typecheck_param arg (TAst.Param {paramname = _; typ}) = typecheck_expr env arg typ in
          let typed_params = List.map2 typecheck_param args params in
          (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = typed_params; tp = ret}, ret, loc)
and infertype_comma env left right loc =
  let left_texpr, _, _ = infertype_expr env left in
  let right_texpr, right_tp, _ = infertype_expr env right in
  TAst.Comma {left = left_texpr; right = right_texpr; tp = right_tp}, right_tp, loc
and infertype_record_field_init env rec_name_sym expected_fields field_init =
  let Ast.RecordFieldInit {fieldname = Ast.FieldName {name; _}; rhs; loc} = field_init in
  let fieldname_sym = Sym.symbol name in
  let typed_fieldname = TAst.FieldName {sym = fieldname_sym} in
  let typed_rhs, rhs_tp, _ = infertype_expr env rhs in
  let expected_field =
    match expected_fields with
    | None -> None
    | Some ef ->
      match find_field_opt ef fieldname_sym with
      | None ->
        let _ = Env.insert_error env (Errors.RecordHasNoFieldName {loc = loc; rname = rec_name_sym; fieldname = fieldname_sym}) in
        None
      | Some f -> Some f in
  let _ =
    match expected_field with
    | None -> ()
    | Some TAst.RecordField {typ; _} ->
      let _ = typecheck_expr env rhs typ in () in
  TAst.RecordFieldInit {fieldname = typed_fieldname; rhs = typed_rhs; tp = rhs_tp}
(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr env expr tp =
  let texpr, texprtp , loc = infertype_expr env expr in
  if texprtp = TAst.Nil
    then match tp with
      | TAst.Array _ | TAst.Record _ -> texpr
      | _ -> let _ = Env.insert_error env (Errors.InvalidNil {loc = loc; typ = tp}) in texpr
  else if texprtp <> tp && texprtp <> TAst.ErrorType && tp <> TAst.ErrorType
    then let _ = Env.insert_error env (Errors.TypeMismatch {loc = loc; expected = tp; actual = texprtp}) in texpr
  else texpr 


let typecheck_var_delc env var = match var with
| Ast.Declaration {name; tp; body; loc} -> 
  let decl_sym = let Ast.Ident{name = s; loc = _} = name in Sym.symbol s in
  let typed_body, body_tp, body_loc = infertype_expr env body in
  let _ = 
    if body_tp = TAst.Void
    then Env.insert_error env (Errors.InvalidVoidType{loc = body_loc; sym = decl_sym})
    else () in
  let stm_tp = match tp with
  | None ->
    if body_tp = TAst.Void then TAst.ErrorType 
    else 
      if body_tp = TAst.Nil 
      then let _ = Env.insert_error env (Errors.VarDeclWithAmbiguousNil {loc = loc}) in TAst.ErrorType
      else body_tp
  | Some t -> 
    let decl_tp = typecheck_typ env t in
    match decl_tp with
    | TAst.Int | TAst.Bool | TAst.Str | TAst.Byte ->
      let _ =
        if decl_tp <> body_tp && body_tp <> TAst.ErrorType
        then Env.insert_error env (Errors.TypeMismatch{loc = loc; expected = decl_tp; actual = body_tp})
        else () in
      decl_tp
    | TAst.Record _ | TAst.Array _ ->
      let _ =
        if decl_tp <> body_tp && body_tp <> TAst.ErrorType && body_tp <> TAst.Nil
        then Env.insert_error env (Errors.TypeMismatch{loc = loc; expected = decl_tp; actual = body_tp})
        else () in
      decl_tp
    | TAst.Void ->
      let _ = Env.insert_error env (Errors.InvalidVoidType{loc = loc; sym = decl_sym}) in
      if body_tp <> TAst.Void then body_tp else TAst.ErrorType
    | TAst.Nil -> raise UnexpectedNilType
    | TAst.ErrorType -> raise UnexpectedErrorType
  in
  let new_env = Env.insert_local_decl env decl_sym stm_tp in
  (TAst.Declaration {name = TAst.Ident {sym = decl_sym}; tp = stm_tp; body = typed_body}, new_env)

let rec typecheck_var_delcs env vars = 
  match vars with
  | [] -> ([],env)
  | h :: t -> 
    let d, e1 = typecheck_var_delc env h in
    let ds, e2 = typecheck_var_delcs e1 t in
    (d::ds, e2)

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env stm =
  match stm with
  | Ast.VarDeclStm declaration_block -> 
    begin match declaration_block with
    | DeclBlock {declarations; loc = _} -> 
      let dlst, e = typecheck_var_delcs env declarations in 
      let decl = TAst.DeclBlock dlst in
      TAst.VarDeclStm decl, e
    end
  | Ast.IfThenElseStm {cond; thbr; elbro; loc = _} -> 
    let b = typecheck_expr env cond TAst.Bool in 
    let thS, th_env = typecheck_statement env thbr in
    let has_then_br_returned = Env.has_all_paths_returned th_env in
    begin match elbro with 
    | Some e -> let elS, el_env = typecheck_statement env e in
      let has_else_br_returned = Env.has_all_paths_returned el_env in
      let final_env = Env.{env with has_all_paths_returned = has_then_br_returned && has_else_br_returned} in
      (TAst.IfThenElseStm {cond = b; thbr = thS; elbro = Some elS}, final_env)
    | None ->
      let final_env = Env.{env with has_all_paths_returned = has_then_br_returned} in
      (TAst.IfThenElseStm {cond = b; thbr = thS; elbro = None}, final_env)
    end
  | Ast.ExprStm {expr : Ast.expr option; loc : Loc.location} -> 
    begin match expr with 
    | Some e ->
      let (b, _, _) = infertype_expr env e in
      let _ =
        begin match e with
          | Ast.Assignment _ | Ast.Call _ -> ()
          | Ast.Integer _ | Ast.Boolean _ | Ast.BinOp _ | Ast.UnOp _ | Ast.Lval _ | Ast.Comma _ | String _ | ArrayInitialization _ | RecordInitialization _ | Ast.Nil _ | Ast.LengthOf _-> 
            Env.insert_error env (Errors.ShouldBeCallOrAssignment {loc = loc})
        end in
      (TAst.ExprStm {expr=Some b}, env)
    | None -> (TAst.ExprStm {expr=None}, env)
    end
  | Ast.WhileStm {cond; body; loc = _} -> 
    let c = typecheck_expr env cond TAst.Bool in 
    let inside_loop_env = Env.enter_loop env in
    let b, _ = typecheck_statement inside_loop_env body in
    TAst.WhileStm {cond = c; body =b}, env 
  | Ast.ForStm { init; cond; update; body; loc = _} -> 
    let ini, newEnv = begin match init with
    | None -> None, env
    | Some FIExpr i -> 
      let forE, _, _ = infertype_expr env i in
      let forExpr = TAst.FIExpr forE in
      Some forExpr, env
    | Some FIDecl declaration_block -> 
      begin match declaration_block with
      | DeclBlock {declarations; loc = _} -> 
        let forD, newE = typecheck_var_delcs env declarations in
        let forDe = TAst.DeclBlock forD in
        let forDecl = TAst.FIDecl forDe in
        Some forDecl, newE
        end
      end in
    let con = begin match cond with
    | None -> None
    | Some c -> 
      let co = typecheck_expr newEnv c TAst.Bool in
      Some co
      end in
    let upd =begin match update with
    | None -> None
    | Some u -> 
      let up, _, _ = infertype_expr newEnv u in
      Some up
      end in
    let inside_loop_env = Env.enter_loop newEnv in
    let stat, _ = typecheck_statement inside_loop_env body in
    TAst.ForStm{init = ini; cond =con; update =upd; body = stat}, env
  | Ast.BreakStm {loc} -> 
    let _ = 
      if not (Env.is_inside_loop env)
      then Env.insert_error env (Errors.BreakOrContinueOutsideLoop {loc = loc})
      else () in
    TAst.BreakStm, env
  | Ast.ContinueStm {loc} ->
    let _ = 
      if not (Env.is_inside_loop env)
      then Env.insert_error env (Errors.BreakOrContinueOutsideLoop {loc = loc})
      else () in
    TAst.ContinueStm, env
  | Ast.CompoundStm {stms : Ast.statement list; loc = _} -> 
    let tstmt_list, env2 = typecheck_statement_seq env stms in
    let has_returned = Env.has_all_paths_returned env2 in
    let final_env = Env.{env with has_all_paths_returned = has_returned} in
    let x : TAst.statement = TAst.CompoundStm {stms = tstmt_list} in (x, final_env)
  | Ast.ReturnStm {ret; loc} ->
    let expected_ret_tp = Env.expected_ret_tp env in
    let typed_ret = match ret with
      | None ->
        let _ =
          if expected_ret_tp <> TAst.Void
          then Env.insert_error env (Errors.FunctionUnexpectedReturnVoid {loc = loc; typ = expected_ret_tp})
          else () in
        None
      | Some r ->
        let _ =
          if expected_ret_tp = TAst.Void
          then Env.insert_error env (Errors.FunctionVoidReturnExpr {loc = loc})
          else () in
        Some (typecheck_expr env r expected_ret_tp)
    in
    let final_env = Env.{env with has_all_paths_returned = true} in
    let x = TAst.ReturnStm {ret = typed_ret} in (x, final_env)

(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =
  match stms with
  | [] -> ([], env)
  | h::t ->
    let typed_h, env1 = typecheck_statement env h in
    let typed_t, env2 = typecheck_statement_seq env1 t in
    (typed_h :: typed_t, env2)

let infertype_param ~reportError env p =
  let Ast.Param{paramname = Ast.Ident{name = name; loc = _}; typ; loc = loc} = p in
  let param_sym = Sym.symbol name in
  let _ =
    if reportError && typecheck_typ env typ = TAst.Void
    then Env.insert_error env (Errors.FunctionParamInvalidTypeVoid {loc = loc; sym = param_sym})
    else ()
  in
  TAst.Param {paramname = TAst.Ident{sym = param_sym}; typ = typecheck_typ env typ}

let infertype_param_list ~reportError env params = List.map (infertype_param ~reportError:reportError env) params

(*First pass, add dummy rec_decl and func_decl to deal with recursion*)
let rec first_pass_add_toplevel_decl_to_env env td_list =
  let Env.{idents; _} = env in
  match td_list with
  | [] -> env
  | h::t -> match h with
    | Ast.RecordDeclaration rd ->
      let Ast.RecDecl{rec_name = RecordName{name; loc = rname_loc}; fields = _; _} = rd in
      let sym = Sym.symbol name in
      let _ =
        match Env.lookup_rec_type env sym with
        | None -> ()
        | Some _ -> Env.insert_error env (Errors.RecordDuplicateDeclaration {loc = rname_loc; sym = sym})
      in
      let new_env = Env.add_rec_to_env env (sym, []) in
      first_pass_add_toplevel_decl_to_env new_env t
    | Ast.FunctionDeclaration fd -> 
      let Ast.FuncDecl{name = Ident{name; loc = fname_loc}; ret_tp; params; body = _; loc = _} = fd in
      let sym = Sym.symbol name in
      let typed_ret_tp = typecheck_typ env ret_tp in
      let typed_params = infertype_param_list ~reportError:true env params in
      let fun_typ = TAst.FunTyp{ret = typed_ret_tp; params = typed_params} in
      let _ = 
        (*TODO: refactor using Env.lookup_var_fun*)
        if Sym.Table.mem sym idents
        then Env.insert_error env (Errors.FunctionDuplicateDeclaration{loc = fname_loc; sym = sym})
        else () in
      let new_env = Env.add_fun_to_env env (sym, fun_typ) in
      first_pass_add_toplevel_decl_to_env new_env t

let typecheck_field ?(reportError=false) env f =
  let Ast.RecordField {fieldname = Ast.FieldName{name; _}; typ; loc} = f in
  let sym = Sym.symbol name in
  let _ =
    if reportError then
      match typ with
      | Ast.Void _ -> Env.insert_error env (Errors.InvalidVoidType {loc = loc; sym = sym})
      | _ -> ()
    else () in
  let tp = if reportError then typecheck_typ env typ else infertype_typ typ in
  TAst.RecordField {fieldname = TAst.FieldName{sym = sym}; typ = tp}

let typecheck_rec_decl env rd =
  let Ast.RecDecl {rec_name = Ast.RecordName {name; _}; fields; _} = rd in
  let rec_name_sym = Sym.symbol name in
  let typed_name = TAst.RecordName {sym = rec_name_sym} in
  let typed_fields = List.map (typecheck_field env) fields in
  TAst.RecDecl {rec_name = typed_name; fields = typed_fields}

let get_fieldname_sym (Ast.RecordField {fieldname = Ast.FieldName {name; _}; _}) = Sym.symbol name
let rec second_pass_add_toplevel_decl_to_env env td_list =
  match td_list with
  | [] -> env
  | h::t -> 
    match h with
    | Ast.RecordDeclaration rd ->
      let Ast.RecDecl{rec_name = RecordName{name; loc = _}; fields; loc} = rd in
      let recname_sym = Sym.symbol name in
      let fieldname_syms = List.map get_fieldname_sym fields in
      let duplicated_syms = Sym.find_duplicates fieldname_syms in
      let _ =
        if List.length duplicated_syms > 0
        then Env.insert_error env (Errors.RecordDuplicatedFieldnames {loc = loc; rname_sym = recname_sym; syms = duplicated_syms})
        else ()
      in
      let typed_fields = List.map (typecheck_field ~reportError:true env) fields in
      let new_env = Env.add_rec_to_env env (recname_sym, typed_fields) in
      second_pass_add_toplevel_decl_to_env new_env t
    | Ast.FunctionDeclaration _ -> second_pass_add_toplevel_decl_to_env env t

let insert_param_to_env env param =
  let TAst.Param {paramname = TAst.Ident {sym}; typ} = param in
  Env.insert_local_decl env sym typ

let get_param_sym_list typed_params =
  let get_param_sym (TAst.Param {paramname = TAst.Ident {sym}; _}) = sym in
  List.map get_param_sym typed_params

let typecheck_func_decl env fd =
  let Ast.FuncDecl{name = Ident{name = func_name; loc = _}; ret_tp; params; body = func_body; loc = func_decl_loc} = fd in
  let func_name_sym = Sym.symbol func_name in
  let typed_params = infertype_param_list ~reportError:false env params in
  let param_syms = get_param_sym_list typed_params in
  let duplicated_syms = Sym.find_duplicates param_syms in
  let _ =
    if List.length duplicated_syms > 0
    then Env.insert_error env (Errors.FunctionDuplicatedParamnames {loc = func_decl_loc; fname_sym = func_name_sym; syms = duplicated_syms})
    else ()
  in
  let decl_fun_tp = TAst.FunTyp{ret = typecheck_typ env ret_tp; params = typed_params} in
  let Ast.FuncBody{stms; _} = func_body in
  let env2 = Env.{env with expected_ret_tp = typecheck_typ env ret_tp} in
  let env3 = List.fold_left insert_param_to_env env2 typed_params in
  let typed_stms, final_env = typecheck_statement_seq env3 stms in
  let _ =
    if (typecheck_typ env3 ret_tp) <> TAst.Void && not (Env.has_all_paths_returned final_env)
    then Env.insert_error final_env (Errors.FunctionMissingReturn{loc = func_decl_loc; sym = func_name_sym})
    else () in
  TAst.FuncDecl{name = TAst.Ident {sym = func_name_sym}; fun_tp = decl_fun_tp; body = typed_stms}

let typecheck_toplevel_decl env td = match td with
| Ast.RecordDeclaration rd -> TAst.RecordDeclaration (typecheck_rec_decl env rd)
| Ast.FunctionDeclaration fd -> TAst.FunctionDeclaration (typecheck_func_decl env fd)

let check_main_func env =
  match Env.lookup_var_fun env (Sym.symbol "main") with
  | None -> Env.insert_error env Errors.MainFunctionMissing
  | Some fd -> match fd with
    | Env.FunTyp TAst.FunTyp {ret; params} ->
      if ret <> TAst.Int || List.length params <> 0
      then Env.insert_error env Errors.FunctionMainInvalidSignature
      else()
    | Env.VarTyp _ -> raise UnreachableControlFlow

let typecheck_library_func_param (Ast.Param {paramname = Ast.Ident {name; _}; typ; _}) =
  let typed_paramname = TAst.Ident {sym = Sym.symbol name} in
  TAst.Param {paramname = typed_paramname; typ = infertype_typ typ}
let infertype_library_func_sig (Ast.FuncSig {name = Ast.Ident {name; _}; ret_tp; params; _}) =
  let typed_params = List.map (typecheck_library_func_param) params in
  let ftp = TAst.FunTyp {ret = infertype_typ ret_tp; params = typed_params} in
  TAst.FuncSig {name = TAst.ident_of_string name; fun_tp = ftp}

let library_records = DlpStdLib.library_records
let library_header = List.map infertype_library_func_sig DlpStdLib.library_functions

let typecheck_prog prog =
  let library_env = Env.make_env library_records library_header in
  (* Run first pass to add all the declared functions, in case of recursive call*)
  let env = first_pass_add_toplevel_decl_to_env library_env prog in
  let env2 = second_pass_add_toplevel_decl_to_env env prog in
  let _ = check_main_func env2 in
  (* Run third pass for semantic analysis*)
  let tprog = List.map (typecheck_toplevel_decl env2) prog in
  tprog, env2.rec_names, Env.(env2.errors)
