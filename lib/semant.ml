module Sym = Symbol
module TAst = TypedAst
open Ast
module Env = Env

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnreachableControlFlow
exception UnexpectedErrorType

let typecheck_typ = function
| Ast.Int -> TAst.Int
| Ast.Bool -> TAst.Bool

let typecheck_binop = function
| Ast.Plus -> TAst.Plus
| Ast.Minus -> TAst.Minus
| Ast.Mul -> TAst.Mul
| Ast.Div -> TAst.Div
| Ast.Rem -> TAst.Rem
| Ast.Lt -> TAst.Lt
| Ast.Le -> TAst.Le
| Ast.Gt-> TAst.Gt
| Ast.Ge-> TAst.Ge
| Ast.Lor-> TAst.Lor
| Ast.Land-> TAst.Land
| Ast.Eq-> TAst.Eq
| Ast.NEq -> TAst.NEq

let typecheck_unop = function
| Ast.Neg -> TAst.Neg
| Ast.Lnot -> TAst.Lnot

let get_expected_binop_arg_typ = function 
  | Plus | Minus | Mul | Div | Rem -> TAst.Int
  | Lt | Le | Gt | Ge -> TAst.Int
  | Lor | Land -> TAst.Bool 
  | Eq | NEq -> raise UnreachableControlFlow
let get_expected_binop_res_typ = function 
  | Plus | Minus | Mul | Div | Rem -> TAst.Int
  | Lt | Le | Gt | Ge -> TAst.Bool
  | Lor | Land -> TAst.Bool
  | Eq | NEq -> TAst.Bool
let get_expected_unop_arg_typ = function 
  | Neg -> TAst.Int
  | Lnot -> TAst.Bool

(* should return a pair of a typed expression and its inferred type. you can/should use typecheck_expr inside infertype_expr. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer {int} -> (TAst.Integer {int}, TAst.Int)
  | Ast.Boolean {bool} -> (TAst.Boolean {bool}, TAst.Bool)
  | Ast.BinOp {left; op; right} -> infertype_binop env left op right
  | Ast.UnOp {op; operand} -> infertype_unop env op operand
  | Ast.Lval lvl -> infertype_lval env lvl
  | Ast.Assignment {lvl; rhs} -> infertype_assignment env lvl rhs
  | Ast.Call {fname; args} -> infertype_call env fname args
and infertype_binop env left op right =
    match op with
    | Plus | Minus | Mul | Div | Rem | Lt | Le | Gt | Ge | Lor | Land -> 
      let expected_arg_typ = get_expected_binop_arg_typ op in
      let expected_res_typ = get_expected_binop_res_typ op in
      let left_texpr = typecheck_expr env left expected_arg_typ in
      let right_texpr = typecheck_expr env right expected_arg_typ in
      (TAst.BinOp {left = left_texpr; op = typecheck_binop op; right = right_texpr; tp = expected_res_typ}, expected_res_typ)
    | Eq | NEq ->
      let right_texpr, right_tp = infertype_expr env right in
      let left_texpr, left_tp = infertype_expr env left in
      let _ = 
        if right_tp = TAst.Void
        then Env.insert_error env (Errors.InvalidVoidTypeOperand{expr = right})
        else if left_tp = TAst.Void 
        then Env.insert_error env (Errors.InvalidVoidTypeOperand{expr = left})
        else () in
      (TAst.BinOp {left = left_texpr; op = typecheck_binop op; right = right_texpr; tp = TAst.Bool}, TAst.Bool)
and infertype_unop env op operand =
    let (operand_texpr, operand_tp) = infertype_expr env operand in 
    let expected_tp = get_expected_unop_arg_typ op in
    let _ = 
      if operand_tp <> expected_tp
      then Env.insert_error env (Errors.TypeMismatch {expected = expected_tp; actual = operand_tp})
      else () in 
    (TAst.UnOp {op = typecheck_unop op; operand = operand_texpr; tp = expected_tp}, expected_tp)
and infertype_assignment env lvl rhs =
  let _ , lvl_tp = infertype_lval env lvl in
  let rhs_texpr, rhs_tp = infertype_expr env rhs in
  let asgn_tp =
    if rhs_tp = TAst.ErrorType then lvl_tp
    else if lvl_tp = TAst.ErrorType then rhs_tp
    else if lvl_tp = rhs_tp then lvl_tp
    else 
      let err = Errors.TypeMismatch {expected = lvl_tp; actual = rhs_tp} in 
      let _ = Env.insert_error env err in lvl_tp
  in match lvl with Ast.Var Ast.Ident {name} ->
    (TAst.Assignment {lvl = TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = lvl_tp}; rhs = rhs_texpr; tp = asgn_tp}, asgn_tp)
and infertype_lval env lvl =
  match lvl with 
  | Ast.Var Ast.Ident {name} -> 
    let lvl_typ = Env.lookup_var_fun env (Sym.symbol name) in
    match lvl_typ with
    | None ->
      let _ = Env.insert_error env (Errors.LValueNotFound {sym = Sym.symbol name}) in
      (TAst.Lval (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = TAst.ErrorType}), TAst.ErrorType)
    | Some Env.VarTyp vt ->
      (TAst.Lval (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = vt}), vt)
    | Some Env.FunTyp _ ->
      let _ = Env.insert_error env (Errors.LValueInvalid {sym = Sym.symbol name}) in
      (TAst.Lval (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = TAst.ErrorType}), TAst.ErrorType)
and infertype_call env fname args =
  match fname with Ast.Ident {name} ->
    let fun_sym = Sym.symbol name in
    let fun_in_env = Env.lookup_var_fun env fun_sym in
    match fun_in_env with
    | None ->
      let _ = Env.insert_error env (Errors.FunctionUndeclared {sym = fun_sym}) in
      (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = []; tp = TAst.ErrorType}, TAst.ErrorType)
    | Some Env.VarTyp _ ->
      let _ = Env.insert_error env (Errors.FunctionNameInvalid {sym = fun_sym}) in
      (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = []; tp = TAst.ErrorType}, TAst.ErrorType)
    | Some Env.FunTyp TAst.FunTyp {ret; params} ->
      let params_count = List.length params in
      let args_count = List.length args in
      if params_count <> args_count
        then
          let _ = Env.insert_error env (Errors.FunctionParamCountMismatch{sym = fun_sym; expected = params_count; actual = args_count}) in
          (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = []; tp = TAst.ErrorType}, TAst.ErrorType)
        else
          let typecheck_param arg (TAst.Param {paramname = _; typ}) = typecheck_expr env arg typ in
          let typed_params = List.map2 typecheck_param args params in
          (TAst.Call {fname = TAst.Ident {sym = fun_sym}; args = typed_params; tp = ret}, ret)
(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr env expr tp =
  let texpr, texprtp = infertype_expr env expr in
  if texprtp <> tp && texprtp <> TAst.ErrorType && tp <> TAst.ErrorType
  then let _ = Env.insert_error env (Errors.TypeMismatch {expected = tp; actual = texprtp}) in texpr
  else texpr 


let typecheck_var_delc env var = match var with
| Declaration {name : Ast.ident; tp : Ast.typ option; body : Ast.expr} -> 
  let decl_sym = let Ast.Ident{name = s} = name in Sym.symbol s in
  let typed_body, body_tp = infertype_expr env body in
  let _ = 
    if body_tp = TAst.Void
    then Env.insert_error env (Errors.InvalidVoidType{sym = decl_sym})
    else () in
  let stm_tp = match tp with
  | None -> if body_tp = TAst.Void then TAst.ErrorType else body_tp
  | Some t -> 
    let decl_tp = typecheck_typ t in
    match decl_tp with
    | TAst.Int | TAst.Bool ->
      let _ =
        if decl_tp <> body_tp && body_tp <> TAst.ErrorType
        then Env.insert_error env (Errors.TypeMismatch{expected = decl_tp; actual = body_tp})
        else () in
      decl_tp
    | TAst.Void ->
      (* This case is unreachable in phase 1, but it can be in later phases where there is a void type in AST*)
      let _ = Env.insert_error env (Errors.InvalidVoidType{sym = decl_sym}) in
      if body_tp <> TAst.Void then body_tp else TAst.ErrorType
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
  | Ast.ReturnStm {ret : Ast.expr} -> 
    let b = typecheck_expr env ret TAst.Int in 
    let x = TAst.ReturnStm {ret=b} in (x, env)
  | BreakStm -> raise Unimplemented
  | ContinueStm -> raise Unimplemented
  | WhileStm {cond : expr; body : statement} -> raise Unimplemented
  | ForStm { init : for_init option; cond : expr option; update : expr option; body : statement } -> raise Unimplemented
  | Ast.VarDeclStm declaration_block -> 
    begin match declaration_block with
    | DeclBlock h -> 
      let dlst, e = typecheck_var_delcs env h in 
      let decl = TAst.DeclBlock dlst in
      TAst.VarDeclStm decl, e
    end
  | Ast.IfThenElseStm {cond : Ast.expr; thbr : Ast.statement; elbro : Ast.statement option} -> 
    let b = typecheck_expr env cond TAst.Bool in 
    let thS, _ = typecheck_statement env thbr in
    begin match elbro with 
    | Some e -> let elS, _ = typecheck_statement env e in
      (TAst.IfThenElseStm {cond = b; thbr = thS; elbro = Some elS}, env)
    | None ->
      (TAst.IfThenElseStm {cond = b; thbr = thS; elbro = None}, env)
    end
  | Ast.ExprStm {expr : Ast.expr option} -> 
    begin match expr with 
    | Some e ->
      let (b, _) = infertype_expr env e in
      let _ =
        begin match e with
          | Ast.Assignment _ | Ast.Call _ -> ()
          | Ast.Integer _ | Ast.Boolean _ | Ast.BinOp _ | Ast.UnOp _ | Ast.Lval _ -> 
            Env.insert_error env (Errors.ShouldBeCallOrAssignment {expr = b})
        end in
      (TAst.ExprStm {expr=Some b}, env)
    | None -> (TAst.ExprStm {expr=None}, env)
    end
  | Ast.CompoundStm {stms : Ast.statement list} -> 
    let tstmt_list, _ = typecheck_statement_seq env stms in
    let x : TAst.statement = TAst.CompoundStm {stms = tstmt_list} in (x, env)
(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms =
  match stms with
  | [] -> ([], env)
  | h::t ->
    let typed_h, env1 = typecheck_statement env h in
    let typed_t, env2 = typecheck_statement_seq env1 t in
    (typed_h :: typed_t, env2)

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment = Env.make_env Library.library_functions

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
let typecheck_prog prg =
  let env = initial_environment in
  let tprog , _ = typecheck_statement_seq env prg in 
  let _ = match List.rev tprog with 
  | [] -> Env.insert_error env Errors.NoReturn
  | h::_ -> 
    begin match h with 
      | TAst.ReturnStm _ -> ()
      | TAst.VarDeclStm _ | TAst.ExprStm _ | TAst.IfThenElseStm _ | TAst.WhileStm _ | TAst.ForStm _ | TAst.ContinueStm | TAst.BreakStm | TAst.CompoundStm _ ->
        Env.insert_error env Errors.NoReturn
    end in
  tprog, Env.(env.errors)
