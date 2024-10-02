module Sym = Symbol
module TAst = TypedAst
open Ast
open Env

exception Unimplemented (* your code should eventually compile without this exception *)
exception UnreachableControlFlow

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
  | Neg -> raise Unimplemented
  | Lnot -> raise Unimplemented

(* should return a pair of a typed expression and its inferred type. you can/should use typecheck_expr inside infertype_expr. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer {int} -> (TAst.Integer {int}, TAst.Int)
  | Ast.Boolean {bool} -> (TAst.Boolean {bool}, TAst.Bool)
  | Ast.BinOp {left; op; right} -> infertype_binop env left op right
  | Ast.UnOp {op; operand} -> raise Unimplemented
  | Ast.Lval lvl -> infertype_lval env lvl
  | Ast.Assignment {lvl; rhs} -> infertype_assignment env lvl rhs
  | Ast.Call _ -> raise Unimplemented
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
      let left_texpr = typecheck_expr env left right_tp in
      (TAst.BinOp {left = left_texpr; op = typecheck_binop op; right = right_texpr; tp = TAst.Bool}, TAst.Bool)
and infertype_assignment env lvl rhs =
  let _ , lvl_tp = infertype_lval env lvl in
  let rhs_texpr, rhs_tp = infertype_expr env rhs in
  let asgn_tp =
    if rhs_tp = TAst.ErrorType then lvl_tp
    else if lvl_tp = TAst.ErrorType then rhs_tp
    else if lvl_tp = rhs_tp then lvl_tp
    else let _ = Errors.TypeMismatch {expected = lvl_tp; actual = rhs_tp} in lvl_tp
  in match lvl with Ast.Var Ast.Ident {name} ->
    TAst.Assignment {lvl = TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = lvl_tp}; rhs = rhs_texpr; tp = asgn_tp}, asgn_tp

and infertype_lval env lvl =
  match lvl with 
  | Ast.Var Ast.Ident {name} -> 
    let lvl_typ = Env.lookup_var_fun env (Sym.symbol name) in
    match lvl_typ with
    | None ->
      let _ = Env.insert_error env (Errors.LValueNotFound {sym = Sym.symbol name}) in
          TAst.Lval (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = TAst.ErrorType}), TAst.ErrorType
    | Some t ->
      match t with
      | VarTyp vt -> TAst.Lval (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = vt}), vt
      | FunTyp _ -> 
        let _ = Env.insert_error env (Errors.LValueInvalid {sym = Sym.symbol name}) in
        TAst.Lval (TAst.Var {ident = TAst.Ident {sym = Sym.symbol name}; tp = TAst.ErrorType}), TAst.ErrorType 

(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr env expr tp =
  let texpr, texprtp = infertype_expr env expr in
  if texprtp <> tp && texprtp <> TAst.ErrorType && tp <> TAst.ErrorType
  then let _ = Env.insert_error env (Errors.TypeMismatch {expected = tp; actual = texprtp}) in texpr
  else texpr 

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env stm =
  match stm with
  | Ast.ReturnStm {ret : Ast.expr} -> 
    let (b , t) = infertype_expr env ret in 
    let x : TAst.statement = TAst.ReturnStm {ret=b} in x
  | Ast.VarDeclStm {name : Ast.ident; tp : Ast.typ option; body : Ast.expr} -> 
    let (b, t) = infertype_expr env body in
    let na : string = match name with Ast.Ident{name} -> name in
    let (s,y) = Sym.symbol(na) in
    let sy : Sym.symbol = (s,y) in
    let n : TAst.ident = TAst.Ident {sym=sy} in
    let x : TAst.statement = TAst.VarDeclStm {name = n; tp= t; body=b} in x
  | Ast.IfThenElseStm {cond : Ast.expr; thbr : Ast.statement; elbro : Ast.statement option} -> 
    let (b, t) = infertype_expr env cond in 
    let thS : TAst.statement = typecheck_statement env thbr in
    begin match elbro with 
    | Some e -> let elS : TAst.statement = typecheck_statement env e in
      let x : TAst.statement = TAst.IfThenElseStm {cond = b; thbr = thS; elbro = Some elS} in x
    | None -> let elS : TAst.statement option = None in 
      let x : TAst.statement = TAst.IfThenElseStm {cond = b; thbr = thS; elbro = elS} in x
      end
  | Ast.ExprStm {expr : Ast.expr option} -> 
    begin match expr with 
    | Some e ->
      let (b, _) = infertype_expr env e in
      let b2 : TAst.expr option = Some b in 
      let x = TAst.ExprStm {expr=b2} in x
    | None -> 
      let n : TAst.expr option = None in 
      let x = TAst.ExprStm {expr = n} in x 
    end
  | Ast.CompoundStm {stms : Ast.statement list} -> 
    let sL elem = 
      typecheck_statement env elem 
    in
    let newList = List.map sL stms in 
    let x : TAst.statement = TAst.CompoundStm {stms = newList} in x
(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms = raise Unimplemented

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment = Env.make_env Library.library_functions

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
let typecheck_prog prg = raise Unimplemented
