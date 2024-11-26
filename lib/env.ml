(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

module Sym = Symbol
module Ast = Ast
module TAst = TypedAst

type identType = 
  | VarTyp of TAst.typ
  | FunTyp of TAst.funtype

type environment = {idents : identType Sym.Table.t;
                    errors : Errors.error list ref;
                    is_inside_loop: bool;
                    expected_ret_tp : TAst.typ;
                    has_all_paths_returned: bool}

let add_fun_to_env env (fsym, ftp) = Sym.Table.add fsym (FunTyp ftp) env

let add_stdlib_fun_to_env env fs =
  let TAst.FuncSig {name = TAst.Ident {sym}; fun_tp} = fs in
  Sym.Table.add sym (FunTyp fun_tp) env

(* create an initial environment with the given functions defined *)
let make_env library_functions =
  let emp = Sym.Table.empty in
  let env =
    List.fold_left 
      add_stdlib_fun_to_env
      emp 
      library_functions
  in {idents = env; errors = ref []; is_inside_loop = false; expected_ret_tp = TAst.Void; has_all_paths_returned = false}

(* insert a local declaration into the environment *)
let insert_local_decl env sym typ =
  let {idents; _} = env in
  {env with idents = Sym.Table.add sym (VarTyp typ) idents}

let insert_error env err =
  let {errors; _} = env in
  errors := err :: !errors

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym =
  let {idents; _} = env in
  Sym.Table.find_opt sym idents

let enter_loop env = {env with is_inside_loop = true}

let is_inside_loop {is_inside_loop; _} = is_inside_loop

let expected_ret_tp {expected_ret_tp; _} = expected_ret_tp

let has_all_paths_returned {has_all_paths_returned; _} = has_all_paths_returned