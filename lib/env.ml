(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

module Sym = Symbol
module TAst = TypedAst

type identType = 
  | VarTyp of TAst.typ
  | FunTyp of TAst.funtype

type environment = {idents : identType Sym.Table.t ; errors : Errors.error list ref}

(* create an initial environment with the given functions defined *)
let make_env function_types =
  let emp = Sym.Table.empty in
  let env =
    List.fold_left 
      (fun env (fsym, ftp) -> Sym.Table.add fsym (FunTyp ftp) env)
      emp 
      function_types
  in {idents = env; errors = ref []}

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