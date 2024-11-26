(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

module Sym = Symbol
module Ast = Ast
module TAst = TypedAst

type identType = 
  | VarTyp of TAst.typ
  | FunTyp of TAst.funtype

type environment = {idents : identType Sym.Table.t;
                    rec_names: (TAst.record_field list) Sym.Table.t;
                    errors : Errors.error list ref;
                    is_inside_loop: bool;
                    expected_ret_tp : TAst.typ;
                    has_all_paths_returned: bool}

let add_rec_to_env env (rsym, body) = {env with rec_names = (Sym.Table.add rsym body env.rec_names)}

let add_stdlib_rec_to_env env rd =
  let TAst.RecDecl {rec_name = TAst.RecordName{sym}; fields} = rd in
  Sym.Table.add sym fields env

let add_fun_to_env env (fsym, ftp) = {env with idents = (Sym.Table.add fsym (FunTyp ftp) env.idents)}

let add_stdlib_fun_to_env env fs =
  let TAst.FuncSig {name = TAst.Ident {sym}; fun_tp} = fs in
  Sym.Table.add sym (FunTyp fun_tp) env

(* create an initial environment with the given functions defined *)
let make_env library_records library_functions =
  let recname_table = List.fold_left add_stdlib_rec_to_env Sym.Table.empty library_records in
  let idents_table = List.fold_left add_stdlib_fun_to_env Sym.Table.empty library_functions in
  {idents = idents_table; rec_names = recname_table; errors = ref []; is_inside_loop = false; expected_ret_tp = TAst.Void; has_all_paths_returned = false}

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