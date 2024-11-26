(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

module Sym = Symbol
module TAst = TypedAst

type reg = {real: Sym.symbol; alias: Sym.symbol}
let string_of_reg (_, sym) = Sym.name sym

type is_inside_type = {conti: Sym.symbol; brea: Sym.symbol}

type llvmEnvironment = {regs: reg list; str_lits: (string*Sym.symbol) list ref; counter: int ref; is_inside_loop: is_inside_type option}

(* create an initial environment with the given functions defined *)
let make_empty_env :llvmEnvironment = {regs = []; str_lits = ref []; counter = ref 0; is_inside_loop = None}

let insert_reg env sym =
  let {regs; counter; _} = env in
  let alias_sym = Sym.symbol (Sym.name sym ^ string_of_int !counter) in
  let _ = counter := !counter + 1 in
  ({env with regs = {real = sym; alias = alias_sym} :: regs}, alias_sym)

let insert_tmp_reg env = 
  let tmp_sym = Sym.symbol "tmp" in
  let new_env, alias_sym = insert_reg env tmp_sym in
  new_env, alias_sym

let insert_label env = 
  let tmp_sym = Sym.symbol "label" in
  let new_env, alias_sym = insert_reg env tmp_sym in
  new_env, alias_sym

let insert_arg env sym =
  let name = Symbol.name sym in
  let arg_sym = Sym.symbol (name ^ "_arg") in
  let new_env, alias_sym = insert_reg env arg_sym in
  new_env, alias_sym

let rec lookup_aux lst sym =
  match lst with
  | [] -> failwith ("Symbol " ^ (Sym.name sym) ^ " not found.")
  | h::t ->
    let {real = h_real; alias = h_alias} = h in
    if h_real = sym then h_alias else lookup_aux t sym

let get_alias_sym (env:llvmEnvironment) sym =
  let {regs; counter=_; is_inside_loop=_} = env in
  lookup_aux regs sym

let get_loop_sym env  =
  let {regs=_; counter=_; is_inside_loop: is_inside_type option} = env in
  match is_inside_loop with
  | None -> let x: is_inside_type option= None in x
  | Some {conti; brea} -> let x = Some {conti = conti; brea = brea} in x

let set_loop_sym env con bre  =
  let {is_inside_loop = _; _} = env in
  let inside = {conti = con; brea = bre} in
  let newEnv = {env with is_inside_loop = Some inside} in
  newEnv

let rec lookup_str_lit_aux lst str =
  match lst with
  | [] -> None
  | h::t ->
    let (s, sym) = h in
    if s = str then Some(sym) else lookup_str_lit_aux t str

let rec lookup_str_lit env str =
  let {str_lits; _} = env in
  lookup_str_lit_aux !str_lits str

let insert_str_lit_reg env str = 
  match lookup_str_lit env str with
  | None -> 
    let str_lit_sym = Sym.symbol "str_lit" in
    let new_env, alias_str_lit_sym = insert_reg env str_lit_sym in
    let conv_str_lit_packed_sym = Sym.symbol "conv_str_lit_packed" in
    let new_env, alias_conv_str_lit_packed_sym = insert_reg new_env conv_str_lit_packed_sym in
    let {str_lits; _} = new_env in
    let _ = str_lits := (str, alias_str_lit_sym)::!str_lits in
    new_env, alias_str_lit_sym, alias_conv_str_lit_packed_sym
  | Some s -> env, s, (get_alias_sym env s)
