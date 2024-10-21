(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

module Sym = Symbol
module TAst = TypedAst

type reg = {real: Sym.symbol; alias: Sym.symbol}
let string_of_reg (_, sym) = Sym.name sym

type is_inside_type = {conti: Sym.symbol; brea: Sym.symbol}

type llvmEnvironment = {regs: reg list; counter: int ref; is_inside_loop: is_inside_type option}

(* create an initial environment with the given functions defined *)
let make_empty_env :llvmEnvironment = {regs = []; counter = ref 0; is_inside_loop = None}

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
  let {regs=r; counter=c; is_inside_loop = _} = env in
  let inside = {conti = con; brea = bre} in
  let newEnv = {regs=r; counter=c; is_inside_loop = Some inside} in
  newEnv
