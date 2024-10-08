(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

module Sym = Symbol
module TAst = TypedAst

type reg = {real: Sym.symbol; alias: Sym.symbol}
let string_of_reg (_, sym) = Sym.name sym

type llvmEnvironment = {regs: reg list; counter: int ref}

(* create an initial environment with the given functions defined *)
let make_empty_env :llvmEnvironment = {regs = []; counter = ref 0}

let insert_reg env sym =
  let {regs; counter} = env in
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

let get_alias_sym env sym =
  let {regs; _} = env in
  lookup_aux regs sym
