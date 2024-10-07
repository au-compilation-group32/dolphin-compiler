(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

module Sym = Symbol
module TAst = TypedAst

type reg = string * Sym.symbol
let string_of_reg (_, sym) = Sym.name sym

type llvmEnvironment = reg list ref

(* create an initial environment with the given functions defined *)
let make_empty_env :llvmEnvironment = ref []

let insert_reg_by_name env name =
  let index = List.length !env in
  let reg_sym = Sym.symbol (name ^ string_of_int index) in
  let _ = env := (name, reg_sym) :: !env in
  reg_sym

let insert_reg_by_sym env sym =
  let name = Sym.name sym in
  insert_reg_by_name env name

let insert_tmp_reg env = insert_reg_by_name env "tmp"

let rec lookup_aux lst sym =
  let name = Sym.name sym in
  match lst with
  | [] -> raise Not_found
  | h::t ->
    let (h_name, h_sym) = h in
    if h_name = name then h_sym else lookup_aux t sym

let lookup env sym = lookup_aux !env sym