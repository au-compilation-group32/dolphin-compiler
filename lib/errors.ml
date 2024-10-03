(* Errors module *)
module Sym = Symbol
module TAst = TypedAst
module TPretty = TypedPretty

type error =
| TypeMismatch of {expected : TAst.typ; actual : TAst.typ}
| LValueNotFound of {sym: Sym.symbol}
| LValueInvalid of {sym: Sym.symbol}
(* other errors to be added as needed. *)

(* Useful for printing errors *)
let error_to_string err =
  match err with
  | TypeMismatch {expected; actual; _} -> Printf.sprintf "Type mismatch: expected %s but found %s." (TPretty.typ_to_string expected) (TPretty.typ_to_string actual)
  | LValueNotFound {sym; _} -> Printf.sprintf "LValue %s not found." (Sym.name sym) 
  | LValueInvalid {sym; _} -> Printf.sprintf "LValue %s is invalid." (Sym.name sym)
