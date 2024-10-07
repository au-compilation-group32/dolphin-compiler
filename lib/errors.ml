(* Errors module *)
module Sym = Symbol
module TAst = TypedAst
module TPretty = TypedPretty

type error =
| TypeMismatch of {expected : TAst.typ; actual : TAst.typ}
| ShouldBeCallOrAssignment of {expr : Ast.expr}
| LValueNotFound of {sym: Sym.symbol}
| LValueInvalid of {sym: Sym.symbol}
| FunctionUndeclared of {sym: Sym.symbol}
| FunctionNameInvalid of {sym: Sym.symbol}
| FunctionParamCountMismatch of {sym: Sym.symbol; expected: int; actual: int}
(* other errors to be added as needed. *)

(* Useful for printing errors *)
let error_to_string err =
  match err with
  | TypeMismatch {expected; actual; _} -> Printf.sprintf "Type mismatch: expected %s but found %s." (TPretty.typ_to_string expected) (TPretty.typ_to_string actual)
  | LValueNotFound {sym; _} -> Printf.sprintf "LValue %s not found." (Sym.name sym) 
  | LValueInvalid {sym; _} -> Printf.sprintf "LValue %s is invalid." (Sym.name sym)
  | FunctionUndeclared {sym; _} -> Printf.sprintf "Undeclared function %s." (Sym.name sym)
  | FunctionNameInvalid {sym; _} -> Printf.sprintf "Expect function name, but %s is a var name." (Sym.name sym)
  | FunctionParamCountMismatch{sym; expected; actual; _} -> Printf.sprintf "Function %s expects %d params, but is given %d params." (Sym.name sym) expected actual
