(* Errors module *)
module Sym = Symbol
module TAst = TypedAst
module TPretty = TypedPretty
module Location = Location

let loc_to_string loc = PrintBox_text.to_string (Location.location_to_tree ~includefile:false loc)

type error =
| TypeMismatch of {loc: Location.location; expected : TAst.typ; actual : TAst.typ}
| ShouldBeCallOrAssignment of {loc: Location.location}
| NoReturn
| LValueNotFound of {loc: Location.location; sym: Sym.symbol}
| LValueInvalid of {loc: Location.location; sym: Sym.symbol}
| FunctionUndeclared of {loc: Location.location; sym: Sym.symbol}
| FunctionNameInvalid of {loc: Location.location; sym: Sym.symbol}
| FunctionParamCountMismatch of {loc: Location.location; sym: Sym.symbol; expected: int; actual: int}
| InvalidVoidType of {loc: Location.location; sym: Sym.symbol}
| InvalidVoidTypeOperand of {loc: Location.location}
| BreakOrContinueOutsideLoop of {loc: Location.location}
| LexerUnexpectedCharacter of {loc: Location.location; c: char}
| LexerIntegerOutOfRange of {loc: Location.location; str: string}
| LexerUnmatchedBlockComment of {loc: Location.location; str: string}
| ParserSyntaxError of {loc: Location.location; c: char}

(* Useful for printing errors *)
let error_to_string err =
  match err with
  | TypeMismatch {loc; expected; actual} -> Printf.sprintf "%s: Type mismatch: expected %s but found %s." (loc_to_string loc) (TPretty.typ_to_string expected) (TPretty.typ_to_string actual)
  | LValueNotFound {loc; sym} -> Printf.sprintf "%s: LValue %s not found." (loc_to_string loc) (Sym.name sym) 
  | LValueInvalid {loc; sym} -> Printf.sprintf "%s: LValue %s is invalid." (loc_to_string loc) (Sym.name sym)
  | FunctionUndeclared {loc; sym} -> Printf.sprintf "%s: Undeclared function %s." (loc_to_string loc) (Sym.name sym)
  | FunctionNameInvalid {loc; sym} -> Printf.sprintf "%s: Expect function name, but %s is a var name." (loc_to_string loc) (Sym.name sym)
  | FunctionParamCountMismatch{loc; sym; expected; actual} -> Printf.sprintf "%s: Function %s expects %d params, but is given %d params." (loc_to_string loc) (Sym.name sym) expected actual
  | ShouldBeCallOrAssignment {loc} -> Printf.sprintf "%s: Expression Statement must be either Call or Assignment" (loc_to_string loc)
  | NoReturn -> Printf.sprintf "Program has no return."
  | InvalidVoidType {loc; sym} -> Printf.sprintf "%s: Identifier %s has invalid type void." (loc_to_string loc) (Sym.name sym)
  | InvalidVoidTypeOperand {loc} -> Printf.sprintf "%s: Operand has invalid type void." (loc_to_string loc)
  | BreakOrContinueOutsideLoop {loc} -> Printf.sprintf "%s: Break or continue statement must be inside a loop." (loc_to_string loc)
  | LexerUnexpectedCharacter {loc; c} -> Printf.sprintf "%s: Unexpected character %c." (loc_to_string loc) c
  | LexerIntegerOutOfRange {loc; str} -> Printf.sprintf "%s: Integer %s is out of int64 range." (loc_to_string loc) str
  | LexerUnmatchedBlockComment {loc; str} -> Printf.sprintf "%s: Unmatched block comment near %s." (loc_to_string loc) str
  | ParserSyntaxError {loc; c} -> Printf.sprintf "%s: Syntax error near \'%c\'." (loc_to_string loc) c
