(* -- Use this in your solution without modifications *)
module Sym = Symbol

type ident = Ident of {sym : Sym.symbol}
type recordname = RecordName of {sym : Sym.symbol}
type fieldname = FieldName of {sym : Sym.symbol}

let ident_of_string name = Ident {sym = Sym.symbol name}

type typ = 
| Int
| Bool
| Void
| Byte
| Str
| Array of {typ : typ;}
| Record of {recordname : recordname}
| ErrorType


type binop = | Plus | Minus | Mul | Div | Rem | Lt 
  | Le | Gt | Ge | Lor | Land | Eq | NEq

type unop = | Neg | Lnot

type expr =
| Integer of {int : int64}
| Boolean of {bool : bool}
| Nil
| String of {str: string}
| ArrayInitialization of {tp: typ; length_expr: expr}
| RecordInitialization of {rec_tp: typ; fields: record_field_init list}
| LengthOf of {ident: ident}
| BinOp of {left : expr; op : binop; right : expr; tp : typ}
| UnOp of {op : unop; operand : expr; tp : typ}
| Lval of lval
| Assignment of {lvl : lval; rhs : expr; tp : typ}
| Call of {fname : ident; args : expr list; tp : typ}
| Comma of {left : expr; right : expr; tp : typ}
and lval =
| Var of {ident : ident; tp : typ}
| Idx of {arr: expr; index: expr}
| Fld of {record: expr; field: fieldname}
and record_field_init = RecordFieldInit of {recordname: recordname; expr: expr}

type single_declaration = Declaration of {name : ident; tp : typ; body : expr}

type declaration_block = DeclBlock of single_declaration list

type for_init =
| FIDecl of declaration_block
| FIExpr of expr

type statement =
| VarDeclStm of declaration_block
| ExprStm of {expr : expr option}
| IfThenElseStm of {cond : expr; thbr : statement; elbro : statement option}
| WhileStm of {cond : expr; body : statement}
| ForStm of { init : for_init option 
            ; cond : expr option
            ; update : expr option
            ; body : statement }
| BreakStm
| ContinueStm
| CompoundStm of {stms : statement list}
| ReturnStm of {ret : expr option}

type param = Param of {paramname: ident; typ : typ}

type funtype = FunTyp of {ret : typ; params : param list}

type function_declaration = FuncDecl of {name : ident; fun_tp : funtype; body : statement list}
type function_signature = FuncSig of {name : ident; fun_tp : funtype;}

type record_field = RecordField of {fieldname: fieldname; typ: typ}

type record_declaration = RecDecl of {rec_name: recordname; fields: record_field list}

type toplevel_declaration =
| RecordDeclaration of record_declaration
| FunctionDeclaration of function_declaration

type program = toplevel_declaration list
