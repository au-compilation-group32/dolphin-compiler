module Sym = Symbol
module PBox = PrintBox
open TypedAst

let rec typ_to_string = function
| Void -> "void"
| Int -> "int"
| Bool -> "bool"
| Byte  -> "byte"
| Str  -> "str"
| Array {typ; _} -> "[" ^ (typ_to_string typ) ^ "]"
| Record {recordname = TypedAst.RecordName {sym; _}; _} -> Sym.name sym
| Nil -> "nil"
| ErrorType -> "'type error'"

(* producing trees for pretty printing *)
let ident_to_tree (Ident {sym}) = Pretty.make_ident_line (Sym.name sym)
let fieldname_to_tree (FieldName {sym}) = Pretty.make_ident_line (Sym.name sym)
let recordname_to_tree (RecordName {sym}) = Pretty.make_ident_line (Sym.name sym)

let rec typ_to_tree tp =
  match tp with
  | Void -> Pretty.make_typ_line "Void"
  | Int -> Pretty.make_typ_line "Int"
  | Bool -> Pretty.make_typ_line "Bool"
  | Byte -> Pretty.make_typ_line "Byte"
  | Str -> Pretty.make_typ_line "Str"
  | Array {typ; _} -> PBox.hlist ~bars:false [Pretty.make_typ_line "["; typ_to_tree typ;Pretty.make_typ_line "]"] 
  | Record {recordname = RecordName {sym; _}; _} -> Pretty.make_typ_line (Sym.name sym)
  | Nil -> Pretty.make_typ_line "nil"
  | ErrorType -> PBox.line_with_style (PBox.Style.set_bg_color PBox.Style.Red PBox.Style.default) "ErrorType"

let binop_to_tree op =
  match op with
  | Plus -> Pretty.make_keyword_line "PLUS"
  | Minus -> Pretty.make_keyword_line "Minus"
  | Mul -> Pretty.make_keyword_line "Mul"
  | Div -> Pretty.make_keyword_line "Div"
  | Rem -> Pretty.make_keyword_line "Rem"
  | Lt -> Pretty.make_keyword_line "Lt"
  | Le -> Pretty.make_keyword_line "Le"
  | Gt -> Pretty.make_keyword_line "Gt"
  | Ge -> Pretty.make_keyword_line "Ge"
  | Lor -> Pretty.make_keyword_line "Lor"
  | Land -> Pretty.make_keyword_line "Land"
  | Eq -> Pretty.make_keyword_line "Eq"
  | NEq -> Pretty.make_keyword_line "NEq"

let unop_to_tree op =
  match op with
  | Neg -> Pretty.make_keyword_line "Neg"
  | Lnot -> Pretty.make_keyword_line "Lor"

let rec expr_to_tree e =
  match e with
  | Integer {int; _} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "IntLit("; PBox.line (Int64.to_string int); Pretty.make_info_node_line ")"]
  | Boolean {bool; _} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "BooleanLit("; Pretty.make_keyword_line (if bool then "true" else "false"); Pretty.make_info_node_line ")"]
  | String {str; _} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "StringLit("; PBox.line (String.escaped str); Pretty.make_info_node_line ")"]
  | Nil -> PBox.hlist ~bars:false [Pretty.make_info_node_line "Nil"]
  | ArrayInitialization {elem_tp; length_expr; _} ->
    PBox.tree (Pretty.make_info_node_line "ArrayInit")
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "ElemType: "; typ_to_tree elem_tp];
      PBox.hlist ~bars:false [Pretty.make_info_node_line "LengthExpr: "; expr_to_tree length_expr]]
  | RecordInitialization {rec_name; fields; _} ->
    PBox.tree (Pretty.make_info_node_line "RecordInit")
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "RecName: "; recordname_to_tree rec_name];
        PBox.tree (Pretty.make_info_node_line "Fields") (List.map (fun fi -> record_field_init_to_tree fi) fields)]
  | LengthOf {expr; _} -> PBox.tree (Pretty.make_info_node_line "LengthOf") [expr_to_tree expr]
  | BinOp {left; op; right; tp; _} -> PBox.tree (Pretty.make_info_node_line "BinOp") [typ_to_tree tp; expr_to_tree left; binop_to_tree op; expr_to_tree right]
  | UnOp {op; operand; tp; _} -> PBox.tree (Pretty.make_info_node_line "UnOp") [typ_to_tree tp; unop_to_tree op; expr_to_tree operand]
  | Lval l -> PBox.tree (Pretty.make_info_node_line "Lval") [lval_to_tree l]
  | Assignment {lvl; rhs; tp; _} -> PBox.tree (Pretty.make_info_node_line "Assignment") [typ_to_tree tp; lval_to_tree lvl; expr_to_tree rhs]
  | Call {fname; args; tp; _} ->
    PBox.tree (Pretty.make_info_node_line "Call")
      [typ_to_tree tp; 
      PBox.hlist ~bars:false [Pretty.make_info_node_line "FunName: "; ident_to_tree fname];
        PBox.tree (Pretty.make_info_node_line "Args") (List.map (fun e -> expr_to_tree e) args)]
  | Comma {left; right; _} -> PBox.tree (Pretty.make_info_node_line "Comma") [expr_to_tree left; expr_to_tree right]
and lval_to_tree l =
  match l with
  | Var {ident; tp} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "Var("; ident_to_tree ident; Pretty.make_info_node_line ")"; PBox.line " : "; typ_to_tree tp;]
  | Idx {arr; index; _} ->
    PBox.tree (Pretty.make_info_node_line "Idx")
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "Arr: "; expr_to_tree arr];
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Index: "; expr_to_tree index]]
  | Fld {record; field; _}->
    PBox.tree (Pretty.make_info_node_line "Fld")
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "FieldName: "; fieldname_to_tree field];
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Record: "; expr_to_tree record]]
and record_field_init_to_tree (TypedAst.RecordFieldInit {fieldname; rhs; _}) =
  PBox.tree (Pretty.make_info_node_line "Field") [fieldname_to_tree fieldname; expr_to_tree rhs]

let single_declaration_to_tree (Declaration {name; tp; body; _}) =
  PBox.tree (Pretty.make_keyword_line "Declaration") 
    [PBox.hlist ~bars:false [Pretty.make_info_node_line "Ident: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [Pretty.make_info_node_line "Type: "; typ_to_tree tp];
    PBox.hlist ~bars:false [Pretty.make_info_node_line "Body: "; expr_to_tree body]]

let declaration_block_to_tree (DeclBlock declarations) =
  PBox.tree (Pretty.make_keyword_line "VarDecl") (List.map single_declaration_to_tree declarations)

let for_init_to_tree = function
| FIDecl db -> PBox.hlist ~bars:false [PBox.line "ForInitDecl: "; declaration_block_to_tree db]
| FIExpr e -> PBox.hlist ~bars:false [PBox.line "ForInitExpr: "; expr_to_tree e]

let rec statement_to_tree c =
  match c with
  | VarDeclStm db -> PBox.hlist ~bars:false [PBox.line "DeclStm: "; declaration_block_to_tree db]
  | ExprStm {expr; _} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "ExprStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree expr]
  | IfThenElseStm {cond; thbr; elbro; _} ->
    PBox.tree (Pretty.make_keyword_line "IfStm")
      ([PBox.hlist ~bars:false [Pretty.make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [Pretty.make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
        match elbro with None -> [] | Some elbr -> [PBox.hlist ~bars:false [Pretty.make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | WhileStm {cond; body; _} ->
    PBox.tree (Pretty.make_keyword_line "WhileStm") 
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "Cond: "; expr_to_tree cond];
        PBox.hlist ~bars:false [Pretty.make_info_node_line "Body: "; statement_to_tree body]]
  | ForStm {init; cond; update; body; _} ->
    PBox.tree (Pretty.make_keyword_line "ForStm") 
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "Init: "; Option.fold ~none:PBox.empty ~some:for_init_to_tree init];
        PBox.hlist ~bars:false [Pretty.make_info_node_line "Cond: "; Option.fold ~none:PBox.empty ~some:expr_to_tree cond];
        PBox.hlist ~bars:false [Pretty.make_info_node_line "Update: "; Option.fold ~none:PBox.empty ~some:expr_to_tree update];
        PBox.hlist ~bars:false [Pretty.make_info_node_line "Body: "; statement_to_tree body]]
  | BreakStm -> Pretty.make_keyword_line "BreakStm"
  | ContinueStm -> Pretty.make_keyword_line "ContinueStm"
  | CompoundStm {stms; _} -> PBox.tree (Pretty.make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  | ReturnStm {ret; _} -> PBox.hlist ~bars:false [Pretty.make_keyword_line "ReturnValStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let func_body_to_tree stms = 
  PBox.tree (Pretty.make_info_node_line "Body") (statement_seq_to_forest stms)

let func_decl_param_to_tree (Param{paramname; typ; _}) =
  PBox.tree (Pretty.make_keyword_line "Param") 
    [PBox.hlist ~bars:false [Pretty.make_info_node_line "Name: "; ident_to_tree paramname]; 
    PBox.hlist ~bars:false [Pretty.make_info_node_line "Type: "; typ_to_tree typ]]

let func_type_to_tree (FunTyp{ret; params}) =
  PBox.tree (Pretty.make_keyword_line "FuncType") 
    [PBox.hlist ~bars:false [Pretty.make_info_node_line "Return: "; typ_to_tree ret]; 
    PBox.hlist ~bars:false [PBox.tree (Pretty.make_info_node_line "Params: ") (List.map func_decl_param_to_tree params)]]

let func_decl_to_tree fd = 
  let TypedAst.FuncDecl {name; fun_tp; body} = fd in
  PBox.tree (Pretty.make_keyword_line "FuncDecl") 
    [PBox.hlist ~bars:false [Pretty.make_info_node_line "Name: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [Pretty.make_info_node_line "Type: "; func_type_to_tree fun_tp];
    PBox.hlist ~bars:false [PBox.tree (Pretty.make_info_node_line "Body: ") (statement_seq_to_forest body)]]

let rec_field_to_tree (TypedAst.RecordField{fieldname; typ; _}) =
  PBox.tree (Pretty.make_keyword_line "Field") 
    [PBox.hlist ~bars:false [Pretty.make_info_node_line "FieldName: "; fieldname_to_tree fieldname]; 
    PBox.hlist ~bars:false [Pretty.make_info_node_line "Type: "; typ_to_tree typ]]

let rec_decl_to_tree rd = 
  let TypedAst.RecDecl {rec_name; fields;} = rd in
  PBox.tree (Pretty.make_keyword_line "RecDecl") 
    [PBox.hlist ~bars:false [Pretty.make_info_node_line "RecName: "; recordname_to_tree rec_name]; 
    PBox.hlist ~bars:false [PBox.tree (Pretty.make_info_node_line "RecFields: ") (List.map rec_field_to_tree fields)]]

let toplevel_decl_to_tree = function
| TypedAst.RecordDeclaration rd -> rec_decl_to_tree rd
| TypedAst.FunctionDeclaration fd -> func_decl_to_tree fd

let program_to_tree prog = 
  PBox.tree (Pretty.make_info_node_line "Program") (List.map toplevel_decl_to_tree prog)
