module PBox = PrintBox
open Ast

(* producing trees for pretty printing *)
let typ_style = PBox.Style.fg_color PBox.Style.Green
let ident_style = PBox.Style.fg_color PBox.Style.Yellow
let fieldname_style = ident_style
let recordname_style = ident_style
let keyword_style = PBox.Style.fg_color PBox.Style.Blue

let info_node_style = PBox.Style.fg_color PBox.Style.Cyan

let make_typ_line name = PBox.line_with_style typ_style name
let make_fieldname_line name = PBox.line_with_style fieldname_style name
let make_recordname_line name = PBox.line_with_style recordname_style name
let make_ident_line name = PBox.line_with_style ident_style name
let make_keyword_line name = PBox.line_with_style keyword_style name

let make_info_node_line info = PBox.line_with_style info_node_style info

let ident_to_tree (Ident {name; _}) = make_ident_line name
let fieldname_to_tree (FieldName {name; _}) = make_fieldname_line name
let recordname_to_tree (RecordName {name; _}) = make_recordname_line name

let typ_to_tree tp =
  match tp with
  | Bool _ -> make_typ_line "Bool"
  | Int _ -> make_typ_line "Int"
  | Void _ -> make_typ_line "Void"
  | Byte _ -> make_typ_line "Byte"
  | Str _ -> make_typ_line "Str"
  (*TODO: fix this array name*)
  | Array {typ; _} -> make_typ_line "Array"
  | Record {recordname = RecordName {name; _}; _} -> make_typ_line name

let binop_to_tree op =
    match op with
    | Plus _ -> make_keyword_line "PLUS"
    | Minus _ -> make_keyword_line "Minus"
    | Mul _ -> make_keyword_line "Mul"
    | Div _ -> make_keyword_line "Div"
    | Rem _ -> make_keyword_line "Rem"
    | Lt _ -> make_keyword_line "Lt"
    | Le _ -> make_keyword_line "Le"
    | Gt _ -> make_keyword_line "Gt"
    | Ge _ -> make_keyword_line "Ge"
    | Lor _ -> make_keyword_line "Lor"
    | Land _ -> make_keyword_line "Land"
    | Eq _ -> make_keyword_line "Eq"
    | NEq _ -> make_keyword_line "NEq"
  
let unop_to_tree op =
  match op with
  | Neg _ -> make_keyword_line "Neg"
  | Lnot _ -> make_keyword_line "Lnot"
  
let rec expr_to_tree e =
  match e with
  | Integer {int; _} -> PBox.hlist ~bars:false [make_info_node_line "IntLit("; PBox.line (Int64.to_string int); make_info_node_line ")"]
  | Boolean {bool; _} -> PBox.hlist ~bars:false [make_info_node_line "BooleanLit("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
  | String {str; _} -> PBox.hlist ~bars:false [make_info_node_line "StringLit("; PBox.line (String.escaped str); make_info_node_line ")"]
  | ArrayInitialization {elem_tp; length_expr; _} ->
    PBox.tree (make_info_node_line "ArrayInit")
      [PBox.hlist ~bars:false [make_info_node_line "ElemType: "; typ_to_tree elem_tp];
      PBox.hlist ~bars:false [make_info_node_line "LengthExpr: "; expr_to_tree length_expr]]
  | RecordInitialization {rec_name; fields; _} ->
    PBox.tree (make_info_node_line "RecordInit")
      [PBox.hlist ~bars:false [make_info_node_line "RecName: "; recordname_to_tree rec_name];
        PBox.tree (make_info_node_line "Fields") (List.map (fun fi -> record_field_init_to_tree fi) fields)]
  | LengthOf {expr; _} -> PBox.tree (make_info_node_line "LengthOf") [expr_to_tree expr]
  | BinOp {left; op; right; _} -> PBox.tree (make_info_node_line "BinOp") [expr_to_tree left; binop_to_tree op; expr_to_tree right]
  | UnOp {op; operand; _} -> PBox.tree (make_info_node_line "UnOp") [unop_to_tree op; expr_to_tree operand]
  | Lval l -> PBox.tree (make_info_node_line "Lval") [lval_to_tree l]
  | Assignment {lvl; rhs; _} -> PBox.tree (make_info_node_line "Assignment") [lval_to_tree lvl; expr_to_tree rhs]
  | Call {fname; args; _} ->
    PBox.tree (make_info_node_line "Call")
      [PBox.hlist ~bars:false [make_info_node_line "FunName: "; ident_to_tree fname];
        PBox.tree (make_info_node_line "Args") (List.map (fun e -> expr_to_tree e) args)]
  | Comma {left; right; _} -> PBox.tree (make_info_node_line "Comma") [expr_to_tree left; expr_to_tree right]
and lval_to_tree l =
  match l with
  | Var ident -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]
  | Idx {arr; index; _} ->
    PBox.tree (make_info_node_line "Idx")
      [PBox.hlist ~bars:false [make_info_node_line "Arr: "; expr_to_tree arr];
      PBox.hlist ~bars:false [make_info_node_line "Index: "; expr_to_tree index]]
  | Fld {record; field; _}->
    PBox.tree (make_info_node_line "Fld")
      [PBox.hlist ~bars:false [make_info_node_line "FieldName: "; fieldname_to_tree field];
      PBox.hlist ~bars:false [make_info_node_line "Record: "; expr_to_tree record]]
and record_field_init_to_tree (Ast.RecordFieldInit {fieldname; rhs; _}) =
  PBox.tree (make_info_node_line "Field") [fieldname_to_tree fieldname; expr_to_tree rhs]

let single_declaration_to_tree (Declaration {name; tp; body; _}) =
  PBox.tree (make_keyword_line "Declaration") 
    [PBox.hlist ~bars:false [make_info_node_line "Ident: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [make_info_node_line "Type: "; Option.fold ~none:PBox.empty ~some:typ_to_tree tp];
    PBox.hlist ~bars:false [make_info_node_line "Body: "; expr_to_tree body]]

let declaration_block_to_tree (DeclBlock {declarations; _}) =
PBox.tree (make_keyword_line "VarDecl")  (List.map single_declaration_to_tree declarations)

let for_init_to_tree = function
| FIDecl db -> PBox.hlist ~bars:false [PBox.line "ForInitDecl: "; declaration_block_to_tree db]
| FIExpr e -> PBox.hlist ~bars:false [PBox.line "ForInitExpr: "; expr_to_tree e]

let rec statement_to_tree c =
  match c with
  | VarDeclStm db -> PBox.hlist ~bars:false [PBox.line "DeclStm: "; declaration_block_to_tree db]
  | ExprStm {expr; _} -> PBox.hlist ~bars:false [make_info_node_line "ExprStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree expr]
  | IfThenElseStm {cond; thbr; elbro; _} ->
    PBox.tree (make_keyword_line "IfStm")
      ([PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond]; PBox.hlist ~bars:false [make_info_node_line "Then-Branch: "; statement_to_tree thbr]] @
       match elbro with None -> [] | Some elbr -> [PBox.hlist ~bars:false [make_info_node_line "Else-Branch: "; statement_to_tree elbr]])
  | WhileStm {cond; body; _} ->
    PBox.tree (make_keyword_line "WhileStm") 
      [PBox.hlist ~bars:false [make_info_node_line "Cond: "; expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | ForStm {init; cond; update; body; _} ->
    PBox.tree (make_keyword_line "ForStm") 
      [PBox.hlist ~bars:false [make_info_node_line "Init: "; Option.fold ~none:PBox.empty ~some:for_init_to_tree init];
        PBox.hlist ~bars:false [make_info_node_line "Cond: "; Option.fold ~none:PBox.empty ~some:expr_to_tree cond];
        PBox.hlist ~bars:false [make_info_node_line "Update: "; Option.fold ~none:PBox.empty ~some:expr_to_tree update];
        PBox.hlist ~bars:false [make_info_node_line "Body: "; statement_to_tree body]]
  | BreakStm _ -> make_keyword_line "BreakStm"
  | ContinueStm _ -> make_keyword_line "ContinueStm"
  | CompoundStm {stms; _} -> PBox.tree (make_info_node_line "CompoundStm") (statement_seq_to_forest stms)
  | ReturnStm {ret; _} -> PBox.hlist ~bars:false [make_keyword_line "ReturnValStm: "; Option.fold ~none:PBox.empty ~some:expr_to_tree ret]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let func_body_to_tree stms = 
  PBox.tree (make_info_node_line "Body") (statement_seq_to_forest stms)

let func_decl_param_to_tree (Param{paramname; typ; _}) =
  PBox.tree (make_keyword_line "Param") 
    [PBox.hlist ~bars:false [make_info_node_line "Name: "; ident_to_tree paramname]; 
    PBox.hlist ~bars:false [make_info_node_line "Type: "; typ_to_tree typ]]

let func_decl_to_tree fd = 
  let Ast.FuncDecl {name; ret_tp; params; body; loc = _} = fd in
  let Ast.FuncBody {stms; loc = _} = body in
  PBox.tree (make_keyword_line "FuncDecl") 
    [PBox.hlist ~bars:false [make_info_node_line "Name: "; ident_to_tree name]; 
    PBox.hlist ~bars:false [make_info_node_line "ReturnType: "; typ_to_tree ret_tp];
    PBox.hlist ~bars:false [PBox.tree (make_info_node_line "Params: ") (List.map func_decl_param_to_tree params)];
    PBox.hlist ~bars:false [PBox.tree (make_info_node_line "Body: ") (statement_seq_to_forest stms)]]

let rec_field_to_tree (RecordField{fieldname; typ; _}) =
  PBox.tree (make_keyword_line "Field") 
    [PBox.hlist ~bars:false [make_info_node_line "FieldName: "; fieldname_to_tree fieldname]; 
    PBox.hlist ~bars:false [make_info_node_line "Type: "; typ_to_tree typ]]

let rec_decl_to_tree rd = 
  let Ast.RecDecl {rec_name; fields; loc = _} = rd in
  PBox.tree (make_keyword_line "RecDecl") 
    [PBox.hlist ~bars:false [make_info_node_line "RecName: "; recordname_to_tree rec_name]; 
    PBox.hlist ~bars:false [PBox.tree (make_info_node_line "RecFields: ") (List.map rec_field_to_tree fields)]]

let toplevel_decl_to_tree = function
| Ast.RecordDeclaration rd -> rec_decl_to_tree rd
| Ast.FunctionDeclaration fd -> func_decl_to_tree fd

let program_to_tree prog = 
  PBox.tree (make_info_node_line "Program") (List.map toplevel_decl_to_tree prog)
