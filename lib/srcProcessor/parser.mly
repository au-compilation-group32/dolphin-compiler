
// end of file
%token EOF
// string literals
%token <string> STRING_LIT   (* Strings quoted with "" *)
// integer literals
%token <int64> INT_LIT
// booleans
%token TRUE FALSE
// length operation; for arrays and strings
%token LENGTHOF
// arithmetic oprations
%token PLUS MINUS MUL DIV REM
// comparison operators
%token LT LE GT GE
// logical operations
%token LOR LAND LNOT
// equality
%token EQ NEQ
// assignment
%token ASSIGN
// punctuation
%token QUESTIONMARK COLON COMMA SEMICOLON
// accessors
%token DOT LBRACKET RBRACKET
// braces
%token LBRACE RBRACE
// parentheses
%token LPAREN RPAREN
// identifiers
%token <string> IDENT
// keywords
%token NIL VAR LET IF ELSE WHILE FOR BREAK CONTINUE RETURN NEW
// types
%token INT BOOL STRING BYTE VOID RECORD

%{
    open struct module Ast = Lib.Ast end
    open Lib.Location
%}

%start <Ast.statement list> prog

// %left PLUS

%%

id:
| i = IDENT {Ast.Ident {name = i; loc = {start_pos = $startpos; end_pos = $endpos}}}

tp:
| INT {Ast.Int {loc = {start_pos = $startpos; end_pos = $endpos}}}
| BOOL {Ast.Bool {loc = {start_pos = $startpos; end_pos = $endpos}}}

binop:
| PLUS {Ast.Plus{loc = {start_pos = $startpos; end_pos = $endpos}}}
| MINUS {Ast.Minus{loc = {start_pos = $startpos; end_pos = $endpos}}}

exp:
| i = INT_LIT {Ast.Integer {int = i; loc = {start_pos = $startpos; end_pos = $endpos}}}
| TRUE {Ast.Boolean {bool = true; loc = {start_pos = $startpos; end_pos = $endpos}}}
| FALSE {Ast.Boolean {bool = false; loc = {start_pos = $startpos; end_pos = $endpos}}}
| l = exp o = binop r = exp {Ast.BinOp{left = l; op = o; right = r; loc = {start_pos = $startpos; end_pos = $endpos}}}
| l = lval {Ast.Lval l}

lval:
| i = IDENT {Ast.Var (Ast.Ident {name = i; loc = {start_pos = $startpos; end_pos = $endpos}})}

single_decl:
| i = id ASSIGN e = exp {Ast.Declaration {name = i; tp = None; body = e; loc = {start_pos = $startpos; end_pos = $endpos}}}
| i = id COLON t = tp ASSIGN e = exp {Ast.Declaration {name = i; tp = Some(t); body = e; loc = {start_pos = $startpos; end_pos = $endpos}}}

decl_list:
| d = single_decl {[d]}
| d = single_decl COMMA dl = decl_list {d::dl}

stm:
| VAR dl = decl_list SEMICOLON {Ast.VarDeclStm (Ast.DeclBlock {declarations = dl; loc = {start_pos = $startpos; end_pos = $endpos}})}
| e = exp SEMICOLON {Ast.ExprStm{expr = Some (e); loc = {start_pos = $startpos; end_pos = $endpos}}}
| SEMICOLON {Ast.ExprStm{expr = None; loc = {start_pos = $startpos; end_pos = $endpos}}}
| IF LPAREN c = exp RPAREN t = stm {Ast.IfThenElseStm{cond = c; thbr = t; elbro = None; loc = {start_pos = $startpos; end_pos = $endpos}}}
| IF LPAREN c = exp RPAREN t = stm ELSE e = stm {Ast.IfThenElseStm{cond = c; thbr = t; elbro = Some e; loc = {start_pos = $startpos; end_pos = $endpos}}}
| WHILE LPAREN c = exp RPAREN t = stm {Ast.WhileStm{cond = c; body = t; loc = {start_pos = $startpos; end_pos = $endpos}}}
| BREAK SEMICOLON {Ast.BreakStm{loc = {start_pos = $startpos; end_pos = $endpos}}}
| CONTINUE SEMICOLON {Ast.ContinueStm{loc = {start_pos = $startpos; end_pos = $endpos}}}
| cs = compound_stm {Ast.CompoundStm{stms = cs; loc = {start_pos = $startpos; end_pos = $endpos}}}
| RETURN e = exp SEMICOLON {Ast.ReturnStm{ret = e; loc = {start_pos = $startpos; end_pos = $endpos}}}

compound_stm:
  LBRACE sl = stm_list RBRACE {sl}

stm_list:
| s = stm sl = stm_list {s::sl}
|                       {[]}

prog:
  sl = stm_list EOF {sl}
