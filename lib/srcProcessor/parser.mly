
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
    open Lib.Ast
    open Lib.Location
%}

%start <Lib.Ast.statement list> prog

// %left PLUS

%%

binop:
| PLUS {Plus{loc = {start_pos = $startpos; end_pos = $endpos}}}

exp:
| i = INT_LIT {Integer {int = i; loc = {start_pos = $startpos; end_pos = $endpos}}}
| l = exp o = binop r = exp {BinOp{left = l; op = o; right = r; loc = {start_pos = $startpos; end_pos = $endpos}}}

stm:
| e = exp SEMICOLON {ExprStm{expr = Some (e); loc = {start_pos = $startpos; end_pos = $endpos}}}
| cs = compound_stm {CompoundStm{stms = cs; loc = {start_pos = $startpos; end_pos = $endpos}}}
| IF LPAREN c = exp RPAREN t = stm {IfThenElseStm{cond = c; thbr = t; elbro = None; loc = {start_pos = $startpos; end_pos = $endpos}}}
| IF LPAREN c = exp RPAREN t = stm ELSE e = stm {IfThenElseStm{cond = c; thbr = t; elbro = Some e; loc = {start_pos = $startpos; end_pos = $endpos}}}

compound_stm:
  LBRACE sl = stm_list RBRACE {sl}

stm_list:
| s = stm sl = stm_list {s::sl}
|                       {[]}

prog:
  cs = compound_stm EOF {cs}
