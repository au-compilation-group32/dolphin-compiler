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

%start <Ast.program> prog
// %nonassoc LPAREN RPAREN
%nonassoc COMMA
%left LOR
%left LAND
%left LNOT
%nonassoc LT LE GT GE EQ NEQ
%right ASSIGN
%left PLUS MINUS
%left MUL DIV REM
%%

id:
| i = IDENT {Ast.Ident {name = i; loc = {start_pos = $startpos; end_pos = $endpos}}}

rec_name:
| i = IDENT {Ast.RecordName {name = i; loc = {start_pos = $startpos; end_pos = $endpos}}}

field_name:
| i = IDENT {Ast.FieldName {name = i; loc = {start_pos = $startpos; end_pos = $endpos}}}

tp:
| INT {Ast.Int {loc = {start_pos = $startpos; end_pos = $endpos}}}
| BOOL {Ast.Bool {loc = {start_pos = $startpos; end_pos = $endpos}}}
| VOID {Ast.Void {loc = {start_pos = $startpos; end_pos = $endpos}}}
| BYTE {Ast.Byte {loc = {start_pos = $startpos; end_pos = $endpos}}}
| STRING {Ast.Str {loc = {start_pos = $startpos; end_pos = $endpos}}}

%inline binop:
| PLUS {Ast.Plus{loc = {start_pos = $startpos; end_pos = $endpos}}}
| MINUS {Ast.Minus{loc = {start_pos = $startpos; end_pos = $endpos}}}
| MUL {Ast.Mul{loc = {start_pos = $startpos; end_pos = $endpos}}}
| DIV {Ast.Div{loc = {start_pos = $startpos; end_pos = $endpos}}}
| REM {Ast.Rem{loc = {start_pos = $startpos; end_pos = $endpos}}}
| LT {Ast.Lt{loc = {start_pos = $startpos; end_pos = $endpos}}}
| LE {Ast.Le{loc = {start_pos = $startpos; end_pos = $endpos}}}
| GT {Ast.Gt{loc = {start_pos = $startpos; end_pos = $endpos}}}
| GE {Ast.Ge{loc = {start_pos = $startpos; end_pos = $endpos}}}
| LOR {Ast.Lor{loc = {start_pos = $startpos; end_pos = $endpos}}}
| LAND {Ast.Land{loc = {start_pos = $startpos; end_pos = $endpos}}}
| EQ {Ast.Eq{loc = {start_pos = $startpos; end_pos = $endpos}}}
| NEQ {Ast.NEq{loc = {start_pos = $startpos; end_pos = $endpos}}}

%inline unop:
| MINUS {Ast.Neg{loc = {start_pos = $startpos; end_pos = $endpos}}}
| LNOT {Ast.Lnot{loc = {start_pos = $startpos; end_pos = $endpos}}}

// exp_list:
// {[]}
// | e = exp COMMA el = exp_list {e::el}

exp:
| LPAREN e = exp RPAREN {e}
| i = INT_LIT {Ast.Integer {int = i; loc = {start_pos = $startpos; end_pos = $endpos}}}
| TRUE {Ast.Boolean {bool = true; loc = {start_pos = $startpos; end_pos = $endpos}}}
| FALSE {Ast.Boolean {bool = false; loc = {start_pos = $startpos; end_pos = $endpos}}}
| s = STRING_LIT {Ast.String {str = s; loc = {start_pos = $startpos; end_pos = $endpos}}}
| l = exp o = binop r = exp {Ast.BinOp{left = l; op = o; right = r; loc = {start_pos = $startpos; end_pos = $endpos}}}
| o = unop ex = exp {Ast.UnOp{op = o; operand = ex; loc = {start_pos = $startpos; end_pos = $endpos}}}
| l = lval {Ast.Lval l}
| l = lval ASSIGN ex = exp {Ast.Assignment{lvl = l; rhs = ex; loc = {start_pos = $startpos; end_pos = $endpos}}}
| i = id LPAREN expList = separated_list(COMMA, exp) RPAREN {Ast.Call{fname = i; args = expList; loc = {start_pos = $startpos; end_pos = $endpos}}}
| LPAREN e = comma_exp RPAREN {e}

comma_exp:
| l = exp COMMA r = exp {Ast.Comma{left = l; right = r; loc = {start_pos = $startpos; end_pos = $endpos}}}
| l = comma_exp COMMA r = exp {Ast.Comma{left = l; right = r; loc = {start_pos = $startpos; end_pos = $endpos}}}

exp_or_comma_exp:
| e = exp {e}
| e = comma_exp {e}

lval:
| i = IDENT {Ast.Var (Ast.Ident {name = i; loc = {start_pos = $startpos; end_pos = $endpos}})}

single_decl:
| i = id ASSIGN e = exp {Ast.Declaration {name = i; tp = None; body = e; loc = {start_pos = $startpos; end_pos = $endpos}}}
| i = id COLON t = tp ASSIGN e = exp {Ast.Declaration {name = i; tp = Some(t); body = e; loc = {start_pos = $startpos; end_pos = $endpos}}}

decl_list:
| d = single_decl {[d]}
| d = single_decl COMMA dl = decl_list {d::dl}

for_init:
| e = exp_or_comma_exp {Ast.FIExpr e}
| VAR dl = decl_list {Ast.FIDecl (Ast.DeclBlock{declarations = dl; loc = {start_pos = $startpos; end_pos = $endpos}})}

stm:
| VAR dl = decl_list SEMICOLON {Ast.VarDeclStm (Ast.DeclBlock {declarations = dl; loc = {start_pos = $startpos; end_pos = $endpos}})}
| e = exp_or_comma_exp SEMICOLON {Ast.ExprStm{expr = Some (e); loc = {start_pos = $startpos; end_pos = $endpos}}}
| SEMICOLON {Ast.ExprStm{expr = None; loc = {start_pos = $startpos; end_pos = $endpos}}}
| IF LPAREN c = exp_or_comma_exp RPAREN t = stm {Ast.IfThenElseStm{cond = c; thbr = t; elbro = None; loc = {start_pos = $startpos; end_pos = $endpos}}}
| IF LPAREN c = exp_or_comma_exp RPAREN t = stm ELSE e = stm {Ast.IfThenElseStm{cond = c; thbr = t; elbro = Some e; loc = {start_pos = $startpos; end_pos = $endpos}}}
| WHILE LPAREN c = exp_or_comma_exp RPAREN t = stm {Ast.WhileStm{cond = c; body = t; loc = {start_pos = $startpos; end_pos = $endpos}}}
| FOR LPAREN i = for_init? SEMICOLON c = exp_or_comma_exp? SEMICOLON u = exp_or_comma_exp? RPAREN t = stm {Ast.ForStm{init = i; cond = c; update = u; body = t; loc = {start_pos = $startpos; end_pos = $endpos}}}
| BREAK SEMICOLON {Ast.BreakStm{loc = {start_pos = $startpos; end_pos = $endpos}}}
| CONTINUE SEMICOLON {Ast.ContinueStm{loc = {start_pos = $startpos; end_pos = $endpos}}}
| cs = compound_stm {Ast.CompoundStm{stms = cs; loc = {start_pos = $startpos; end_pos = $endpos}}}
| RETURN e = exp_or_comma_exp? SEMICOLON {Ast.ReturnStm{ret = e; loc = {start_pos = $startpos; end_pos = $endpos}}}

compound_stm:
  LBRACE sl = stm_list RBRACE {sl}

stm_list:
| s = stm sl = stm_list {s::sl}
|                       {[]}

func_param:
  i = id COLON t = tp {Ast.Param {paramname = i; typ = t; loc = {start_pos = $startpos; end_pos = $endpos}}}

func_body:
  LBRACE sl = stm_list RBRACE {Ast.FuncBody {stms = sl; loc = {start_pos = $startpos; end_pos = $endpos}}}

func_decl:
  t = tp i = id LPAREN pl = separated_list(COMMA, func_param) RPAREN fb = func_body {
    Ast.FuncDecl{name = i; ret_tp = t; params = pl; body = fb; loc = {start_pos = $startpos; end_pos = $endpos}}
  }

rec_field:
  name = field_name COLON t = tp {Ast.RecordField {fieldname = name; typ = t; loc = {start_pos = $startpos; end_pos = $endpos}}}

rec_decl:
  RECORD name = rec_name LBRACE fl = separated_list(SEMICOLON, rec_field) RBRACE {
    Ast.RecDecl {rec_name = name; fields = fl; loc = {start_pos = $startpos; end_pos = $endpos}}
  }

toplevel_decl:
| rd = rec_decl {Ast.RecordDeclaration rd}
| fd = func_decl {Ast.FunctionDeclaration fd}

prog:
  fl = list(toplevel_decl) EOF {fl}
