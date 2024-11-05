{
  open Parser
  exception Error of string
}


rule token = parse
| [' ' '\t'] {token lexbuf}
| '\n' {Lexing.new_line lexbuf; token lexbuf}
| eof   {EOF}
| "true" {TRUE}
| "false" {FALSE}
| "length_of" {LENGTHOF}
| '+' {PLUS}
| '-' {MINUS}
| '*' {MUL}
| '/' {DIV}
| '%' {REM}
| '<' {LT}
| "<=" {LE}
| '>' {GT}
| ">=" {GE}
| "||" {LOR}
| "&&" {LAND}
| '!' {LNOT}
| "==" {EQ}
| "!=" {NEQ}
| '=' {ASSIGN}
| '?' {QUESTIONMARK}
| ':' {COLON}
| ',' {COMMA}
| ';' {SEMICOLON}
| '.' {DOT}
| '[' {LBRACKET}
| ']' {RBRACKET}
| '{' {LBRACE}
| '}' {RBRACE}
| '(' {LPAREN}
| ')' {RPAREN}
| "nil" {NIL}
| "var" {VAR}
| "let" {LET}
| "if" {IF}
| "else" {ELSE}
| "while" {WHILE}
| "for" {FOR}
| "break" {BREAK}
| "continue" {CONTINUE}
| "return" {RETURN}
| "new" {NEW}
| "int" {INT}
| "bool" {BOOL}
| "string" {STRING}
| "byte" {BYTE}
| "void" {VOID}
| "record" {RECORD}
| ['0'-'9']+ as i { INT_LIT (Int64.of_string i) }
| '"'[^'"']*'"' as s {STRING_LIT s}
| ['a'-'z' 'A'-'Z' '_']['0'-'9' 'a'-'z' 'A'-'Z' '_']* as s {IDENT (s)}
| _ as c {raise (Error (Printf.sprintf "unexpected character %c\n" c))}