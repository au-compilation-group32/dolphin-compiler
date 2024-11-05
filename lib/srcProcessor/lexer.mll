{
  open Parser
  open Lib.Location
  exception UnexpectedCharacter of location*char
  exception IntegerOutOfRange of location*string
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
| ['0'-'9']+ as s {
    let loc = {start_pos = (Lexing.lexeme_start_p lexbuf); end_pos = (Lexing.lexeme_end_p lexbuf)} in
    match Int64.of_string_opt s with
    | None -> raise (IntegerOutOfRange(loc, s))
    | Some i -> INT_LIT i
  }
| '"'[^'"']*'"' as s {STRING_LIT s}
| ['a'-'z' 'A'-'Z' '_']['0'-'9' 'a'-'z' 'A'-'Z' '_']* as s {IDENT (s)}
| _ as c {
    let loc = {start_pos = (Lexing.lexeme_start_p lexbuf); end_pos = (Lexing.lexeme_end_p lexbuf)} in
    raise (UnexpectedCharacter (loc, c))
  }
