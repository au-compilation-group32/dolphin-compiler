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
| '+' {PLUS}
| '-' {MINUS}
| '=' {ASSIGN}
| ':' {COLON}
| ',' {COMMA}
| ';' {SEMICOLON}
| '{' {LBRACE}
| '}' {RBRACE}
| '(' {LPAREN}
| ')' {RPAREN}
| "var" {VAR}
| "if" {IF}
| "else" {ELSE}
| "while" {WHILE}
| "for" {FOR}
| "break" {BREAK}
| "continue" {CONTINUE}
| "return" {RETURN}
| "int" {INT}
| "bool" {BOOL}
| ['0'-'9']+ as i { INT_LIT (Int64.of_string i) }
| ['a'-'z' 'A'-'Z' '_']['0'-'9' 'a'-'z' 'A'-'Z' '_']* as s {IDENT (s)}
| _ as c {raise (Error (Printf.sprintf "unexpected character %c\n" c))}