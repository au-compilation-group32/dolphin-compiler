{
  open Parser
  exception Error of string
}


rule token = parse
| [' ' '\t'] {token lexbuf}
| '\n' {Lexing.new_line lexbuf; token lexbuf}
| eof   {EOF}
| ['0'-'9']+ as i { INT_LIT (Int64.of_string i) }
| '+' {PLUS}
| ';' {SEMICOLON}
| '{' {LBRACE}
| '}' {RBRACE}
| '(' {LPAREN}
| ')' {RPAREN}
| "if" {IF}
| "else" {ELSE}
| _ {raise (Error "unexpected character\n")}