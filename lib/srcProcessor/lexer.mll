{
  open Parser
  open Lib.Location
  exception UnexpectedCharacter of location*char
  exception IntegerOutOfRange of location*string
  exception UnmatchedBlockComment of location*string
}


rule token = parse
| [' ' '\t'] {token lexbuf}
| '\n' {Lexing.new_line lexbuf; token lexbuf}
| "//" {line_comment lexbuf}
| "/*" {block_comment 1 lexbuf}
| "*/" {
    let loc = {start_pos = (Lexing.lexeme_start_p lexbuf); end_pos = (Lexing.lexeme_end_p lexbuf)} in
    raise (UnmatchedBlockComment(loc, "*/"))
}
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
(* TODO: handle min negative int*)
| '0' | ['1'-'9']['0'-'9']* as s {
    let loc = {start_pos = (Lexing.lexeme_start_p lexbuf); end_pos = (Lexing.lexeme_end_p lexbuf)} in
    match Int64.of_string_opt s with
    | None -> raise (IntegerOutOfRange(loc, s))
    | Some i -> INT_LIT i
  }
| '"'[^'"']*'"' as s {
    let len = String.length s in
    STRING_LIT (String.sub s 1 (len-2))
  }
| ['a'-'z' 'A'-'Z' '_']['0'-'9' 'a'-'z' 'A'-'Z' '_']* as s {IDENT (s)}
| _ as c {
    let loc = {start_pos = (Lexing.lexeme_start_p lexbuf); end_pos = (Lexing.lexeme_end_p lexbuf)} in
    raise (UnexpectedCharacter (loc, c))
  }

and block_comment depth = parse
| "*/" {
    if depth = 1
    then token lexbuf
    else block_comment (depth-1) lexbuf
  }
| "/*" {block_comment (depth+1) lexbuf}
| eof {
    let loc = {start_pos = (Lexing.lexeme_start_p lexbuf); end_pos = (Lexing.lexeme_end_p lexbuf)} in
    raise (UnmatchedBlockComment(loc, "eof"))
  }
| _ {block_comment depth lexbuf}

and line_comment = parse
| eof {EOF}
| '\n' {Lexing.new_line lexbuf; token lexbuf}
| _ {line_comment lexbuf}
