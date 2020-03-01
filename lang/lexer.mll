{
  open Lexing
  open Parser
  open Printf
}

let dec_digit = ['0'-'9']
let signed_int = dec_digit+ | ('-' dec_digit+)

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+

let space = [' ' '\t' '\n']+

rule token = parse
  | "(*" { comment lexbuf }
  | blank { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | signed_int as x { INT (Int64.of_string x) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '%' { MODULO }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '+' { PLUS }
  | '-' { MINUS }
  | "~-" { UNEGATE }
  | "<>" { NEQ }
  | "=" { EQ }
  | '!' { NOT }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LTE }
  | ">=" { GTE }
  | "||" { OR }
  | "&&" { AND }
  | "true" { TRUE }
  | "false" { FALSE }
  | eof { EOF }
  | _ as c { failwith (sprintf "Unrecognized character: %c" c) }

(* does not support nested comments *)
and comment = parse
  | "*)" { token lexbuf }
  | _ { comment lexbuf }