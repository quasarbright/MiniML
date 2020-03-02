open Printf
open Lexing
open Parser
open Lexer
open Exprs
open Pretty

let parse (name : string) lexbuf : sourcespan program  =
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Parser.program Lexer.token lexbuf
  with
  | (Failure msg) as exn ->
     if msg = "lexing: empty token" then
       raise (Failure (sprintf "Lexical error at %s" (string_of_position lexbuf.lex_curr_p)))
     else
       let bt = Printexc.get_raw_backtrace () in
       Printexc.raise_with_backtrace exn bt (* make sure we throw with the same stack trace *)
  | Parsing.Parse_error ->
     begin
       let curr = lexbuf.Lexing.lex_curr_p in
       let line = curr.Lexing.pos_lnum in
       let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
       let tok = Lexing.lexeme lexbuf in
       raise (Failure (sprintf "Parse error at line %d, col %d: token `%s`"
                            line cnum tok))
     end

let parse (name : string) lexbuf : sourcespan program  =
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Parser.program Lexer.token lexbuf
  with
  | (Failure msg) as exn ->
     if msg = "lexing: empty token" then
       raise (Failure (sprintf "Lexical error at %s" (string_of_position lexbuf.lex_curr_p)))
     else
       let bt = Printexc.get_raw_backtrace () in
       Printexc.raise_with_backtrace exn bt (* make sure we throw with the same stack trace *)
  | Parsing.Parse_error ->
     begin
       let curr = lexbuf.Lexing.lex_curr_p in
       let line = curr.Lexing.pos_lnum in
       let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
       let tok = Lexing.lexeme lexbuf in
       raise (Failure (sprintf "Parse error at line %d, col %d: token `%s`"
                            line cnum tok))
     end

(* Read a file into a string *)
let string_of_file (file_name : string) : string =
  let inchan = open_in file_name in
  let ans = really_input_string inchan (in_channel_length inchan) in
  close_in inchan;
  ans

let parse_string (name : string) (s : string) : sourcespan program = 
  let lexbuf = Lexing.from_string s in
  parse name lexbuf

let parse_file (name : string) input_file : sourcespan program = 
  let lexbuf = Lexing.from_channel input_file in
  parse name lexbuf


let run_file runner filename =
  filename
    |> string_of_file
    |> parse_string filename
    |> runner