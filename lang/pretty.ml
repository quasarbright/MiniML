open Exprs
open Values
open Printf
open Format
open Lexing
open Types

let string_of_prim1 p1 =
  match p1 with
    | Not -> "!"
    | UNegate -> "~-"

let string_of_prim2 p2 =
  match p2 with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Modulo -> "%"
    | And -> "&&"
    | Or -> "||"
    | Eq -> "="
    | Neq -> "<>"
    | Less -> "<"
    | LessEq -> "<="
    | Greater -> ">"
    | GreaterEq -> ">="

let rec string_of_expr e =
  match e with
    | EInt(num, _) -> Int64.to_string num
    | EBool(b, _) -> Bool.to_string b
    | EPrim1(prim1, arg_expr, _) -> sprintf "(%s(%s))" (string_of_prim1 prim1) (string_of_expr arg_expr)
    | EPrim2(prim2, left_expr, right_expr, _) -> sprintf "((%s) %s (%s))" (string_of_expr left_expr) (string_of_prim2 prim2) (string_of_expr right_expr)

let string_of_program (p : 'a program) =
  let (e, tag) = p in
  string_of_expr e


let string_of_position (p : position) : string =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;

let string_of_sourcespan ((pstart, pend) : sourcespan) : string =
  sprintf "%s, %d:%d-%d:%d" pstart.pos_fname pstart.pos_lnum (pstart.pos_cnum - pstart.pos_bol)
          pend.pos_lnum (pend.pos_cnum - pend.pos_bol)

let ( >> ) f g x = g (f x)
let string_of_value v =
  match v with
    | VInt(num, _) -> Int64.to_string num
    | VBool(b, _) -> Bool.to_string b
    | VErr(exn, exprs, tag) ->
        let exn_str = Printexc.to_string exn in
        let traceback_strs = exprs |> List.map (get_tag >> string_of_sourcespan) in
        let traceback_str = String.concat "\n" traceback_strs in
        (* let loc_str = string_of_sourcespan tag in *)
        Printf.sprintf "Traceback (most recent call last):\n\n%s\n\n%s" traceback_str exn_str