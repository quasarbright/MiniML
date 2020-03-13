open Exprs
open Values
open Printf
open Format
open Lexing
open Types
open Errors

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
    | EId(name, _) -> name
    | EPrim1(prim1, arg_expr, _) -> sprintf "(%s%s)" (string_of_prim1 prim1) (string_of_expr arg_expr)
    | EPrim2(prim2, left_expr, right_expr, _) -> sprintf "(%s %s %s)" (string_of_expr left_expr) (string_of_prim2 prim2) (string_of_expr right_expr)
    | EIf(cnd,thn,els,_) ->
        sprintf "(if %s then %s else %s)"
          (string_of_expr cnd)
          (string_of_expr thn)
          (string_of_expr els)
    | ELet((names, val_expr, _), body_expr, _) ->
        sprintf "(let %s = %s in %s)"
          (names |> List.map fst |> String.concat " ")
          (string_of_expr val_expr)
          (string_of_expr body_expr)
    | EApp(func_expr, arg_expr, _) -> sprintf "(%s $ %s)" (string_of_expr func_expr) ( string_of_expr arg_expr)

let string_of_program (p : 'a program) =
  let (e, tag) = p in
  string_of_expr e


let string_of_position (p : position) : string =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;

let string_of_sourcespan ((pstart, pend) : sourcespan) : string =
  sprintf "%s, %d:%d-%d:%d" pstart.pos_fname pstart.pos_lnum (pstart.pos_cnum - pstart.pos_bol)
          pend.pos_lnum (pend.pos_cnum - pend.pos_bol)

let rec string_of_typ = function
  | TyBottom _ -> "<bottom>"
  | TyTop _ -> "<top>"
  | TyVar(name, _) -> name
  | TyCons(name, typs, tag) when name = arrow_name -> sprintf "(%s)" (String.concat " -> " (List.map string_of_typ typs))
  | TyCons(name, typs, tag) when name = cross_name -> sprintf "(%s)" (String.concat " * " (List.map string_of_typ typs))
  | TyCons(name, [t], tag) when name = list_name -> sprintf "[%s]" (string_of_typ t)
  | TyCons(name, [], tag) -> name
  | TyCons(name, [t], tag) -> sprintf "(%s %s)" (string_of_typ t) name
  | TyCons(name, ((_::_::_) as typs), tag) -> sprintf "((%s) %s)" (String.concat ", " (List.map string_of_typ typs)) name


let indent_line s = sprintf "\t%s" s

let string_of_error = function
  | DivideByZero(pos) -> sprintf "Divide by zero at %s" (string_of_sourcespan pos)
  | TypeMismatch(expected, actual, pos) ->
      sprintf "Type error at %s: expected %s, but got %s" (string_of_sourcespan pos) (string_of_typ expected) (string_of_typ actual)
  | ArgumentError(msg, pos) -> sprintf "Argument error at %s: %s" (string_of_sourcespan pos) msg
  | InternalError(msg) -> sprintf "Internal error: %s" msg
  | UnboundId(name, pos) -> sprintf "The name %s is not in scope at %s" name (string_of_sourcespan pos)
  | DuplicateId(name, new_pos, old_pos) -> sprintf "The name %s, bound at %s, shadows another binding at %s" name (string_of_sourcespan new_pos) (string_of_sourcespan old_pos)
  | err -> Printexc.to_string err

let string_of_errors errors =
  errors
  |> List.map string_of_error
  |> String.concat "\n\n"

let ( >> ) f g x = g (f x)
let rec string_of_value v =
  let rec helpF (Func(maybe_name, arg_name, body, env, tag)) =
    let string_of_body =
      match body with
        | Left(fv) -> helpF fv
        | Right(e) -> string_of_expr e
    in
    let name_prefix =
      match maybe_name with
        | None -> ""
        | Some(name) -> sprintf "%s: " name
    in
    (* let () = printf "|||%d|||n\n" (List.length env) in *)
    (sprintf "(%s%s -> %s)" name_prefix arg_name string_of_body)
    ^
    (* (if Option.is_some maybe_name
    then *)
    "{" ^
      (env
      |> List.map (fun (name, value) -> sprintf "[%s := %s]" name (string_of_value value))
      |> String.concat ", ")
    ^ "}"
    (* else "") *)

  in
  match v with
    | VInt(num, _) -> Int64.to_string num
    | VBool(b, _) -> Bool.to_string b
    | VFunc(fv) -> helpF fv
    | VErr(exn, exprs, tag) ->
        let exn_str = string_of_error exn in
        let traceback_strs = 
          exprs
          |> List.map
             (fun e -> sprintf "%s\n\t%s" (string_of_sourcespan (get_tag e)) (string_of_expr e))
        in
        let traceback_str = String.concat "\n" traceback_strs in
        (* let loc_str = string_of_sourcespan tag in *)
        Printf.sprintf "Traceback (most recent call last):\n\n%s\n\n%s" traceback_str exn_str