open Exprs
open Types

type ('a, 'b) either = Left of 'a | Right of 'b

type 'a value =
  | VInt of int64 * 'a
  | VBool of bool * 'a
  | VFunc of 'a functionValue
  | VErr of exn * 'a expr list * 'a (* exception, "call stack" most recent call last, tag *)
and 'a functionValue = Func of string option * string * ('a functionValue, 'a expr) either * (string * 'a value) list * 'a (* maybe name, argname, body, environment, tag *)

let map_value_tag f v =
  let rec helpV f v =
    match v with
      | VInt(num, tag) -> VInt(num, f tag)
      | VBool(b, tag) -> VBool(b, f tag)
      | VFunc(fv) -> VFunc(helpF f fv)
      | VErr(exn, exprs, tag) -> VErr(exn, exprs, f tag)
  and helpF f (Func(maybe_name, arg_name, body, env, tag)) =
    let tag' = f tag in
    let body' =
      match body with
        | Left(fv) -> Left(helpF f fv)
        | Right(e) -> Right(map_expr_tag f e)
    in
    let env' =
      env
      |> List.map
         (fun (name, value) -> (name, helpV f value))
    in
    Func(maybe_name, arg_name, body', env', tag')
  in helpV f v

(* for some reason, removing the v causes a compile error *)
let untag_value v = map_value_tag ignore v

let tag_of_value v =
  let rec helpV = function
    | VInt(_, tag) -> tag
    | VBool(_, tag) -> tag
    | VFunc(fv) -> helpF fv
    | VErr(_,_,tag) -> tag
  and helpF = function
    Func(_,_,_,_,tag) -> tag
  in
  helpV v

let type_of_value = function
  | VInt(_, tag) -> TyInt(tag)
  | VBool(_, tag) -> TyBool(tag)
  | VFunc _ -> failwith "function types not yet implemented"
  | VErr(_,_,tag) -> TyBottom(tag)