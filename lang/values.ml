open Exprs
open Types

type 'a value =
  | VInt of int64 * 'a
  | VBool of bool * 'a
  | VErr of exn * 'a expr list * 'a (* exception, "call stack" most recent call last, tag *)


let map_value_tag f v =
  match v with
    | VInt(num, tag) -> VInt(num, f tag)
    | VBool(b, tag) -> VBool(b, f tag)
    | VErr(exn, exprs, tag) -> failwith "can't manipulate error value tag"

(* for some reason, removing the v causes a compile error *)
let untag_value v = map_value_tag ignore v

let tag_of_value = function
  | VInt(_, tag) -> tag
  | VBool(_, tag) -> tag
  | VErr(_,_,tag) -> tag

let type_of_value = function
  | VInt(_, tag) -> TyInt(tag)
  | VBool(_, tag) -> TyBool(tag)
  | VErr(_,_,tag) -> TyBottom(tag)