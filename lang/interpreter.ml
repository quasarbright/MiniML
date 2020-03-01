open Exprs
open Pretty

type 'a value =
  | VInt of int64 * 'a
  | VBool of bool * 'a

let interpret (p : sourcespan program) : sourcespan value =
  let (e, tag) = p in
  let helpE e =
    match e with
      | EInt(num, tag) -> VInt(num, tag)
      | EBool(b, tag) -> VBool(b, tag)
  in
  helpE e

let untag_value v =
  match v with
    | VInt(num, tag) -> VInt(num, ())
    | VBool(b, tag) -> VBool(b, ())

let string_of_value v =
  match v with
    | VInt(num, _) -> Int64.to_string num
    | VBool(b, _) -> Bool.to_string b