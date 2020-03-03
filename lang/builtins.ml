open Exprs
open Values
open Errors
open Types

let ds : sourcespan = Lexing.dummy_pos, Lexing.dummy_pos

let type_err_behavior tag = VErr(Failure("type error"), [], tag)

let type_mismatch tag expected_type v =
  TypeMismatch(expected_type, type_of_value v, tag)

let assert_typ v expected_type on_err on_ok =
  let tag = tag_of_value v in
  let actual_type = type_of_value v in
  if untag_type actual_type = untag_type expected_type
  then on_ok v
  else on_err (TypeMismatch(expected_type, actual_type, tag))


(* NOT ACTUALLY A MONAD *)
let (>>=) (v, expected_type) on_ok =
  let tag = tag_of_value v in
  assert_typ v expected_type
    (fun exn -> VErr(exn, [], tag))
    on_ok

let wrap_int_binop binop tag left right =
    (left, TyInt(ds)) >>= (fun _ ->
    (right, TyInt(ds)) >>= (fun _ ->
        match left, right with
    | VInt(left_num, _), VInt(right_num, _) -> VInt(binop left_num right_num, tag)
    | _ -> raise (InternalError("unexpected type error in wrap_int_binop"))))

let wrap_cmp binop tag left right =

  match left, right with
    | VInt(l, _), VInt(r, _) -> VBool(l < r, tag)
    | VBool(l, _), VBool(r, _) -> VBool(l < r, tag)
    | VInt(_), VBool(_) | VBool(_), VInt _ -> VErr(type_mismatch tag (type_of_value left) right, [], tag)
    | VErr(_), _ | _, VErr(_) -> raise (InternalError("unexpected type error in wrap_cmp"))
    | VFunc(_), _ | _, VFunc(_) -> VErr(ArgumentError("cannot compare function values", tag), [], tag)


let wrap_bool_binop binop tag left right =
  (left, TyBool(ds)) >>= (fun _ ->
  (right, TyBool(ds)) >>= (fun _ ->
    match left, right with
    | VBool(left_b, _), VBool(right_b, _) -> VBool(binop left_b right_b, tag)
    | _ -> raise (InternalError("unexpected type error in wrap_bool_binop"))))

let fn_of_prim2 = function
  | Plus -> wrap_int_binop Int64.add
  | Minus -> wrap_int_binop Int64.sub
  | Times -> wrap_int_binop Int64.mul
  | Modulo ->
      (fun tag left right ->
        (left, TyInt(ds)) >>= (fun _ -> 
        (right, TyInt(ds)) >>= (fun _ ->
          match left, right with
            | VInt(left_num, _), VInt(0L, _) -> VErr(DivideByZero(tag), [], tag)
            | VInt(left_num, _), VInt(right_num, _) -> VInt(Int64.rem left_num right_num, tag)
            | _ -> raise (InternalError("unexpected type error in modulo")))
        ))
  | Divide ->
      (fun tag left right ->
        (left, TyInt(ds)) >>= (fun _ -> 
        (right, TyInt(ds)) >>= (fun _ ->
          match left, right with
            | VInt(left_num, _), VInt(0L, _) -> VErr(DivideByZero(tag), [], tag)
            | VInt(left_num, _), VInt(right_num, _) -> VInt(Int64.div left_num right_num, tag)
            | _ -> raise (InternalError("unexpected type error in divide")))
        ))
  | Less -> wrap_cmp (<)
  | LessEq -> wrap_cmp (<=)
  | Greater -> wrap_cmp (>)
  | GreaterEq -> wrap_cmp (>=)
  | Eq -> wrap_cmp (=)
  | Neq -> wrap_cmp (<>)
  | And -> wrap_bool_binop (&&)
  | Or -> wrap_bool_binop (||)

let fn_of_prim1 = function
  | Not -> 
      (fun tag arg ->
        match arg with
          | VBool(b, _) -> VBool(not b, tag)
          | _ -> type_err_behavior tag)
  | UNegate ->
      (fun tag arg ->
        match arg with
          | VInt(n, _) -> VInt(Int64.neg n, tag)
          | _ -> type_err_behavior tag)
  