open Exprs
open Values
open Errors

let type_err_behavior tag = VErr(Failure("type error"), [], tag)

let wrap_int_binop binop tag left right =
  match left, right with
    | VInt(left_num, _), VInt(right_num, _) -> VInt(binop left_num right_num, tag)
    | _ -> type_err_behavior tag

let wrap_cmp binop tag left right =
  match left, right with
    | VInt(l, _), VInt(r, _) -> VBool(l < r, tag)
    | VBool(l, _), VBool(r, _) -> VBool(l < r, tag)
    | VInt(_), VBool(_) | VBool(_), VInt _ -> type_err_behavior tag
    | VErr(_), _ | _, VErr(_) -> failwith "unexpected error value in fn_of_prim2"


let wrap_bool_binop binop tag left right =
  match left, right with
    | VBool(left_b, _), VBool(right_b, _) -> VBool(binop left_b right_b, tag)
    | _ -> type_err_behavior tag

let fn_of_prim2 = function
  | Plus -> wrap_int_binop Int64.add
  | Minus -> wrap_int_binop Int64.sub
  | Times -> wrap_int_binop Int64.mul
  | Modulo ->
      (fun tag left right ->
        match left, right with
          | VInt(left_num, _), VInt(0L, _) -> VErr(DivideByZero(tag), [], tag)
          | VInt(left_num, _), VInt(right_num, _) -> VInt(Int64.rem left_num right_num, tag)
          | _ -> type_err_behavior tag)
  | Divide ->
      (fun tag left right ->
        match left, right with
          | VInt(left_num, _), VInt(0L, _) -> VErr(DivideByZero(tag), [], tag)
          | VInt(left_num, _), VInt(right_num, _) -> VInt(Int64.div left_num right_num, tag)
          | _ -> type_err_behavior tag)
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
  