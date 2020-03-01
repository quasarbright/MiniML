open Exprs
open Pretty
open Errors
open Values
open Builtins


let interpreter_failure_handler (evaluator : 'a expr -> 'a value) (on_err : exn -> 'a expr list -> 'a -> 'a value) (e : 'a expr) (on_success : 'a value -> 'b) =
  let value = evaluator e in
  match value with
    | VErr(exn, exprs, tag) -> on_err exn exprs tag
    | _ -> on_success value

let add_to_trace_if_err e v =
  match v with
    | VErr(exn, exprs, tag) -> VErr(exn, e::exprs, tag)
    | _ -> v

let interpret (p : sourcespan program) : sourcespan value =
  let rec helpE e =
    (* abstracts handling failure while evaluating *)
    let (>>=) = interpreter_failure_handler helpE (fun exn exprs tag -> VErr(exn, e::exprs, tag)) in
    match e with
      | EInt(num, tag) -> VInt(num, tag)
      | EBool(b, tag) -> VBool(b, tag)
      | EPrim1(prim1, arg_expr, tag) ->
          arg_expr >>= fn_of_prim1 prim1 tag
          |> add_to_trace_if_err e
      | EPrim2(prim2, left_expr, right_expr, tag) ->
          left_expr >>= (fun left ->
          right_expr >>= (fun right ->
          fn_of_prim2 prim2 tag left right))
          |> add_to_trace_if_err e
  in
  let (e, tag) = p in
  helpE e