open Exprs
open Pretty
open Errors
open Values


let interpreter_failure_handler (evaluator : 'a expr -> 'a value) (on_err : exn -> 'a expr list -> 'a -> 'a value) (e : 'a expr) (on_success : 'a value -> 'b) =
  let value = evaluator e in
  match value with
    | VErr(exn, exprs, tag) -> on_err exn exprs tag
    | _ -> on_success value

let add_to_trace e err =
  match err with
    | VErr(exn, exprs, tag) -> VErr(exn, e::exprs, tag)
    | _ -> failwith "add_to_trace expects error values only"

let interpret (p : sourcespan program) : sourcespan value =
  let rec helpE e =
    (* abstracts handling failure while evaluating *)
    let (>>=) = interpreter_failure_handler helpE (fun exn exprs tag -> VErr(exn, e::exprs, tag)) in
    match e with
      | EInt(num, tag) -> VInt(num, tag)
      | EBool(b, tag) -> VBool(b, tag)
      | EPrim2(Plus, left_expr, right_expr, tag) ->
          left_expr >>= (fun left ->
          right_expr >>= (fun right -> 
            match left, right with
              | VInt(left_num, _), VInt(right_num, _) -> VInt(Int64.add left_num right_num, tag)
              | _ -> VErr(Failure("type error"), [e], tag)
          )) 
      | EPrim2(Or, left_expr, right_expr, tag) ->
          left_expr >>= (fun left ->
          right_expr >>= (fun right ->
            match left, right with
              | VBool(left_b, _), VBool(right_b, _) -> VBool(left_b || right_b, tag)
              | _ -> VErr(Failure("type error"), [e], tag)
          ))
      | EPrim1(_,_,tag) | EPrim2(_,_,_,tag) -> VErr(Failure("not yet implemented"), [e], tag)
  in
  let (e, tag) = p in
  helpE e