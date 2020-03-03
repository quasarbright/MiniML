open Exprs
open Pretty
open Errors
open Values
open Builtins
open InterpreterUtils


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
  let rec helpE (env : 'a value envt) e =
    let failure_bind env = interpreter_failure_handler (helpE env) (fun exn exprs tag -> VErr(exn, exprs, tag)) in
    (* abstracts handling failure while evaluating 
    NOT ACTUALLY A MONAD *)
    let (>>=) = failure_bind env in
    match e with
      | EInt(num, tag) -> VInt(num, tag)
      | EBool(b, tag) -> VBool(b, tag)
      | EId(name, tag) ->
          begin
            match find_opt env name with
              | None -> VErr(UnboundId(name, tag), [e], tag)
              | Some(v) -> v 
          end
      | EPrim1(prim1, arg_expr, tag) ->
          arg_expr >>= fn_of_prim1 prim1 tag
          |> add_to_trace_if_err e
      | EPrim2(prim2, left_expr, right_expr, tag) ->
          left_expr >>= (fun left ->
          right_expr >>= (fun right ->
          fn_of_prim2 prim2 tag left right))
          |> add_to_trace_if_err e
      | EIf(cnd, thn, els, tag) ->
          cnd >>= (fun cnd -> Builtins.(>>=)
            (cnd, TyBool(ds))
            (fun v ->
              match v with
                | VBool(b, _) ->
                    if b
                    then helpE env thn
                    else helpE env els
                | _ -> failwith "unexpected type error in EIf interpreting"))
      | ELet((names, val_expr, bind_tag), body_expr, tag) ->
          match names with
          | [(name, _)] ->
              val_expr >>=
              (fun value ->
                let env' = (name, value)::env in
                let (>>=) = failure_bind env' in
                body_expr >>= (fun body -> body)
              )
          | (name, _)::(arg_name::arg_names) ->
              let arg_name' = fst arg_name in
              let arg_names' = List.map fst arg_names in
              let func_value = VFunc(Func(Some(name), arg_name', helpF arg_names' val_expr env tag, env, tag)) in
              let env' = (name, func_value)::env in
              let (>>=) = failure_bind env' in
              body_expr >>= (fun body -> body)
          | [] -> raise (InternalError("encountered elet with no binds while interpreting"))
  and helpF arg_names val_expr env tag =
    match arg_names with
      | [] -> Right(val_expr)
      | arg_name::arg_names' -> Left(Func(None, arg_name, helpF arg_names val_expr env tag, env, tag))
  in
  let (e, tag) = p in
  helpE [] e