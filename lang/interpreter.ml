open Exprs
open Pretty
open Errors
open Values
open Builtins
open InterpreterUtils
open Phases

let get_child_exprs e =
  match e with
    | EInt(_)
    | EBool(_)
    | EId(_) -> []
    | EPrim1(_, arg, _) -> [arg]
    | EPrim2(_,left, right, _) -> [left;right]
    | EIf(cnd, thn, els, _) -> [cnd;thn;els]
    | ELet((_, value, _), body, _) ->
        [value;body]
    | EApp(func, arg, _) -> [func;arg]

let detect_dup_names tagged_names =
  let rec aux tagged_names seen errs =
    match tagged_names with
      | [] -> errs
      | (name, tag)::rest_tagged_names ->
          let maybe_original =
            seen |> List.find_opt (fun (original_name, original_tag) -> name = original_name)
          in
          let new_errs, new_seen = match maybe_original with
            | None -> errs, (name, tag)::seen
            | Some(original_name, original_tag) -> (DuplicateId(name, tag, original_tag)::errs), seen
          in
          aux rest_tagged_names new_seen new_errs
  in
  aux tagged_names [] []

let check_well_formedness p : sourcespan program fallible =
  let rec helpE env e =
    match e with
      | EInt(_)
      | EBool(_)
      | EPrim1(_)
      | EPrim2(_)
      | EIf(_)
      | EApp(_) -> get_child_exprs e |> List.map @@ helpE env |> List.concat
      | EId(name, tag) ->
          if List.mem name env
          then []
          else [UnboundId(name, tag)]
      | ELet((tagged_names, val_expr, bind_tag), body_expr, tag) ->
          let main_name, dup_errs, val_env = match tagged_names with
            | (name, name_tag)::[] -> name, [], env
            | (name, name_tag)::rest ->
                  name, detect_dup_names rest, List.map fst rest @ env
            | [] -> raise (InternalError("encountered let with empty bind in well formedness check"))
          in
          let val_errs = helpE val_env val_expr in
          let env' = main_name::env in
          let body_errs = helpE env' body_expr in
          dup_errs @ val_errs @ body_errs
  in
  let (e, tag) = p in
  match helpE [] e with
    | [] -> Ok(p)
    | _::_ as errs -> Error(errs)

let interpreter_failure_handler (evaluator : 'a expr -> 'a value) (on_err : exn -> 'a expr list -> 'a -> 'a value) (e : 'a expr) (on_success : 'a value -> 'b) =
  let value = evaluator e in
  match value with
    | VErr(exn, exprs, tag) -> on_err exn exprs tag
    | _ -> on_success value

let debug = false
let log s = if debug then print_string s else ()

let add_to_trace_if_err e v =
  log @@ Printf.sprintf "evaluated %s:\n\t%s\n\n" (string_of_expr e) (string_of_value v);
  match v with
    | VErr(exn, exprs, tag) -> VErr(exn, e::exprs, tag)
    | _ -> v

(** assumes well-formedness *)
let evaluate (p : sourcespan program) : sourcespan value =
  let rec helpE (env : 'a value envt) e =
    log @@ Printf.sprintf "evaluating %s\nwith environment\n%s\n\n" (string_of_expr e)
      (env
      |> List.map
         (fun (name, value) -> Printf.sprintf "\t%s := %s" name (string_of_value value))
      |> String.concat "\n");
    let failure_bind env = interpreter_failure_handler (helpE env) (fun exn exprs tag -> VErr(exn, exprs, tag)) in
    (* abstracts handling failure while evaluating 
    NOT ACTUALLY A MONAD *)
    let (>>=) = failure_bind env in
    match e with
      | EInt(num, tag) -> VInt(num, tag) |> add_to_trace_if_err e
      | EBool(b, tag) -> VBool(b, tag) |> add_to_trace_if_err e
      | EId(name, tag) ->
          begin
            match find_opt env name with
              | None -> VErr(UnboundId(name, tag), [e], tag)
              | Some(v) -> v |> add_to_trace_if_err e
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
                    then thn >>= (fun thn -> thn)
                    else els >>= (fun els -> els)
                | _ -> failwith "unexpected type error in EIf interpreting"))
          |> add_to_trace_if_err e
      | ELet((names, val_expr, bind_tag), body_expr, tag) ->
          begin
            match names with
            | [(name, _)] ->
                val_expr >>=
                (fun value ->
                  let env' = (name, value)::env in
                  let (>>=) = failure_bind env' in
                  body_expr >>= (fun body -> body)
                )
                |> add_to_trace_if_err e
            | (name, _)::(arg_name::arg_names) ->
                let arg_name' = fst arg_name in
                let arg_names' = List.map fst arg_names in
                let func_value = VFunc(Func(Some(name), arg_name', helpF arg_names' val_expr env tag, env, tag)) in
                let env' = (name, func_value)::env in
                let (>>=) = failure_bind env' in
                body_expr >>= (fun body -> body)
                |> add_to_trace_if_err e
            | [] -> raise (InternalError("encountered elet with no binds while interpreting"))
          end
      | EApp(func_expr, arg_expr, tag) ->
          func_expr >>= (fun func_value ->
          match func_value with
            | VFunc(Func(maybe_name, arg_name, body, env, tag)) ->
                arg_expr >>= (fun arg_value -> 
                let arg_pair = (arg_name, arg_value) in
                match body with
                  | Left(Func(maybe_name', arg_name', body', _, tag')) ->
                      VFunc(Func(maybe_name', arg_name', body', arg_pair::env, tag'))
                      (* I thought this should be env', but really, this only happens within a multi argument function.
                      otherwise, body is an expr which may be another function or a straight value. So it must inherit env *)
                  | Right(body_expr) ->
                      let env' = arg_pair::env in
                      let (>>=) = failure_bind env' in
                      body_expr >>= (fun body -> body)
                )
            | _ -> VErr(Failure("attempted to apply non-function"), [], tag)
          )
          |> add_to_trace_if_err e
  and helpF arg_names val_expr env tag =
    match arg_names with
      | [] -> Right(val_expr)
      | arg_name::arg_names' -> Left(Func(None, arg_name, helpF arg_names' val_expr env tag, env, tag))
  in
  let (e, tag) = p in
  helpE [] e

let interpret_program (prog : sourcespan program pipeline) =
  prog
  |> (add_err_phase well_formed check_well_formedness)
  |> (add_phase evaluated evaluate)
  (* match check_well_formedness p with
    | Error(errs) -> Error(errs)
    | Ok(p) -> Ok(evaluate p) *)