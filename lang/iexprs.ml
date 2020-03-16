open Exprs
open Errors

type 'a iexpr =
  | IInt of int64 * 'a
  | IBool of bool * 'a
  | IId of string * 'a
  | IApp of 'a iexpr * 'a iexpr * 'a
  | ILambda of
      string option (* name if any *)
      * (string * 'a) (* name bind *)
      * 'a iexpr (* body *)
      * 'a (* tag *)
  | ILet of
      (string * 'a) (* name *)
      * 'a iexpr (* value *)
      * 'a iexpr (* body *)
      * 'a (* tag *)
  | ILetRec of
      ((string * 'a) * 'a iexpr) list (* bindings *)
      * 'a iexpr (* body *)
      * 'a (* tag *)
type 'a iprogram = 'a iexpr * 'a

let iprogram_of_program (p : 'a program) : 'a iprogram =
  let rec helpP1 p1 tag =
    p1 |> Pretty.string_of_prim1 |> (fun name -> IId(name, tag))
  in
  let rec helpP2 p2 tag =
    p2 |> Pretty.string_of_prim2 |> (fun name -> IId(name, tag))
  in
  let rec helpE e =
    match e with
      | EInt(num, tag) -> IInt(num, tag)
      | EBool(b, tag) -> IBool(b, tag)
      | EId(name, tag) -> IId(name, tag)
      | EPrim1(prim1, arg_expr, tag) ->
          IApp(helpP1 prim1 tag, helpE arg_expr, tag)
      | EPrim2(prim2, left_expr, right_expr, tag) ->
          IApp(IApp(helpP2 prim2 tag, helpE left_expr, tag), helpE right_expr, tag)
      | ELet(([], _, _), body, tag) -> raise (InternalError("tried to desugar a let with no binds (iexprs)"))
      | ELet((((name, name_tag) as main_bind)::arg_binds, val_expr, binding_tag), body, tag) ->
          ILet(main_bind, helpLambda (Some(name)) name_tag arg_binds val_expr, helpE body, tag)
      | EIf(cnd, thn, els, tag) ->
          let ifid = IId("if", tag) in
          IApp(IApp(ifid, helpE thn, tag), helpE els, tag)
      | EApp(e1, e2, tag) -> IApp(helpE e1, helpE e2, tag)
  and helpLambda name_opt tag arg_binds val_expr =
    match arg_binds with
    | [] -> helpE val_expr
    | arg_bind::arg_binds -> ILambda(name_opt, arg_bind, helpLambda name_opt tag arg_binds val_expr, tag)
  in
  let (e, tag) = p in
  (helpE e, tag)