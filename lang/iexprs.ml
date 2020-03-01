open Exprs

type 'a iexpr =
  | IInt of int64 * 'a
  | IBool of bool * 'a
  | IId of string * 'a
  | IApp of 'a iexpr * 'a iexpr * 'a

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
      | EPrim1(prim1, arg_expr, tag) ->
          IApp(helpP1 prim1 tag, helpE arg_expr, tag)
      | EPrim2(prim2, left_expr, right_expr, tag) ->
          IApp(IApp(helpP2 prim2 tag, helpE left_expr, tag), helpE right_expr, tag)
  in
  let (e, tag) = p in
  (helpE e, tag)