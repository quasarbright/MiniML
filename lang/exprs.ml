type sourcespan = Lexing.position * Lexing.position

type prim1 =
  | Not
  | UNegate

type prim2 =
  | Plus
  | Minus
  | Times
  | Modulo
  | Divide
  | And
  | Or
  | Eq
  | Neq
  | Less
  | LessEq
  | Greater
  | GreaterEq

type 'a expr =
  | EInt of int64 * 'a
  | EBool of bool * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a

type 'a program = 'a expr * 'a

let untag (p : 'a program) =
  let rec helpE e =
    match e with
      | EInt(num, _) -> EInt(num, ())
      | EBool(b, _) -> EBool(b, ())
      | EPrim1(prim1, arg_expr, _) -> EPrim1(prim1, helpE arg_expr, ())
      | EPrim2(prim2, left_expr, right_expr, _) -> EPrim2(prim2, helpE left_expr, helpE right_expr, ())
  in
  let e, tag = p in
    (helpE e, ())