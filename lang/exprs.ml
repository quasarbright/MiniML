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
  | EId of string * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | ELet of 'a bind * 'a expr * 'a
and 'a bind = string * 'a expr * 'a

type 'a program = 'a expr * 'a

let untag (p : 'a program) =
  let rec helpE e =
    match e with
      | EInt(num, _) -> EInt(num, ())
      | EBool(b, _) -> EBool(b, ())
      | EId(name, _) -> EId(name, ())
      | EPrim1(prim1, arg_expr, _) -> EPrim1(prim1, helpE arg_expr, ())
      | EPrim2(prim2, left_expr, right_expr, _) -> EPrim2(prim2, helpE left_expr, helpE right_expr, ())
      | EIf(cnd,thn,els,_) -> EIf(helpE cnd,helpE thn,helpE els,())
      | ELet((name, val_expr, _), body_expr, _) -> ELet((name, helpE val_expr, ()), helpE body_expr, ())
  in
  let e, tag = p in
    (helpE e, ())


let get_tag (e : 'a expr) =
  match e with
    | EInt(_, tag) -> tag
    | EBool(_, tag) -> tag
    | EId(_, tag) -> tag
    | EPrim1(_, _, tag) -> tag
    | EPrim2(_,_,_,tag) -> tag
    | EIf(_,_,_,tag) -> tag
    | ELet(_,_,tag) -> tag