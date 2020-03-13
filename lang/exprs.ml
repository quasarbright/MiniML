type sourcespan = Lexing.position * Lexing.position
let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos)

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
  | EApp of 'a expr * 'a expr * 'a
and 'a bind = (string * 'a) list * 'a expr * 'a

type 'a program = 'a expr * 'a


let rec map_expr_tag f e =
  let helpB f (name, tag) = (name, f tag) in
  match e with
    | EInt(num, tag) -> EInt(num, f tag)
    | EBool(b, tag) -> EBool(b, f tag)
    | EId(name, tag) -> EId(name, f tag)
    | EPrim1(prim1, arg_expr, tag) -> EPrim1(prim1, map_expr_tag f arg_expr, f tag)
    | EPrim2(prim2, left_expr, right_expr, tag) -> EPrim2(prim2, map_expr_tag f left_expr, map_expr_tag f right_expr, f tag)
    | EIf(cnd,thn,els,tag) -> EIf(map_expr_tag f cnd,map_expr_tag f thn,map_expr_tag f els, f tag)
    | ELet((names, val_expr, bind_tag), body_expr, tag) -> ELet((List.map (helpB f) names, map_expr_tag f val_expr, f bind_tag), map_expr_tag f body_expr, f tag)
    | EApp(func_expr, arg_expr, tag) -> EApp(map_expr_tag f func_expr, map_expr_tag f arg_expr, f tag)

let map_program_tag f (p : 'a program) =
  let e, tag = p in
    (map_expr_tag f e, f tag)

let untag (p : 'a program) : unit program = map_program_tag ignore p


let get_tag (e : 'a expr) =
  match e with
    | EInt(_, tag) -> tag
    | EBool(_, tag) -> tag
    | EId(_, tag) -> tag
    | EPrim1(_, _, tag) -> tag
    | EPrim2(_,_,_,tag) -> tag
    | EIf(_,_,_,tag) -> tag
    | ELet(_,_,tag) -> tag
    | EApp(_,_,tag) -> tag