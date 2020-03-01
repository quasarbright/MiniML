type 'a typ =
  | TyTop of 'a
  | TyBottom of 'a
  | TyVar of string * 'a
  | TyInt of 'a
  | TyBool of 'a

let map_type_tag f t =
  match t with
    | TyTop(tag) -> TyTop(f tag)
    | TyBottom(tag) -> TyBottom(f tag)
    | TyVar(name, tag) -> TyVar(name, f tag)
    | TyInt(tag) -> TyInt(f tag)
    | TyBool(tag) -> TyBool(f tag)

let untag_type t = map_type_tag ignore t