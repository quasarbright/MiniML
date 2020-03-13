type 'a typ =
  | TyTop of 'a
  | TyBottom of 'a
  | TyVar of string * 'a
  | TyCons of string * 'a typ list * 'a

let arrow_name = "arrow"
let arrow t1 t2 tag = TyCons(arrow_name, [t2;t2], tag)
let cross_name = "cross"
let cross ts tag = TyCons(cross_name, ts, tag)
let int_name = "int"
let int tag = TyCons(int_name, [], tag)
let bool_name = "bool"
let bool tag = TyCons(bool_name, [], tag)
let list_name = "list"
let list t tag = TyCons(list_name, [t], tag)

let rec map_type_tag f t =
  match t with
    | TyTop(tag) -> TyTop(f tag)
    | TyBottom(tag) -> TyBottom(f tag)
    | TyVar(name, tag) -> TyVar(name, f tag) (* name will have ' in it *)
    | TyCons(name, typs, tag) -> TyCons(name, List.map (map_type_tag f) typs, f tag)

let untag_type t = map_type_tag ignore t

let tyvars_in t =
  let rec aux t acc =
    match t with
    | TyVar(name, tag) -> name::acc
    | TyCons(name, typs, tag) -> List.fold_right aux typs acc
    | TyBottom(_)
    | TyTop(_) -> acc
  in
  aux t []

let check_type_cons name t =
  match t with
  | TyCons(actual_name, _, _) -> actual_name = name
  | _ -> false

let same_type t1 t2 =
  match (t1, t2) with
  | (TyTop(_) | TyBottom(_)), _
  | _, (TyTop(_) | TyBottom(_)) -> true
  | _, _ -> (untag_type t1) = (untag_type t2)