open Types
open Errors

(*
unification algorithm from section 9.4 of "The Implementation of Functional Programming Languages"
by Simon L. Peyton Jones
*)

(** mapping from type variable names to concrete types. *)
type 'a subst = 
    string (* type variable name *)
    -> 'a (* tag for constructing tyvars *)
    -> 'a typ (* (non-variable) type *)

(** actually does a substitution *)
let rec perform_subst phi typ =
  match typ with
  | TyVar(name, tag) -> phi name tag
  | TyCons(name, typs, tag) -> TyCons(name, List.map (perform_subst phi) typs, tag)
  | TyTop _
  | TyBottom _ -> typ

(** applies the second substitution, then the first *)
let compose_substs (s2 : 'a subst) (s1 : 'a subst) : 'a subst = fun tvn tag -> perform_subst s2 (s1 tvn tag)

(** identity function for ty vars
represents the equation 'a = 'a *)
let id_subst name tag = TyVar(name, tag) (* this is the whole reason subst has a tag. ignored everywhere else *)

(** represents equation 'a = t *)
let delta name (t : 'a typ) : 'a subst = fun tvn tag -> if tvn = name then t else id_subst tvn tag


(**
adds the equation TyVar tvn = t to the system of equations phi
just adds a delta unless there's a circular reference or that delta is already in phi
circular reference would be like 'a = 'a -> int
*)
let extend phi tvn t =
  let normal() = Ok(compose_substs (delta tvn t) phi) in
  match t with
  | TyVar(name, tag) ->
      if name = tvn
      then Ok(phi) (* don't bother extending, it's redundant *)
      else normal()
  | TyBottom(tag)
  | TyTop(tag)
  | TyCons(_, _, tag) ->
      if List.mem tvn (tyvars_in t)
      then Error(RecursiveType(tvn, t, tag))
      else normal()

let rec zip l1 l2 =
  match (l1, l2) with
  | [], _
  | _, [] -> []
  | x::xs, y::ys -> (x, y)::zip xs ys

(** adds the equation t1 = t2 to the system of equations in phi *)
let rec unify phi (t1, t2) (* reasons *) =
  match (t1, t2) with
  | ((TyVar(name, tag) as tyvar), t)
  | (t, (TyVar(name, tag) as tyvar)) ->
      let phi_of_t = perform_subst phi t in
      let phi_of_name = phi name tag in
      if same_type phi_of_name tyvar (* if name is unmoved by phi *)
      then extend phi name t (* just add a delta *)
      else unify phi (phi_of_name, phi_of_t) (* name's real type is the same as phi of t *)
           (* need to do this because name is moved by phi. You have to get rid of name first *)
  | (TyCons(name1, typs1, tag1), TyCons(name2, typs2, tag2)) ->
      if name1 <> name2
      then (Error(TypeMismatch(t1, t2, tag1))) (* TODO using tag1 is suboptimal *)
      else if List.length typs1 <> List.length typs2
      then (Error(InternalError("tried to unify two types with the same constructor, but different arities?")))
      else unifyl phi (zip typs1 typs2)
  | (TyTop _ | TyBottom _), _
  | _, (TyBottom _ | TyTop _) -> failwith "idk how to handle top and bottom here"
and unifyl phi eqns =
  let aux eqn phi_res =
    Result.bind phi_res (fun
    phi -> unify phi eqn)
  in
  List.fold_right aux eqns (Ok(phi))