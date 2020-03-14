open Printf
open Types
open Exprs
open Pretty
open Errors
open Unify
(* TODO make sure you gensym schematic and unknown variables differently!!!!!!! *)
(* gives everything a unique tyVar type *)
let rec tag_expr_with_typs e =
  let count = ref 0 in
  let get_next_count() =
    let ans = !count in
    count := ans + 1;
    ans
  in
  let gen_sym() = sprintf "'t_%d" (get_next_count()) in
  let make_var tag = var (gen_sym()) tag in
  map_expr_tag (fun tag -> make_var tag, tag) e


let unknowns_in_scheme (SForall(names, typ)) =
  let set_difference l1 l2 =
    List.filter (fun x -> not @@ List.mem x l2) l1
  in
  set_difference (tyvars_in typ) names

(** exclude the names from the substitution phi *)
let exclude phi names : 'a subst = (fun name tag -> if List.mem name names then id_subst name tag else phi name tag)

(** perform a substitution in a scheme (don't substitute schematic (bound) variables, only unknown (free) ones) *)
let subst_scheme phi (SForall(names, typ)) = SForall(names, perform_subst (exclude phi names) typ)

