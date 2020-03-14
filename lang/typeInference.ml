open Printf
open Types
open Exprs
open Pretty
open Errors
open Unify

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


