open Printf
open Seq
open Types
open Exprs
open Pretty
open Errors
open Unify

type 'a seq = 'a Seq.t
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
let perform_subst_on_scheme (phi : 'a subst) (SForall(names, typ)) = SForall(names, perform_subst (exclude phi names) typ)

type 'a typ_env = (string * 'a scheme) list

let rec remove_dups xs =
  match xs with
  | [] -> []
  | x::xs when List.mem x xs -> remove_dups xs
  | x::xs -> x::remove_dups xs

let range assoc = remove_dups (List.map snd assoc)

(** finds all the unkown type variables in the environment *)
let unkowns_in_env (gamma : 'a typ_env) = List.concat (List.map unknowns_in_scheme (range gamma))

let perform_subst_on_te (phi : 'a subst) (gamma : 'a typ_env) : 'a typ_env = List.map (fun (name, scheme) -> (name, perform_subst_on_scheme phi scheme)) gamma


type name_supply = (int seq) (* infinite sequence of unique numbers *)
                 * (int -> string) (* make varname from number *)

let next_name ((iseq, render_tag) : name_supply) : string =
  match iseq() with
  | Cons(num, iseq) -> render_tag num
  | Nil -> failwith "tried to get next name of an empty sequence"

let deplete ((iseq, render_tag) : name_supply) : name_supply =
  match iseq() with
  | Cons(_, iseq) -> (iseq, render_tag)
  | Nil -> (empty, render_tag)

let split ((iseq, render_tag) : name_supply) : name_supply * name_supply =
  (Seq.map (fun num -> 2 * num + 1) iseq, render_tag),
  (Seq.map (fun num -> 2 * num) iseq, render_tag)

let rec name_sequence (ns : name_supply) : string seq = fun () -> Seq.Cons((next_name ns), name_sequence (deplete ns))

(**
gamma associates free vars in expr with type schemes
  should be initialized with builtin identifier types
ns is a supply of variable names
expr is the expression to be checked

returns Ok(phi, t) such that
  phi is a substitution on the unkown type variables in gamma
  t is the type of expr in the environment (perform_subst_on_te phi gamma). It's a fixed point of phi
  if typeCheck gamma name_supply expr is Ok(phi, t)
    then e:t can be derived from gamma provided that the unkowns in gamma have the values given by phi
 *)
let rec typeCheck (gamma : 'a typ_env) (name_supply : name_supply) (expr : 'a expr) : (('a subst * 'a typ), exn) result =
  match expr with
  | EInt(num, tag) -> Error(Failure("not implemented"))
  | EBool(b, tag) -> Error(Failure("not implemented"))
  | EId(name, tag) -> Error(Failure("not implemented"))
  | EIf(cnd, thn, els, tag) -> Error(Failure("not implemented"))
  | ELet(bind, body, tag) -> Error(Failure("not implemented"))
  | EApp(e1, e2, tag) -> Error(Failure("not implemented"))
  | EPrim1(prim1, arg, tag) -> Error(Failure("not implemented"))
  | EPrim2(prim2, left, right, tag) -> Error(Failure("not implemented"))
(** type check a list of expressions cummulatively *)
and tcl gamma ns es =
  match es with
  | [] -> Ok(id_subst, [])
  | e::es ->
      let ns0, ns1 = split ns in
      tcl1 gamma ns0 es (typeCheck gamma ns1 e)
and tcl1 gamma ns es res =
  Result.bind res (fun
  (phi, t) ->
  let new_gamma = perform_subst_on_te phi gamma in
  tcl2 phi t (tcl new_gamma ns es)
  )
and tcl2 phi t res =
  Result.bind res (fun
  (psi, ts) ->
  Ok(compose_substs psi phi, (perform_subst psi t::ts))
  )
