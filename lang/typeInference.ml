open Printf
open Seq
open Types
open Iexprs
open Pretty
open Errors
open Unify

type 'a seq = 'a Seq.t

let rec zip_seq xs ys =
  match (xs(), ys()) with
  | (Nil, _)
  | (_, Nil) -> empty
  | (Cons(x, xs), Cons(y, ys)) -> fun () -> (Cons((x, y), zip_seq xs ys))

let special_zip xs ys =
  List.of_seq @@ zip_seq (List.to_seq xs) ys

(* TODO make sure you gensym schematic and unknown variables differently!!!!!!! *)
(* gives everything a unique tyVar type *)
(* let rec tag_expr_with_typs e =
  let count = ref 0 in
  let get_next_count() =
    let ans = !count in
    count := ans + 1;
    ans
  in
  let gen_sym() = sprintf "'t_%d" (get_next_count()) in
  let make_var tag = var (gen_sym()) tag in
  map_expr_tag (fun tag -> make_var tag, tag) e *)


let set_difference l1 l2 =
  List.filter (fun x -> not @@ List.mem x l2) l1

let unknowns_in_scheme (SForall(names, typ)) =
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
used when trying to instantiate a scheme with fresh unkowns given by a name supply
you'd zip the scheme vars with some of the name supply and then you'd make an assoc
list out of it (scheme var -> unkown var)

could be optimized to take in and return a name supply
rather than an association list
*)
let al_to_subst name_to_name name tag =
  if List.mem_assoc name name_to_name
  then TyVar(List.assoc name name_to_name, tag)
  else TyVar(name, tag)

(**
instantiate a type scheme with fresh unkowns
*)
let new_instance name_supply (SForall(scheme_type_vars, scheme_type)) =
  let al = special_zip scheme_type_vars (name_sequence name_supply) in
  let phi =  al_to_subst al in
  perform_subst phi scheme_type

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
let rec typeCheck (gamma : 'a typ_env) (name_supply : name_supply) (expr : 'a iexpr) : (('a subst * 'a typ), exn) result =
  (* composes the substitution for the body with the substitution for the names (does names sub first) *)
  let tclet2 vals_phi res =
    Result.bind res (fun
    (body_phi, body_t) -> Ok(compose_substs body_phi vals_phi, body_t)
    )
  in
  let genbar unkowns ns t =
    let scvs = set_difference (remove_dups (tyvars_in t)) unkowns in
    let al = special_zip scvs (name_sequence ns) in
    let new_t = perform_subst (al_to_subst al) t in
    SForall(List.map snd al, new_t)
  in
  (* generates associations from bound names to schematic types *)
  let add_decls gamma name_supply binds ts : 'a typ_env =
    let unkowns = unkowns_in_env gamma in
    let schemes = List.map (genbar unkowns name_supply) ts in
    zip binds schemes
  in
  match expr with
  | IInt(num, tag) -> Ok((id_subst, int tag))
  | IBool(b, tag) -> Ok((id_subst, bool tag))
  | IId(name, tag) ->
      let scheme = List.assoc name gamma in
      Ok(id_subst, new_instance name_supply scheme)
  | IApp(e1, e2, tag) ->
      (*
      applies the substitution (if any) to the name
      returning the substitution and the name's corresponding type
      *)
      let tcap2 name tag sub_res =
        Result.bind sub_res (fun
        (phi : 'a subst) -> Ok(phi, phi name tag)
        )
      in
      (*
      takes the result of type checking e1 and e2 and
      asserts t1 = t2 -> 't
      where 't is name
      *)
      let tcap1 name tag maybe_t1_t2 =
        Result.bind maybe_t1_t2 (fun
        (phi, t1_t2) ->
        match t1_t2 with
        | [t1;t2] -> tcap2 name tag (unify phi (t1, arrow t2 (TyVar(name, tag)) tag))
        | _ -> raise (InternalError("expected there to be a list of two types in tcap1"))
        )
      in
      let name = next_name name_supply in
      let new_name_supply = deplete name_supply in
      tcap1 name tag (tcl gamma new_name_supply [e1;e2])
    | ILet((name, name_tag), val_expr, body, tag) ->
        (*
        takes the subst and types after evaluating the right-hand values (vals_phi, ts),
        updates gamma with vals_phi (new_gamma),
        update new_gamma with types of bound names (final gamma),
        typecheck the body
        *)
        let tclet1 gamma name_supply binds body res =
          Result.bind res (fun
          (vals_phi, ts) -> 
          let ns0, ns1 = split name_supply in
          let new_gamma = perform_subst_on_te vals_phi gamma in
          let final_gamma = add_decls new_gamma ns0 binds ts in
          let body_res = (typeCheck final_gamma ns1 body) in
          tclet2 vals_phi body_res
          )
        in
        let ns0, ns1 = split name_supply in
        let val_results = (tcl gamma ns1 [val_expr]) in
        tclet1 gamma ns0 [name] body val_results
    | ILetRec(bindings, body, tag) ->
        let xs = List.map (fun ((x,_),_) -> x) bindings in
        let es = List.map snd bindings in
        (* schemify a (monomorphic) type *)
        let new_bvar (x, tvn) = (x, SForall([], TyVar(tvn, tag))) in
        (* schemify a list of (monomorphic) types *)
        let new_bvars (xs : string list) ns = List.map new_bvar (special_zip xs (name_sequence ns)) in
        (* un-schemifies a monomorphic scheme *)
        let old_bvar (x, SForall(names, t)) =
          match names with
          | [] -> t
          | _::_ -> raise (InternalError("old_bvar got an unexpected nonempty scheme"))
        in
        let tletrec2 gamma ns bindings_env body res =
          Result.bind res (fun
          (phi) ->
          let ns0, ns1 = split ns in
          let new_bindings_env = perform_subst_on_te phi bindings_env in
          let ts = List.map old_bvar new_bindings_env in
          let new_gamma = perform_subst_on_te phi gamma in
          let final_gamma = add_decls new_gamma ns0 (List.map fst bindings_env) ts in
          tclet2 phi (typeCheck final_gamma ns1 body))
        in
        let tletrec1  gamma ns bindings_env body res =
          Result.bind res (fun
          (phi, ts) ->
          let new_gamma = perform_subst_on_te phi gamma in
          let new_bindings_env = perform_subst_on_te phi bindings_env in
          let new_ts = List.map old_bvar new_bindings_env in
          tletrec2 new_gamma ns new_bindings_env body (unifyl phi (zip ts new_ts)))
        in
        let ns0, ns' = split name_supply in
        let ns1, ns2 = split ns' in
        let bindings_env = new_bvars xs ns2 in
        tletrec1 gamma ns0 bindings_env body (tcl (bindings_env @ gamma) ns1 es)
    | ILambda(name_opt, (argname, argname_tag), body, tag) ->
        let ty_var_name = next_name name_supply in
        let new_name_supply = deplete name_supply in
        let new_assoc = (argname, SForall([], TyVar(ty_var_name, tag))) in (* maps argname to a fresh (monomorphic) unkown variable *)
        let new_gamma = new_assoc::gamma in (* environment with bound variable's (monomorphic) type *)
        let body_type_res = (typeCheck new_gamma new_name_supply body) in (* the new subst after type checking the body (phi) and the type of the body (t) *)
        (* the new subst will know the type of argname *)
        Result.bind body_type_res (fun
        (phi, t) -> Ok((phi, arrow (phi ty_var_name tag) t tag)))
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