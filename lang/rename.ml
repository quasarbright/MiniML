open Exprs
open Printf

let tag ((e, prog_tag) : 'a program) : (tag * 'a) program =
  let count = ref 0 in
  let get_next_count _ =
    let ans = !count in
    count := ans + 1;
    ans
  in
  (map_expr_tag (fun t -> get_next_count(), t) e, (get_next_count(), prog_tag))


let rename ((e, tag) : (tag * 'a) program) : (tag * 'a) program =
  let gen_sym name tag = sprintf "%s_%d" name (fst tag) in
  let rec renameE env e =
    match e with
    | EId(name, tag) -> EId(List.assoc name env, tag)
    | ELet((tagged_names, val_expr, bind_tag), body_expr, tag) ->
        let new_tagged_names = List.map (fun (name, tag) -> gen_sym name tag, tag) tagged_names in
        let new_env = List.map2 (fun (name, _) (new_name, _) -> (name, new_name)) tagged_names new_tagged_names @ env in
        let renamed_val_expr = renameE env val_expr in
        let renamed_body_expr = renameE new_env body_expr in
        ELet((new_tagged_names, renamed_val_expr, bind_tag), renamed_body_expr, tag)
    | EInt _
    | EBool _
    | EIf _
    | EPrim1 _
    | EPrim2 _
    | EApp _ -> recurse_on_children (renameE env) e
  in
  (renameE [] e, tag)

let tag_and_rename (p : 'a program) : (tag * 'a) program = p |> tag |> rename