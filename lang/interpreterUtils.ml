open Exprs
open Values
open Types

type 'a envt = (string * 'a) list

let find_opt (assoc : 'a envt) k = List.assoc_opt k assoc
