open Printf
open Exprs
open Values
open Errors
open Pretty

type phase =
  | Source of string
  | Parsed of sourcespan program
  | WellFormed of sourcespan program
  | TypeInferred of sourcespan program
  | TypeChecked of sourcespan program
  | Desugared of sourcespan program
  | Evaluated of sourcespan value 
  | Result of string * string

let source s = Source s
let parsed p = Parsed p
let well_formed p = WellFormed p
let desugared p = Desugared p
let type_inferred p = TypeInferred p
let type_checked p = TypeChecked p
let evaluated v = Evaluated v
let result (out, err) = Result(out, err)

type failure = exn list * phase list

type 'a fallible = ('a, exn list) result

type 'a pipeline = ('a * phase list, failure) result

let add_err_phase
      (log : 'b -> phase)
      (next : 'a -> 'b fallible)
      (cur_pipeline : 'a pipeline)
    : 'b pipeline =
  match cur_pipeline with
  | Error (errs, trace) -> Error (errs, trace)
  | Ok (cur_val, trace) ->
     try
       match (next cur_val) with
       | Error errs -> Error(errs, trace)
       | Ok new_val -> Ok(new_val, (log new_val) :: trace)
     with
     | Failure s -> Error([Failure("Error: " ^ s)], trace)
     | err -> Error([Failure("Unexpected error: " ^ Printexc.to_string err)], trace)


let add_phase
      (log : 'b -> phase)
      (next : 'a -> 'b)
      (cur_pipeline : 'a pipeline)
    : 'b pipeline =
  match cur_pipeline with
  | Error(errs, trace)-> Error(errs, trace)
  | Ok(cur_val, trace) ->
     try
       let new_val = next cur_val in
       Ok(new_val, (log new_val) :: trace)
     with
     | Failure s -> Error([Failure("Error: " ^ s)], trace)
     | err -> Error([Failure("Unexpected error: " ^ Printexc.to_string err)], trace)

let no_op_phase (cur_pipeline : 'a pipeline) = cur_pipeline

let print_trace (trace : phase list) : string list =
  let phase_name p = match p with
    | Source _ -> "Source"
    | Parsed _ -> "Parsed"
    | WellFormed _ -> "Well-formed"
    | Desugared _ -> "Desugared"
    | TypeInferred _ -> "TypeInferred"
    | TypeChecked _ -> "TypeChecked"
    | Evaluated _ -> "Evaluated"
    | Result _ -> "Result"
  in
  let string_of_phase p = match p with
    | Source s -> s
    | Parsed p
    | WellFormed p -> string_of_program p
    | Desugared p -> string_of_program p
    | TypeInferred p
    | TypeChecked p -> string_of_program p
    | Evaluated(v) -> string_of_value v
    | Result(out, err) -> sprintf "output:\n%s\nerror:\n%s" out err
  in
  List.mapi (fun n p -> sprintf "Phase %d (%s):\n%s" n (phase_name p) (string_of_phase p)) (List.rev trace)
