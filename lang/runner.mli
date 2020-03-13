val parse_string : string -> string -> Exprs.sourcespan Exprs.program
val parse_file : string -> Exprs.sourcespan Exprs.program
val run_string :
  string -> string -> Exprs.sourcespan Values.value Phases.pipeline
val run_file : string -> Exprs.sourcespan Values.value Phases.pipeline
val string_to_out_err : string -> string -> string * string
val string_to_result : string -> string -> (string * string) Phases.pipeline
val file_to_out_err : string -> string * string
val file_to_result : string -> (string * string) Phases.pipeline
val input_file_suite :
  unit -> OUnit2.test