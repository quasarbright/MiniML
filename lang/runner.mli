val parse_string : string -> string -> Exprs.sourcespan Exprs.program
val parse_file : string -> Exprs.sourcespan Exprs.program
val run_string :
  string -> string -> (Exprs.sourcespan Values.value, exn list) result
val run_file : string -> (Exprs.sourcespan Values.value, exn list) result
val string_to_out_err : string -> string -> string * string
val file_to_out_err : string -> string * string
val input_file_suite :
  unit -> OUnit2.test