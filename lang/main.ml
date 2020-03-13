open Interpreter
open Runner
open Pretty
open Values
open Phases

let usage = "usage: main [-t] filename"
let print_usage() = print_endline usage

let () =
  if Array.length Sys.argv < 2
  then print_usage()
  else
    let filename, should_print_trace =
      if Array.length Sys.argv = 2
      then Sys.argv.(1), false
      else if Sys.argv.(1) <> "-t" then failwith usage
      else Sys.argv.(2), true
    in
    (* let p = parse_string filename (string_of_file filename) in
    print_endline (string_of_program p); *)
    let out, err, phases =
      match file_to_result filename with
      | Ok((out, err), phases) -> out, err, phases
      | Error((errs, phases)) -> "", string_of_errors errs, phases
    in
    begin
      if should_print_trace
      then
        let trace_strings = print_trace phases in
        let trace_string = String.concat "\n===========================\n" trace_strings in
        Printf.printf "%s" trace_string
      else
        ()
    end;
    Printf.printf "%s" out;
    Printf.eprintf "%s" err