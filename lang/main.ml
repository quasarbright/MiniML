open Interpreter
open Runner
open Pretty
open Values

let () =
  if Array.length Sys.argv < 2
  then print_endline "must supply filename to main"
  else
    let filename = Sys.argv.(1) in
    (* let p = parse_string filename (string_of_file filename) in
    print_endline (string_of_program p); *)
    let out, err = 
      match file_to_result filename with
      | Ok((out, err), phases) -> out, err
      | Error((errs, phases)) -> "", string_of_errors errs
    in
    Printf.printf "%s" out;
    Printf.eprintf "%s" err