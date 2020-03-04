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
      filename
      |> file_to_out_err
    in
    Printf.printf "%s" out;
    Printf.eprintf "%s" err