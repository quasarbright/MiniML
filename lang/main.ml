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
    let value = 
      filename
      |> run_file interpret
    in
    match value with
      | VErr _ -> Printf.eprintf "%s\n" (string_of_value value)
      | _ -> Printf.printf "%s\n" (string_of_value value)