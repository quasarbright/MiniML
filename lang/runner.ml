open Printf
open Lexing
open Parser
open Lexer
open Exprs
open Values
open Pretty
open OUnit2

let parse (name : string) lexbuf : sourcespan program  =
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Parser.program Lexer.token lexbuf
  with
  | (Failure msg) as exn ->
     if msg = "lexing: empty token" then
       raise (Failure (sprintf "Lexical error at %s" (string_of_position lexbuf.lex_curr_p)))
     else
       let bt = Printexc.get_raw_backtrace () in
       Printexc.raise_with_backtrace exn bt (* make sure we throw with the same stack trace *)
  | Parsing.Parse_error ->
     begin
       let curr = lexbuf.Lexing.lex_curr_p in
       let line = curr.Lexing.pos_lnum in
       let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
       let tok = Lexing.lexeme lexbuf in
       raise (Failure (sprintf "Parse error at line %d, col %d: token `%s`"
                            line cnum tok))
     end

let parse (name : string) lexbuf : sourcespan program  =
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Parser.program Lexer.token lexbuf
  with
  | (Failure msg) as exn ->
     if msg = "lexing: empty token" then
       raise (Failure (sprintf "Lexical error at %s" (string_of_position lexbuf.lex_curr_p)))
     else
       let bt = Printexc.get_raw_backtrace () in
       Printexc.raise_with_backtrace exn bt (* make sure we throw with the same stack trace *)
  | Parsing.Parse_error ->
     begin
       let curr = lexbuf.Lexing.lex_curr_p in
       let line = curr.Lexing.pos_lnum in
       let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
       let tok = Lexing.lexeme lexbuf in
       raise (Failure (sprintf "Parse error at line %d, col %d: token `%s`"
                            line cnum tok))
     end

(* Read a file into a string *)
let string_of_file (file_name : string) : string =
  let inchan = open_in file_name in
  let ans = really_input_string inchan (in_channel_length inchan) in
  close_in inchan;
  ans

let parse_string (name : string) (s : string) : sourcespan program = 
  let lexbuf = Lexing.from_string s in
  try
    parse name lexbuf
  with Failure(msg) ->
    let msg' = sprintf "%s: %s" name msg in
    failwith msg'

let parse_file (name : string) input_file : sourcespan program = 
  let lexbuf = Lexing.from_channel input_file in
  parse name lexbuf


let run_file runner filename =
  filename
    |> string_of_file
    |> parse_string filename
    |> runner

let file_to_out_err runner filename =
  let value = 
    filename
    |> run_file runner
  in
  match value with
    | VErr _ -> ("", (string_of_value value))
    | _ -> ((string_of_value value), "")


let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try 
    ignore (Str.search_forward re s1 0); 
    true
  with Not_found -> false

let ends_with s ending =
  let l1 = String.length s in
  let l2 = String.length ending in
  if l1 < l2 then false else
  let re = Str.regexp (sprintf "%s$" ending) in
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found -> false

let begins_with s beginning =
  let re = Str.regexp (sprintf "^%s" beginning) in
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found -> false

let drop_ending ending s =
  let re = sprintf "%s$" ending |> Str.regexp in
  Str.replace_first re "" s

let get_filenames_matching_extension dirname ext =
  let filenames = dirname |> Sys.readdir |> Array.to_list in
  filenames |> List.filter (fun filename -> ends_with filename ext)

let get_strings_with_ending strings ending =
  strings |> List.filter (fun filename -> ends_with filename ending)


let remove_dups xs =
  let rec help xs seen acc =
    match xs with
      | [] -> acc
      | x::xs' ->
          if List.mem x seen
          then help xs' seen acc
          else (help xs' (x::seen) (x::acc))
  in
  help xs [] [] |> List.rev

(** expects extensions with no "." ex: "mml"
and expects input_dir to be flat with files with "file.mml"-like names *)
let get_input_test_triplets input_dir source_ext out_ext err_ext =
  let input_filenames = input_dir |> Sys.readdir |> Array.to_list |> List.map (sprintf "%s/%s" input_dir) in
  let input_filenames_no_ext =
    input_filenames
    |> List.map (fun filename -> drop_ending "\\.[a-zA-z_-]*" filename)
    |> remove_dups
  in
  input_filenames_no_ext
  |> List.map
      (fun filename_no_ext ->
        let get_maybe ext =
          List.find_opt (fun filename -> begins_with filename filename_no_ext && ends_with filename ext) input_filenames
        in
        (get_maybe source_ext, get_maybe out_ext, get_maybe err_ext))

let assert_string_equal s1 s2 = assert_equal s1 s2 ~printer:(fun s -> s)

let assert_substring sub s =
          if contains s sub
          then ()
          else assert_string_equal sub s


(**
runs the code at filename src

If m_out is present, assert the output is identical to it.
Otherwise, ignore any output

If m_err is present, assert that it is a substring of the actual error output
Otherwise, assert that there is no error output

NOTE: to ensure that there is just some error and not care about its contents, make it Some("")
*)
let t_triplet runner (src, m_out, m_err) =
  src>::(fun _ ->
  let actual_output, actual_error_output = file_to_out_err runner src in
  begin
    match m_out with
      | None -> ()
      | Some(expected_output_filename) ->
          let expected_output = string_of_file expected_output_filename in
          assert_string_equal expected_output actual_output
  end;
  begin
    match m_err with
      | None -> assert_string actual_error_output
      | Some(expected_error_output_filename) -> 
        let expected_error_output = string_of_file expected_error_output_filename in
        assert_substring expected_error_output actual_error_output
  end
  )


let combine_opt_triples opt_triples =
  let rec help opt_triples acc =
    match opt_triples with
      | [] -> acc
      | (Some(x),y,z)::opt_triples' -> help opt_triples' ((x,y,z)::acc)
      | (None,_,_)::opt_triples' -> help opt_triples' acc
  in
  List.rev (help opt_triples [])

let input_file_suite runner =
  "inputs">:::(
    get_input_test_triplets "inputs" "mml" "out" "err"
    |> combine_opt_triples
    |> List.map (t_triplet runner)
  )