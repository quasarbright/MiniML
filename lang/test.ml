open OUnit2
open ExtLib

open Parser
open Lexer
open Runner
open Exprs
open Pretty
open Interpreter



let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_parse name prog expected =
  t_any name expected (untag (parse_string (name ^ ".mml") prog)) ~printer:string_of_program

let t_interpret name prog expected =
  t_any name expected (prog |> parse_string (name ^ ".mml") |> interpret |> string_of_value) ~printer:(fun s -> s)


let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try 
       ignore (Str.search_forward re s1 0); 
       true
    with Not_found -> false

let t_error name prog substring =
  let value = prog |> parse_string (name ^ ".mml") |> interpret in
  let value_string = string_of_value value in
  name>::(fun _ -> 
    match value with
      | VErr(_) ->
          if contains value_string substring
          then assert_string ""
          else assert_equal substring value_string ~printer:(fun s -> s)
      | _ -> assert_failure (Printf.sprintf "no error found. got %s" value_string))


let suite = "suite">:::[
    t_any "test" 2 (1+1);
    t_any_err "test-error" (fun () -> failwith "rip") (Failure("rip"));
]


let parse_tests = "parse_tests">:::[
  t_parse "42" "42" ((EInt(42L, ())), ());
  t_parse "neg42" "-42" ((EInt(Int64.neg 42L, ())), ());
  t_parse "zero" "0" ((EInt(0L, ())), ());
  t_parse "true" "true" ((EBool(true, ())), ());
  t_parse "false" "false" ((EBool(false, ())), ());
  t_parse "comment" "1 (* comment *)" ((EInt(1L, ())), ());
  t_parse "neg-paren" "~-(1)" ((EPrim1(UNegate, EInt(1L, ()), ()), ()));
  t_parse "add-left-assoc" "1 + 2 + 3" ((EPrim2(Plus, EPrim2(Plus, EInt(1L, ()), EInt(2L, ()), ()), EInt(3L, ()), ()), ()));
  t_parse "unegate-int" "~-1" ((EPrim1(UNegate, EInt(1L, ()), ()), ()));
]

let integration_tests = "integration_tests">:::[
  t_interpret "69" "69" "69";
  t_interpret "-100" "-100" "-100";
  t_interpret "true" "true" "true";
  t_interpret "false" "false" "false";
  t_interpret "add" "1 + 1" "2";
  t_interpret "or-f-f" "false || false" "false";
  t_interpret "or-f-t" "false || true" "true";
  t_interpret "or-t-f" "true || false" "true";
  t_interpret "or-t-t" "true || true" "true";
  t_interpret "and-f-f" "false && false" "false";
  t_interpret "and-f-t" "false && true" "false";
  t_interpret "and-t-f" "true && false" "false";
  t_interpret "and-t-t" "true && true" "true";
  t_interpret "minus" "1 - 2" "-1";
  t_error "1plustrue" "1 + true" "type";
  t_error "inner_error" "(1 + true) + 2" "type";
  t_error "1" "1 / 0" "Divide by zero";
]

let () = 
  List.iter run_test_tt_main
  [
    suite;
    parse_tests;
    integration_tests;
  ]