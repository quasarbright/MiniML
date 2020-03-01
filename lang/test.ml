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
]

let interpret_tests = "interpret_tests">:::[
  t_interpret "69" "69" "69";
  t_interpret "-100" "-100" "-100";
  t_interpret "true" "true" "true";
  t_interpret "false" "false" "false";
]

let () = 
  List.iter run_test_tt_main
  [
    suite;
    parse_tests;
    interpret_tests;
  ]

let e = (~-)