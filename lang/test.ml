open OUnit2
open ExtLib

open Parser
open Lexer
open Runner
open Exprs
open Pretty
open Interpreter
open Types



let t_any ?cmp:(cmp=(=)) ?printer:(printer=dump) (name : string) expected value = name>::
  (fun _ -> assert_equal expected value ~printer:printer ~cmp:cmp);;

let t_any_err (name : string) (runnable : unit -> 'a) (expected_err : exn) = name>::
  (fun _ -> assert_raises expected_err runnable)

let t_parse name prog expected =
  t_any name expected (untag (parse_string (name ^ ".mml") prog)) ~printer:string_of_program

let t_interpret name prog expected =
  t_any name expected (prog |> string_to_out_err (name ^ ".mml") |> fst) ~printer:(fun s -> s)


let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try 
       ignore (Str.search_forward re s1 0); 
       true
    with Not_found -> false

let t_error name prog substring =
  let out, err = prog |> string_to_out_err (name ^ ".mml") in
  name>::(fun _ -> 
    match err with
      | "" -> assert_failure (Printf.sprintf "no error found. got %s" out)
      | _ ->
          if contains err substring
          then assert_string ""
          else assert_equal substring err ~printer:(fun s -> s)
  )


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
  t_parse "let" "let x = 1 in x" ((ELet(([("x", ())], EInt(1L, ()), ()), EId("x", ()), ()), ()));
  t_parse "if" "if true then 1 else 0" ((EIf(EBool(true, ()), EInt(1L, ()), EInt(0L, ()), ()), ()));
  t_parse "let-seq" "let x = 1 in let y = 2 in 3" ((ELet(([("x",())], EInt(1L, ()), ()), ELet(([("y", ())], EInt(2L, ()), ()), EInt(3L, ()), ()), ()), ()));
  t_parse "let-in-let" "let x = let y = 1 in y in x"
      (ELet(
        (
          [("x", ())],
          ELet(([("y", ())], EInt(1L, ()), ()), EId("y", ()), ()),
          ()
        ),
        EId("x", ()),
        ()
      ), ());
  t_parse "func-def" "let f a b = 1 in 2"
    (ELet(
      (
        [("f", ());("a", ());("b", ())],
        EInt(1L, ()),
        ()
      ),
      EInt(2L, ()),
      ()
    ), ());
  t_parse "app" "f a"
    (EApp(EId("f", ()), EId("a", ()), ()), ());
  t_parse "app-many" "f a b c"
    (EApp(EApp(EApp(EId("f", ()), EId("a", ()), ()), EId("b", ()), ()), EId("c", ()), ()), ());
  t_parse "app-with-prim2" "f a b + g c d"
    (EPrim2(
      Plus,
      (EApp(EApp(EId("f", ()), EId("a", ()), ()), EId("b", ()), ())),
      (EApp(EApp(EId("g", ()), EId("c", ()), ()), EId("d", ()), ())),
      ()
      ), ());
  t_parse "if-in-if" "if if true then true else false then 2 else 3"
    (
      EIf(
        EIf(EBool(true, ()), EBool(true, ()), EBool(false, ()), ()),
        EInt(2L, ()),
        EInt(3L, ()),
        ()
      )
    , ());
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
  t_interpret "let" "let x = 1 in x" "1";
  t_interpret "shadow" "let x = 1 in let x = 2 in x" "2";
  t_interpret "scoping" "let x = (let x = 2 in 1) in x" "1";
  t_interpret "if" "if true then 1 else 0" "1";
  t_error "1plustrue" "1 + true" "expected int, but got bool";
  t_error "inner_error" "(1 + true) + 2" "expected int, but got bool";
  t_error "1" "1 / 0" "Divide by zero";
  t_error "unbound" "x" "x is not in scope";
  t_error "use-in-def" "let x = x in 1" "x is not in scope";
  t_error "if-not-bool" "if 1 then 1 else 0" "expected bool, but got int";
]

let tyvars_in_tests = "tyvars_in_tests">:::[
  t_any "tvar-simple" ["'x"] (tyvars_in (TyVar("'x", ())));
  t_any "tvar-cons" ["'x";"'y"] (tyvars_in (TyCons("either", [TyVar("'x", ());TyCons("list", [TyVar("'y", ())], ())], ())));
]

let () = 
  List.iter run_test_tt_main
  [
    suite;
    parse_tests;
    integration_tests;
    tyvars_in_tests;
    input_file_suite();
  ]