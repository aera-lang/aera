open Frontend
open Alcotest

(* -------------------- Helper Functions -------------------- *)

let read_file path =
    match In_channel.with_open_bin path In_channel.input_all with
    | contents                  -> Ok (contents, Filename.basename path) (* input, filename *)
    | exception Sys_error msg   -> Error (msg)

let parse_source input = 
    let src = Source.create input "fixture.aera" in 
    let lex = Lexer.read_tokens (Lexer.create src []) in 
    Parser.parse (Parser.create src lex.tokens [])

let pp_item fmt item =
    Format.fprintf fmt "%s" (String.concat " " (Pretty.walk_item item))

let item_testable = Alcotest.testable pp_item ( = )

let check_round_trip msg input =
    let (ast1, _) = parse_source input in
    let printed = Pretty.to_source ast1 in
    let (ast2, _) = parse_source printed in
    Alcotest.(check (list item_testable)) msg ast1 ast2

let test_fixture name = 
    let path = "../../fixtures/" ^ name in 
    match read_file path with 
    | Ok (src, filename) -> 
        check_round_trip name src
    | Error msg -> failwith msg

(* -------------------- Valid Program Tests -------------------- *)

let test_basic_program () = test_fixture "parsing/valid/basic.aera"
let test_closures () = test_fixture "parsing/valid/basic.aera"
let test_collections () = test_fixture "parsing/valid/basic.aera"
let test_functions () = test_fixture "parsing/valid/basic.aera"
let test_structs () = test_fixture "parsing/valid/basic.aera"


(* -------------------- Program Tests -------------------- *)


let tests = [
    ("valid programs", [
        Alcotest.test_case "basic program" `Quick test_basic_program;
        Alcotest.test_case "closures" `Quick test_closures;
        Alcotest.test_case "collections (arrays / tuples)" `Quick test_collections;
        Alcotest.test_case "funuctions" `Quick test_functions;
        Alcotest.test_case "structs" `Quick test_structs;
    ]);
]

(* -------------------- Driver Function -------------------- *)

let () =
    Alcotest.run "Parsing" tests
