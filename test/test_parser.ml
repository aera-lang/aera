open Frontend
open Alcotest

(* -------------------- Helper Functions -------------------- *)

let make_parser src tokens = {
    Parser.source = src;
    tokens = tokens;
    reporter = [];
}

let make_lexer src = {
	Lexer.source = src;
	start = 0;
	curr = 0;
	tokens = [];
	reporter = [];
}

let make_source input filename = Source.create input filename

let parse_source input = 
    let src = make_source input "test.aera" in 
    let lex = Lexer.read_tokens (make_lexer src) in 
    Parser.parse (make_parser src lex.tokens)

let pp_item fmt item =
    Format.fprintf fmt "%s" (String.concat " " (Pretty.walk_item item))

let item_testable = Alcotest.testable pp_item ( = )

let check_round_trip msg input =
    let (ast1, _) = parse_source input in
    print_endline (Pretty.to_source ast1);

    let printed = Pretty.to_source ast1 in
    let (ast2, _) = parse_source printed in
    Alcotest.(check (list item_testable)) msg ast1 ast2

(* -------------------- BASIC TESTS -------------------- *)

(*

The following tests test the functionality of parser at the most basic level: items.
These tests check that the parser is correctly parsing top-level items with ONLY one item.

For example: const x = 5 -> const is the only top-level item.
Other tests will test multiple items, as well as other edge cases.

*)

(* -------------------- Literal Tests -------------------- *)

(* These tests use const, which is similar to a let binding but is known at compile time -> no runtime values allowed / nor changing the value *)

let test_int_literal () = check_round_trip "int" "const x = 5"
let test_float_literal () = check_round_trip "float" "const x = 3.14"
let test_char_literal () = check_round_trip "char" "const x = 'a'"
let test_string_literal () = check_round_trip "string" "const x = \"hello\""
let test_bool_literal () = check_round_trip "bool" "const x = true"
let test_escaped_char () = check_round_trip "escaped char" "const x = '\\n'"
let test_escaped_string () = check_round_trip "escaped string" "const x = \"a\\\"b\""
let test_string_with_quote () = check_round_trip "string with apostrophe" " const x = \" it's fine\""
let test_negative_int () = check_round_trip "negative int" "const x = -5"

let tests = [
    ("literals", [
        Alcotest.test_case "int literal" `Quick test_int_literal;
        Alcotest.test_case "float literal" `Quick test_float_literal;
        Alcotest.test_case "char literal" `Quick test_char_literal;
        Alcotest.test_case "string literal" `Quick test_string_literal;
        Alcotest.test_case "bool literal" `Quick test_bool_literal;
        Alcotest.test_case "escaped char literal" `Quick test_escaped_char;
        Alcotest.test_case "escaped string literal" `Quick test_escaped_string;
        Alcotest.test_case "string with apostrophe" `Quick test_string_with_quote;
        Alcotest.test_case "negative int literal" `Quick test_negative_int;
    ]);
]

