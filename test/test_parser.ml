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

let check_round_trip msg input =
    let (ast1, _) = parse_source input in 
    let printed1 = Pretty.to_source ast1 in 
    let (ast2, _) = parse_source printed1 in 
    let printed2 = Pretty.to_source ast2 in
    Alcotest.(check string) msg printed1 printed2

(* -------------------- BASIC TESTS -------------------- *)

(*

The following tests test the functionality of parser at the most basic level: items.
These tests check that the parser is correctly parsing top-level items with ONLY one item.

For example: const x = 5 -> const is the only top-level item.
Other tests will test multiple items, as well as other edge cases.

*)

(* -------------------- Const Tests -------------------- *)

let test_const_int_literal () = check_round_trip "int" "const x = 5"

let tests = [
    ("const literals", [
        Alcotest.test_case "const int literal" `Quick test_const_int_literal;
    ]);
]