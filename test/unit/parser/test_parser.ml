open Frontend
open Alcotest

(* -------------------- Helper Functions -------------------- *)

let parse_source input = 
    let src = Source.create input "test.aera" in 
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

(* -------------------- Literal Expression Tests -------------------- *)

let test_int_literal () = check_round_trip "int" "const x = 5"
let test_float_literal () = check_round_trip "float" "const x = 3.14"
let test_char_literal () = check_round_trip "char" "const x = 'a'"
let test_string_literal () = check_round_trip "string" "const x = \"hello\""
let test_bool_literal () = check_round_trip "bool" "const x = true"
let test_escaped_char () = check_round_trip "escaped char" "const x = '\\n'"
let test_escaped_string () = check_round_trip "escaped string" "const x = \"a\\\"b\""
let test_string_with_quote () = check_round_trip "string with apostrophe" " const x = \" it's fine\""
let test_negative_int () = check_round_trip "negative int" "const x = -5"

(* -------------------- Expression Without Block Tests -------------------- *)

let test_binary_precedence () = check_round_trip "precedence" "const x = 1 + 2 * 3"
let test_assign () = check_round_trip "assign" "fn f() { var x = 1 x += 1 }"
let test_grouping () = check_round_trip "grouping" "const x = (1 + 2) * 3"
let test_tuple () = check_round_trip "tuple" "const x = (1, 2, 3)"
let test_single_tuple () = check_round_trip "tuple" "const x = (1,)"
let test_fixed_array () = check_round_trip "fixed array" "const x = [1, 2, 3]"
let test_index () = check_round_trip "index" "const x = arr[0]"
let test_field_access () = check_round_trip "field access" "const x = obj.field"
let test_tuple_access () = check_round_trip "tuple access" "const x = t.0"
let test_tuple_access_chained () = check_round_trip "chained tuple access" "const x = (t.0).1"
let test_mixed_field_and_tuple_access () = check_round_trip "mixed field/tuple access" "const x = obj.field.0"
let test_call () = check_round_trip "call" "const x = f(1, 2)"
let test_named_arg () = check_round_trip "named arg" "const x = f(a: 1, b: 2)"
let test_chained () = check_round_trip "chained postfix" "const x = a.b[0].c(1)"

(* -------------------- Expressions With Block Tests -------------------- *)

let test_block () = check_round_trip "block" "fn f() { const x = 1 }"
let test_if () = check_round_trip "if" "fn f() { if true { 1 } else { 2 } }"
let test_if_no_else () = check_round_trip "if no else" "fn f() { if true { 1 } }"
let test_while () = check_round_trip "while" "fn f() { while true { 1 } }"
let test_loop () = check_round_trip "loop" "fn f() { loop { 1 } }"
let test_let_var() = check_round_trip "let and var" "fn f() { let x = 1 var y: int32 = 2 }"

(* -------------------- Item Tests -------------------- *)

let test_add_fn () = check_round_trip "fn add" "fn add(a: int32, b: int32) { a + b }"
let test_sum_fn () = check_round_trip "fn sum" "fn sum(x: int32) { let res = 0 while x < 0 { res += 1 x-= 1 } res }"
let test_closure () = check_round_trip "closure to calc power" "const f = fn(x) => x * x"
let test_closure_with_block () = check_round_trip "closure with block" "const f = fn(x, y) => { if x > 0 { x - y } else { y - x } }"
let test_struct () = check_round_trip "struct" "struct Point { x: int32 y: int32 }"
let test_struct_default () = check_round_trip "struct default field" "struct Point { x: int32 = 0 y: int32 = 0 }"
let test_struct_mixed () = check_round_trip "struct default field" "struct Point { x: int32 = 0 y: int32 }"
let test_const_typed () = check_round_trip "typed const" "const x: int32 = 5"

(* -------------------- Parser Tests -------------------- *)

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
    ("expression_without_block", [
        Alcotest.test_case "binary precedence" `Quick test_binary_precedence;
        Alcotest.test_case "assign" `Quick test_assign;
        Alcotest.test_case "grouping" `Quick test_grouping;
        Alcotest.test_case "tuple" `Quick test_tuple;
        Alcotest.test_case "single tuple" `Quick test_single_tuple;
        Alcotest.test_case "fixed array" `Quick test_fixed_array;
        Alcotest.test_case "index" `Quick test_index;
        Alcotest.test_case "field access" `Quick test_field_access;
        Alcotest.test_case "tuple access" `Quick test_tuple_access;
        Alcotest.test_case "chained tuple access" `Quick test_tuple_access_chained;
        Alcotest.test_case "mixed field/tuple access" `Quick test_mixed_field_and_tuple_access;
        Alcotest.test_case "call" `Quick test_call;
        Alcotest.test_case "named arg" `Quick test_named_arg;
        Alcotest.test_case "chained postfix" `Quick test_chained;
    ]);
    ("expression_with_block", [
        Alcotest.test_case "block" `Quick test_block;
        Alcotest.test_case "if" `Quick test_if;
        Alcotest.test_case "if no else" `Quick test_if_no_else;
        Alcotest.test_case "while loop" `Quick test_while;
        Alcotest.test_case "infinite loop" `Quick test_loop;
        Alcotest.test_case "let and var" `Quick test_let_var;
    ]);
        ("items", [
        Alcotest.test_case "add function" `Quick test_add_fn;
        Alcotest.test_case "sum function" `Quick test_sum_fn;
        Alcotest.test_case "power closure" `Quick test_closure;
        Alcotest.test_case "closure with block" `Quick test_closure_with_block;
        Alcotest.test_case "struct" `Quick test_struct;
        Alcotest.test_case "struct with default fields" `Quick test_struct_default;
        Alcotest.test_case "struct with mixed fields" `Quick test_struct_mixed;
        Alcotest.test_case "typed const" `Quick test_const_typed;
    ]);
]

(* -------------------- Driver Function -------------------- *)

let () =
    Alcotest.run "Parser" tests