open Test_lexer_helper

(* Keywords *)

let test_let_binding () =
    let tokens = lex_string "let x = 5" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        int_lit 8 9;
        eof 9 9;
    ]

let test_if_expr () =
    let tokens = lex_string "if true { do_something() }" in
    Test_lexer_helper.expect_tokens tokens [
        if_ 0 2;
        true_lit 3 7;
        left_brace 8 9;
        identifier 10 22;
        left_paren 22 23;
        right_paren 23 24;
        right_brace 25 26;
        eof 26 26;
    ]

let test_while_expr () =
    let tokens = lex_string "while x > 10 { do_something() }" in
    Test_lexer_helper.expect_tokens tokens [
        while_ 0 5;
        identifier 6 7;
        greater 8 9;
        int_lit 10 12;
        left_brace 13 14;
        identifier 15 27;
        left_paren 27 28;
        right_paren 28 29;
        right_brace 30 31;
        eof 31 31;
    ]

let test_loop_expr () =
    let tokens = lex_string "loop { do_something() }" in 
    Test_lexer_helper.expect_tokens tokens [
        loop 0 4;
        left_brace 5 6;
        identifier 7 19;
        left_paren 19 20;
        right_paren 20 21;
        right_brace 22 23;
        eof 23 23;

    ]    

let test_fn_item () = 
    let tokens = lex_string "fn add(a, b) -> int32 { a + b }" in 
    Test_lexer_helper.expect_tokens tokens [
        fn 0 2;
        identifier 3 6;
        left_paren 6 7;
        identifier 7 8;
        comma 8 9;
        identifier 10 11;
        right_paren 11 12;
        minus_greater 13 15;
        identifier 16 21;
        left_brace 22 23;
        identifier 24 25;
        plus 26 27;
        identifier 28 29;
        right_brace 30 31;
        eof 31 31;
    ]
   
let test_let_mut_binding () = 
    let tokens = lex_string "let mut x = 3.14" in 
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        mut 4 7;
        identifier 8 9;
        equal 10 11;
        float_lit 12 16;
        eof 16 16;
    ]

(* Strings *)

let test_empty_string () = 
    let tokens = lex_string "let empty = \"\"" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 9;
        equal 10 11;
        str_lit 12 14;
        eof 14 14;
    ]

let test_escaped_quotes_blacklash () = 
    let tokens = lex_string "let s = \"a \\\"quoted\\\" string and a \\\\ backslash\"" in 
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        str_lit 8 48;
        eof 48 48;
    ]

let test_unterminated_string () = 
    let tokens = lex_string "let s = \"abc" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        illegal 8 12;
        eof 12 12;
    ]

let test_invalid_escape_string () = 
    let tokens = lex_string "\"hello \\z world\"" in
    Test_lexer_helper.expect_tokens tokens [
        illegal 0 16;
        eof 16 16;
    ]

(* Characters *)

let test_simple_char () = 
    let tokens = lex_string "let c = 'a'" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        char_lit 8 11;
        eof 11 11;
    ]

let test_escaped_character () = 
    let tokens = lex_string "let newline = '\\n'" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 11;
        equal 12 13;
        char_lit 14 18;
        eof 18 18;
    ]

let test_empty_char () = 
    let tokens = lex_string "let c = ''" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        illegal 8 10; 
        eof 10 10;
    ]

let test_long_char () = 
    let tokens = lex_string "let c = 'ab'" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        illegal 8 12; 
        eof 12 12;
    ]

let test_invalid_char () = 
    let tokens = lex_string "let x = $" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        illegal 8 9;
        eof 9 9;
    ]

(* Integers *)

let test_binary_int () = 
    let tokens = lex_string "let answer = 0b001" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 10;
        equal 11 12;
        int_lit 13 18;
        eof 18 18;
    ]

let test_octal_int () = 
    let tokens = lex_string "let answer = 0o512" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 10;
        equal 11 12;
        int_lit 13 18;
        eof 18 18;
    ]
    
let test_hexadecimal_int () = 
    let tokens = lex_string "let answer = 0xFF0000" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 10;
        equal 11 12;
        int_lit 13 21;
        eof 21 21;
    ]

let test_decimal_int () = 
    let tokens = lex_string "let answer = 42" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 10;
        equal 11 12;
        int_lit 13 15;
        eof 15 15;
    ]

(* Float *)

let test_float () = 
    let tokens = lex_string "let pi = 3.14" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 6;
        equal 7 8;
        float_lit 9 13;
        eof 13 13;
    ]

let test_float_sci () = 
    let tokens = lex_string "let val = 1.7e12" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 7;
        equal 8 9;
        float_lit 10 16;
        eof 16 16;
    ]

let test_float_sci_pos () = 
    let tokens = lex_string "let e = 2.7e+5" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        float_lit 8 14;
        eof 14 14;
    ]

let test_float_sci_neg () = 
    let tokens = lex_string "let lr = 0.1e-5" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 6;
        equal 7 8;
        float_lit 9 15;
        eof 15 15;
    ]

let test_float_trailing_dot () = 
    let tokens = lex_string "let pi = 3." in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 6;
        equal 7 8;
        float_lit 9 11;
        eof 11 11;
    ]

let test_malformed_number_extra_dot () = 
    let tokens = lex_string "let pi = 3.14." in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 6;
        equal 7 8;
        illegal 9 14;
        eof 14 14;
    ]

let test_malformed_sci () = 
    let tokens = lex_string "let lr = 0.1e-5." in 
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 6;
        equal 7 8;
        illegal 9 16;
        eof 16 16;
    ]

(* Comments *)

let test_line_comment () = 
    let tokens = lex_string "# this is a comment and should be ignored" in
    Test_lexer_helper.expect_tokens tokens [
        eof 41 41;
    ]

let test_comment_ignored () = 
    let tokens = lex_string "let x = 5 # assign 5 to x" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 0 3;
        identifier 4 5;
        equal 6 7;
        int_lit 8 9;
        eof 25 25;
    ]

(* Multi-Line *)

let test_hello_world () = 
    let input = {|fn main() {
    print("Hello world")
}
|} in
    let tokens = lex_string input in 
    Test_lexer_helper.expect_tokens tokens [
        fn 0 2;
        identifier 3 7;
        left_paren 7 8;
        right_paren 8 9;
        left_brace 10 11;
        identifier 16 21;
        left_paren 21 22;
        str_lit 22 35;
        right_paren 35 36;
        right_brace 37 38;
        eof 39 39;
    ]

let test_fn_and_loop () = 
    let input = {|fn main() {
    let ans = sum(5)
    print(ans)
}

fn sum(num: int64) -> int64 {
    let mut total: int64 = 0
    for i in 0..num {
        total += i
    }
    return total
}
|} in
    let tokens = lex_string input in 
    Test_lexer_helper.expect_tokens tokens [
        (* Main function *)
        fn 0 2;
        identifier 3 7;
        left_paren 7 8;
        right_paren 8 9;
        left_brace 10 11;
        let_ 16 19;
        identifier 20 23;
        equal 24 25;
        identifier 26 29;
        left_paren 29 30;
        int_lit 30 31;
        right_paren 31 32;
        identifier 37 42;
        left_paren 42 43;
        identifier 43 46;
        right_paren 46 47;
        right_brace 48 49;

        (* Sum function *)
        fn 51 53;
        identifier 54 57;
        left_paren 57 58;
        identifier 58 61;
        colon 61 62;
        identifier 63 68;
        right_paren 68 69;
        minus_greater 70 72;
        identifier 73 78;
        left_brace 79 80;
        let_ 85 88;
        mut 89 92;
        identifier 93 98;
        colon 98 99;
        identifier 100 105;
        equal 106 107;
        int_lit 108 109;
        for_ 114 117;
        identifier 118 119;
        in_ 120 122;
        int_lit 123 124;
        period_period 124 126;
        identifier 126 129;
        left_brace 130 131;
        identifier 140 145 ;
        plus_equal 146 148;
        identifier 149 150;
        right_brace 155 156;
        return_ 161 167;
        identifier 168 173;
        right_brace 174 175;
        eof 176 176;
    ]

let tests = [
    ("keywords", [
        Alcotest.test_case "let binding" `Quick test_let_binding;
        Alcotest.test_case "if expression" `Quick test_if_expr;
        Alcotest.test_case "while expression" `Quick test_while_expr;
        Alcotest.test_case "loop expression" `Quick test_loop_expr;
        Alcotest.test_case "fn item" `Quick test_fn_item;
        Alcotest.test_case "let mut binding" `Quick test_let_mut_binding;
    ]);
    ("strings", [
        Alcotest.test_case "empty string" `Quick test_empty_string;
        Alcotest.test_case "escaped quotes and backslash" `Quick test_escaped_quotes_blacklash;
        Alcotest.test_case "unterminated string" `Quick test_unterminated_string;
        Alcotest.test_case "invalid escape string" `Quick test_invalid_escape_string;
    ]);
    ("chars", [
        Alcotest.test_case "simple char" `Quick test_simple_char;
        Alcotest.test_case "escaped character" `Quick test_escaped_character;
        Alcotest.test_case "empty char" `Quick test_empty_char;
        Alcotest.test_case "long char" `Quick test_long_char;
        Alcotest.test_case "invalid char" `Quick test_invalid_char;
    ]);
    ("integers", [
        Alcotest.test_case "binary int" `Quick test_binary_int;
        Alcotest.test_case "octal int" `Quick test_octal_int;
        Alcotest.test_case "hexadecimal int" `Quick test_hexadecimal_int;
        Alcotest.test_case "decimal int" `Quick test_decimal_int;
    ]);
    ("floats", [
        Alcotest.test_case "float" `Quick test_float;
        Alcotest.test_case "float scientific" `Quick test_float_sci;
        Alcotest.test_case "float scientific positive" `Quick test_float_sci_pos;
        Alcotest.test_case "float scientific negative" `Quick test_float_sci_neg;
        Alcotest.test_case "float trailing dot" `Quick test_float_trailing_dot;
        Alcotest.test_case "malformed number extra dot" `Quick test_malformed_number_extra_dot;
        Alcotest.test_case "malformed scientific number" `Quick test_malformed_sci;
    ]);
    ("comments", [
        Alcotest.test_case "line comment" `Quick test_line_comment;
        Alcotest.test_case "comment ignored" `Quick test_comment_ignored;
    ]);
    ("multi-line", [
        Alcotest.test_case "hello world" `Quick test_hello_world;
        Alcotest.test_case "sum function" `Quick test_fn_and_loop;
    ]);
]