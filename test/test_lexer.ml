open Test_lexer_helper

(* Keywords *)

let test_let_binding () =
    let tokens = lex_string "let x = 5" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        int_lit 1 9;
        eof 1 10;
    ]

let test_if_expr () =
    let tokens = lex_string "if true { do_something() }" in
    Test_lexer_helper.expect_tokens tokens [
        if_ 1 1;
        true_lit 1 4;
        left_brace 1 9;
        identifier 1 11;
        left_paren 1 23;
        right_paren 1 24;
        right_brace 1 26;
        eof 1 27;
    ]

let test_while_expr () =
    let tokens = lex_string "while x > 10 { do_something() }" in
    Test_lexer_helper.expect_tokens tokens [
        while_ 1 1;
        identifier 1 7;
        greater 1 9;
        int_lit 1 11;
        left_brace 1 14;
        identifier 1 16;
        left_paren 1 28;
        right_paren 1 29;
        right_brace 1 31;
        eof 1 32;
    ]

let test_loop_expr () =
    let tokens = lex_string "loop { do_something() }" in 
    Test_lexer_helper.expect_tokens tokens [
        loop 1 1;
        left_brace 1 6;
        identifier 1 8;
        left_paren 1 20;
        right_paren 1 21;
        right_brace 1 23;
        eof 1 24;

    ]    

let test_fn_item () = 
    let tokens = lex_string "fn add(a, b) -> int32 { a + b }" in 
    Test_lexer_helper.expect_tokens tokens [
        fn 1 1;
        identifier 1 4;
        left_paren 1 7;
        identifier 1 8;
        comma 1 9;
        identifier 1 11;
        right_paren 1 12;
        minus_greater 1 14;
        identifier 1 17;
        left_brace 1 23;
        identifier 1 25;
        plus 1 27;
        identifier 1 29;
        right_brace 1 31;
        eof 1 32;
    ]
   
let test_let_mut_binding () = 
    let tokens = lex_string "let mut x = 3.14" in 
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        mut 1 5;
        identifier 1 9;
        equal 1 11;
        float_lit 1 13;
        eof 1 17;
    ]

(* Strings *)

let test_empty_string () = 
    let tokens = lex_string "let empty = \"\"" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 11;
        str_lit 1 13;
        eof 1 15;
    ]

let test_escaped_quotes_blacklash () = 
    let tokens = lex_string "let s = \"a \\\"quoted\\\" string and a \\\\ backslash\"" in 
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        str_lit 1 9;
        eof 1 49;
    ]

let test_unterminated_string () = 
    let tokens = lex_string "let s = \"abc" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        illegal 1 9;
        eof 1 13;
    ]

let test_invalid_escape_string () = 
    let tokens = lex_string "\"hello \\z world\"" in
    Test_lexer_helper.expect_tokens tokens [
        illegal 1 1;
        eof 1 17;
    ]

(* Characters *)

let test_simple_char () = 
    let tokens = lex_string "let c = 'a'" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        char_lit 1 9;
        eof 1 12;
    ]

let test_escaped_character () = 
    let tokens = lex_string "let newline = '\\n'" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 13;
        char_lit 1 15;
        eof 1 19;
    ]

let test_empty_char () = 
    let tokens = lex_string "let c = ''" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        illegal 1 9; 
        eof 1 11;
    ]

let test_long_char () = 
    let tokens = lex_string "let c = 'ab'" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        illegal 1 9; 
        eof 1 13;
    ]

let test_invalid_char () = 
    let tokens = lex_string "let x = $" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        illegal 1 9;
        eof 1 10;
    ]

(* Integers *)

let test_binary_int () = 
    let tokens = lex_string "let answer = 0b001" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 12;
        int_lit 1 14;
        eof 1 19;
    ]

let test_octal_int () = 
    let tokens = lex_string "let answer = 0o512" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 12;
        int_lit 1 14;
        eof 1 19;
    ]
    
let test_hexadecimal_int () = 
    let tokens = lex_string "let answer = 0xFF0000" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 12;
        int_lit 1 14;
        eof 1 22;
    ]

let test_decimal_int () = 
    let tokens = lex_string "let answer = 42" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 12;
        int_lit 1 14;
        eof 1 16;
    ]

(* Float *)

let test_float () = 
    let tokens = lex_string "let pi = 3.14" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 8;
        float_lit 1 10;
        eof 1 14;
    ]

let test_float_sci () = 
    let tokens = lex_string "let val = 1.7e12" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 9;
        float_lit 1 11;
        eof 1 17;
    ]

let test_float_sci_pos () = 
    let tokens = lex_string "let e = 2.7e+5" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        float_lit 1 9;
        eof 1 15;
    ]

let test_float_sci_neg () = 
    let tokens = lex_string "let lr = 0.1e-5" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 8;
        float_lit 1 10;
        eof 1 16;
    ]

let test_float_trailing_dot () = 
    let tokens = lex_string "let pi = 3." in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 8;
        float_lit 1 10;
        eof 1 12;
    ]

let test_malformed_number_extra_dot () = 
    let tokens = lex_string "let pi = 3.14." in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 8;
        illegal 1 10;
        eof 1 15;
    ]

let test_malformed_sci () = 
    let tokens = lex_string "let lr = 0.1e-5." in 
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 8;
        illegal 1 10;
        eof 1 17;
    ]

(* Comments *)

let test_line_comment () = 
    let tokens = lex_string "# this is a comment and should be ignored" in
    Test_lexer_helper.expect_tokens tokens [
        eof 1 42;
    ]

let test_block_comment () = 
    let tokens = lex_string "<# this is a block comment and should be ignored #>" in
    Test_lexer_helper.expect_tokens tokens [
        eof 1 52;
    ]

let test_comment_ignored () = 
    let tokens = lex_string "let x = 5 # assign 5 to x" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        int_lit 1 9;
        eof 1 26;
    ]

let test_unterminated_block_comment () = 
    let tokens = lex_string "let x = 5 <# this is not closed" in
    Test_lexer_helper.expect_tokens tokens [
        let_ 1 1;
        identifier 1 5;
        equal 1 7;
        int_lit 1 9;
        illegal 1 11;
        eof 1 32;
    ]

let test_malformed_block_comment () = 
    let tokens = lex_string "<# this is not closed again #" in
    Test_lexer_helper.expect_tokens tokens [
        illegal 1 1;
        eof 1 30;
    ]

(* Multi-Line *)

let test_hello_world () = 
    let input = {|fn main() {
    print("Hello world")
}
|} in
    let tokens = lex_string input in 
    Test_lexer_helper.expect_tokens tokens [
        fn 1 1;
        identifier 1 4;
        left_paren 1 8;
        right_paren 1 9;
        left_brace 1 11;
        identifier 2 5;
        left_paren 2 10;
        str_lit 2 11;
        right_paren 2 24;
        right_brace 3 1;
        eof 4 1;
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
        (* main function *)
        fn 1 1;
        identifier 1 4;
        left_paren 1 8;
        right_paren 1 9;
        left_brace 1 11;
        let_ 2 5;
        identifier 2 9;
        equal 2 13;
        identifier 2 15;
        left_paren 2 18;
        int_lit 2 19;
        right_paren 2 20;
        identifier 3 5;
        left_paren 3 10;
        identifier 3 11;
        right_paren 3 14;
        right_brace 4 1;

        (* sum function *)
        fn 6 1;
        identifier 6 4;
        left_paren 6 7;
        identifier 6 8;
        colon 6 11;
        identifier 6 13;
        right_paren 6 18;
        minus_greater 6 20;
        identifier 6 23;
        left_brace 6 29;
        let_ 7 5;
        mut 7 9;
        identifier 7 13;
        colon 7 18;
        identifier 7 20;
        equal 7 26;
        int_lit 7 28;
        for_ 8 5;
        identifier 8 9;
        in_ 8 11;
        int_lit 8 14;
        period_period 8 15;
        identifier 8 17;
        left_brace 8 21;
        identifier 9 9 ;
        plus_equal 9 15;
        identifier 9 18;
        right_brace 10 5;
        return_ 11 5;
        identifier 11 12;
        right_brace 12 1;
        eof 13 1;
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
        Alcotest.test_case "block comment" `Quick test_block_comment;
        Alcotest.test_case "comment ignored" `Quick test_comment_ignored;
        Alcotest.test_case "unterminated block comment" `Quick test_unterminated_block_comment;
        Alcotest.test_case "malformed block comment" `Quick test_malformed_block_comment;
    ]);
    ("multi-line", [
        Alcotest.test_case "hello world" `Quick test_hello_world;
        Alcotest.test_case "sum function" `Quick test_fn_and_loop;
    ]);
]