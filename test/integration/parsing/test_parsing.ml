open Frontend
open Alcotest

(* -------------------- Helper Functions -------------------- *)

let read_file path =
    let contents = In_channel.with_open_bin path In_channel.input_all in
    match contents with 
    | contents                  -> Ok (contents, Filename.basename path) (* input, filename *)
    | exception Sys_error msg   -> Error (msg)

let parse_source path = 
    match read_file path with 
    | Ok (input, filename) -> 
        let src = Source.create input filename in 
        let lex = Lexer.read_tokens (Lexer.create src []) in 
        Parser.parse (Parser.create src lex.tokens [])
    | Error msg -> failwith msg

let pp_item fmt item =
    Format.fprintf fmt "%s" (String.concat " " (Pretty.walk_item item))

let item_testable = Alcotest.testable pp_item ( = )

let check_round_trip msg input =
    let (ast1, _) = parse_source input in
    let printed = Pretty.to_source ast1 in
    let (ast2, _) = parse_source printed in
    Alcotest.(check (list item_testable)) msg ast1 ast2

(* -------------------- Valid Program Tests -------------------- *)


(* -------------------- Invalid Program Tests -------------------- *)