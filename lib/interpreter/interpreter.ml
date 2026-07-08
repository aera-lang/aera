open Frontend
open Eval
open Value

let read_file path =
    let contents = In_channel.with_open_bin path In_channel.input_all in
    match contents with 
    | contents                  -> print_endline contents; Ok (contents)
    | exception Sys_error msg   -> Error (msg)

let print_value v =
    match v with 
    | VInt n        -> print_endline (string_of_int n)
    | VFloat f      -> print_endline (string_of_float f)
    | VBool b       -> print_endline (string_of_bool b)
    | VChar c       -> print_endline ("\'" ^ (String.make 1 c) ^ "\'")
    | VString s     -> print_endline ("\"" ^ s ^ "\"")
    | VUnit         -> print_endline "unit"

let interpret path =
    match read_file path with 
    | Error e -> Printf.eprintf "error: could not read file: %s\n" e
    | Ok source -> 
        let lex = Lexer.read_tokens (Lexer.init source) in 
        let res = Parser.expr (Parser.init lex.tokens lex.reporter) in (* EXPR ONLY PARSES THE FIRST EXPRESSION - not an error since functions, 
                                                                        structs and variants should only be allowed at the top-level
                                                                        , so currently the interpret parses only the first expression.
                                                                        working on statements function closures, so we'll leave this for now *)
        match res with 
        | Error (msg, _, _) -> print_endline msg
        | Ok (expr, _) -> 
            (match eval expr with 
            | Error e   -> print_endline e
            | Ok value  -> print_value value)

let rec repl () = 
    print_string "~> ";
    flush stdout;
    match input_line stdin with 
    | exception End_of_file -> print_endline "thanks for using aera!"
    | "quit" -> print_endline "thanks for using aera!"
    | line -> 
            let lex = Lexer.read_tokens (Lexer.init line) in
            let res = Parser.expr (Parser.init lex.tokens lex.reporter) in
            (match res with
            | Error (msg, _, _) -> print_endline msg
            | Ok (expr, _) ->
                (match eval expr with
                | Error e -> print_endline e
                | Ok value -> print_value value));
            repl ()