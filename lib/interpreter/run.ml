open Frontend
open Eval
open Value

let read_file path =
    let contents = In_channel.with_open_bin path In_channel.input_all in
    match contents with 
    | contents                  -> print_endline contents; Ok (contents)
    | exception Sys_error msg   -> Error (msg)

let interpret path =
    let env = [StringMap.empty] in (* initialize empty environment *)
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
            (match env |> eval_expr expr with 
            | Error e   -> print_endline e
            | Ok value  -> print_value value)