open Frontend
open Eval
open Value

let read_file path =
    let contents = In_channel.with_open_bin path In_channel.input_all in
    match contents with 
    | contents                  -> Ok (contents)
    | exception Sys_error msg   -> Error (msg)

let interpret path =
    let env = [StringMap.empty] in (* initialize empty environment *)
    match read_file path with 
    | Error e -> Printf.eprintf "error: could not read file: %s\n" e
    | Ok source -> 
        let lex = Lexer.read_tokens (Lexer.init source) in 
        let (program, _) = Parser.parse (Parser.init lex.tokens lex.reporter) in
        begin 
            match env |> eval program.items with 
            | Error e -> print_endline e 
            | Ok (value, env') -> print_value value
        end