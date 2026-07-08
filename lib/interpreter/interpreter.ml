open Frontend
open Eval
open Value

(* 

    let contents = In_channel.with_open_bin path In_channel.input_all |> String.split_on_char '\n' in

*)

let read_file path =
    let contents = In_channel.with_open_bin path In_channel.input_all in (* this is actually fine *)
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


let token_to_string kind = (* turn into a proper module *)
  match kind with
  | Token.IntLiteral num 		-> Printf.sprintf "int(%d)" num
  | Token.FloatLiteral num		-> Printf.sprintf "float(%f)" num
  | Token.StringLiteral str		-> Printf.sprintf "str(%s)" str
  | Token.CharLiteral ch		-> Printf.sprintf "char(%c)" ch
  | Token.Identifier str 		-> Printf.sprintf "identifier(%s)" str
  | Token.Let					-> "let"
  | Token.Fn					-> "fn"
  | Token.Equal					-> "="
  | Token.LeftParen             -> "("
  | Token.RightParen            -> ")"
  | Token.Plus                  -> "+"
  | Token.Star                  -> "*"
  | _							-> ""
  (* etc *)

let interpret path =
    match read_file path with 
    | Error e -> Printf.eprintf "error: could not read file: %s\n" e
    | Ok source -> 
        let lex = Lexer.read_tokens (Lexer.init source) in (* lexer is fine *)
        List.iter (fun tok -> print_endline (token_to_string tok.Token.kind)) lex.tokens;
        let res = Parser.expr (Parser.init lex.tokens lex.reporter) in
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