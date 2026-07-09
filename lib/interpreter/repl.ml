open Frontend
open Eval
open Value

let eval_repl repl_value env =
    match repl_value with 
    | Frontend.Ast.ReplExpr expr -> 
        (match eval_expr expr env with (* what if we need to also parse statements? functions? structs? variants? *)
        | Error e -> print_endline e
        | Ok value -> print_value value);
    | ReplStmt stmt -> failwith "implement eval_stmt"
    | ReplItem item -> failwith "implement eval_item"

let rec repl_loop env = 
    print_string "~> ";
    flush stdout;
    match input_line stdin with 
    | exception End_of_file -> print_endline "thanks for using aera!"
    | "quit" -> print_endline "thanks for using aera!"
    | line -> 
            let lex = Lexer.read_tokens (Lexer.init line) in
            let res = Parser.parse_repl (Parser.init lex.tokens lex.reporter) in
            (match res with
            | Error (msg) -> print_endline msg
            | Ok (value) -> env |> eval_repl value);
            env |> repl_loop

let repl () =
    let env = [StringMap.empty] in (* initialize environment  *)
    repl_loop env 

