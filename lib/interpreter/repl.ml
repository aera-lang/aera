open Frontend
open Eval
open Value

let handle_result = 
    function (* syntatic sugar for immediate patterm matching as opposed to fun x -> match x with... *)
    | Error e ->
        print_endline e;
        None
    | Ok (value, env') ->
        print_value value;
        Some env'

let eval_repl repl_value env =
    match repl_value with
    | Frontend.Ast.ReplExpr expr ->
        (match handle_result (eval_expr expr env) with
        | Some env' -> env'
        | None -> env)

    | ReplStmt stmt ->
        (match handle_result (eval_stmt stmt env) with
        | Some env' -> env'
        | None -> env)

    | ReplItem item ->
        (match handle_result (eval_item item env) with
        | Some env' -> env'
        | None -> env)

let rec repl_loop env = 
    print_string "~> ";
    flush stdout;
    match input_line stdin with 
    | exception End_of_file -> print_endline "thanks for using aera!"
    | "quit" -> print_endline "thanks for using aera!"
    | line -> 
            let lex = Lexer.read_tokens (Lexer.init line) in
            let res = Parser.parse_repl (Parser.init lex.tokens lex.reporter) in
            let env =
                match res with
                | Error msg ->
                    print_endline msg;
                    env
                | Ok value ->
                    env |> eval_repl value
            in
            env |> repl_loop

let repl () =
    let env = [StringMap.empty] in (* initialize environment *)
    repl_loop env 

