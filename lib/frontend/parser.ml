open Ast
open Error
open Lexer
open Token
   
type parser = {
    tokens: token list;
    reporter: reporter;
    curr: int;
}

(* Helper Functions *)

let init tokens reporter = {
    tokens = tokens;
    reporter = reporter;
    curr = 0;
}

let peek par =
    match par.tokens with 
    | h :: _ -> h
    | [] -> failwith "the parser cannot be empty."

let peek_next par = 
    match par.tokens with
    | _ :: s :: _ -> s
    | _ -> failwith "the parser cannot be empty."

let is_at_end par =
    (peek par).kind = EOF

let advance par =
    match par.tokens with
    | _ :: t -> { par with tokens = t }
    | [] -> par

let next par = 
    (peek par, advance par)

let check kind par = 
    if is_at_end par then false
    else
        let token = peek par in token.kind = kind

(* Expression Helper Functions *)

let prefix_bp op =
    match op with 
    | Neg | Not -> 30

let assign_bp op = 
    match op with
    | EqAssign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | AndAssign | OrAssign | XorAssign | ShlAssign | ShrAssign 
    -> Some (2, 1) (* format = (left binding power, right binding power) *)

let binary_bp op =
    match op with
    | Or -> Some (3, 4)
    | And -> Some (5, 6)
    | BitOr -> Some (7, 8)
    | BitXor -> Some (9, 10)
    | BitAnd -> Some (11, 12)
    | Lt | Lte | Gt | Gte -> Some (13, 14)
    | Shl | Shr -> Some (15, 16)
    | Add | Sub -> Some (17, 18)
    | Mul | Div | Mod -> Some (19, 20)
    | _ -> None


let get_binary_op tok_kind par =
    match tok_kind with
    (* Arithmetic *)
    | Plus              -> Some Add
    | Minus             -> Some Sub
    | Star              -> Some Mul
    | Slash             -> Some Div
    | Percent           -> Some Mod
    (* Comparison *)
    | Less              -> Some Lt
    | LessEqual         -> Some Lte
    | Greater           -> Some Gt
    | GreaterEqual      -> Some Gte
    (* Logical *)
    | AmpAmp            -> Some And
    | PipePipe          -> Some Or
    (* Bitwise *)
    | Amp               -> Some BitAnd
    | Pipe              -> Some BitOr
    | Caret             -> Some BitXor
    | LessLess          -> Some Shl (* Shift left *)
    | GreaterGreater    -> Some Shr (* Shift right *)
    | _                 -> None

let get_assign_op tok_kind par =
    match tok_kind with
    | Equal                 -> Some EqAssign
    | PlusEqual             -> Some AddAssign
    | MinusEqual            -> Some SubAssign
    | StarEqual             -> Some MulAssign
    | SlashEqual            -> Some DivAssign
    | PercentEqual          -> Some ModAssign
    | AmpEqual              -> Some AndAssign
    | PipeEqual             -> Some OrAssign
    | CaretEqual            -> Some XorAssign
    | LessLessEqual         -> Some ShlAssign
    | GreaterGreaterEqual   -> Some ShrAssign
    | _                     -> None

(* Statement Helper Functions *)

let expect_identifier par =
    let (tok, par') = next par in
    match tok.kind with 
    | Identifier str        -> Ok(str, par')
    | _                     -> Error ("expected identifier name", tok, par)

let parse_type par =
    let (_, par') = next par in (* consume : token *)
    match expect_identifier par' with (* consume type -> identifier in parser *)
    | Error e -> Error e
    | Ok (typ, par'') -> Ok (typ, par'')

(* Pratt Parser *)
    
let rec expr_bp min_bp par =
    let (tok, par') = next par in 
    let lhs = match tok.kind with
    (* Literals *)
    | IntLiteral value      -> Ok (Literal (LitInt value), par')
    | FloatLiteral value    -> Ok (Literal (LitFloat value), par')
    | CharLiteral c         -> Ok (Literal (LitChar c), par')
    | StringLiteral str     -> Ok (Literal (LitString str), par')
    | True                  -> Ok (Literal (LitBool true), par')
    | False                 -> Ok (Literal (LitBool false), par')
    (* Identifier *)
    | Identifier str        -> Ok (Identifier str, par')
    (* Prefix Operators *)
    | Minus                 -> let rhs = par' |> expr_bp (prefix_bp Neg) in
                                    (match rhs with
                                    | Error e -> Error e
                                    | Ok (rhs', par'') -> Ok (Unary ({op = Neg; rhs = rhs'}), par''))
                                   
    | Exclaim               -> let rhs = par' |> expr_bp (prefix_bp Not) in
                                    (match rhs with
                                    | Error e -> Error e
                                    | Ok (rhs', par'') -> Ok (Unary ({op = Not; rhs = rhs'}), par''))
    (* Grouped Expression *)
    | LeftParen             -> (match par' |> expr_bp min_bp with 
                                    | Error e -> Error e
                                    | Ok (expr, par'') -> 
                                        let tok = peek par'' in 
                                        (match tok.kind with 
                                        | RightParen -> let (_, par''') = next par'' in 
                                                            Ok (Grouping expr, par''')
                                        | _ -> Error ("expected ')' to close grouping", tok, par'')))
    (* Invalid Token *)             
    | _                     -> let msg = Printf.sprintf "unsupported token in language: %s" tok.lexeme in
                Error (msg, tok, par') in 
    (* Infix Operators *)
    match lhs with 
    | Error e -> Error e
    | Ok (lhs', par'') -> loop lhs' min_bp par''

and loop lhs min_bp par =
    let tok = peek par in 
    match tok.kind with 
    | EOF   -> Ok (lhs, par)
    | kind  -> begin 
                match par |> get_binary_op kind with
                | None -> par |> parse_assign kind min_bp lhs (* try to parse assign expression *)
                | Some op    -> begin (* parse binary expression *)
                                    match binary_bp op with
                                    | None -> Ok (lhs, par)
                                    | Some (left_bp, right_bp) ->
                                        if left_bp < min_bp then Ok (lhs, par)
                                        else
                                            let (_, par') = next par in
                                            let res = par' |> expr_bp right_bp in
                                            begin 
                                                match res with
                                                | Error e -> Error e
                                                | Ok (rhs, par'') -> let lhs' = Binary ({lhs = lhs; op = op; rhs = rhs}) in
                                                                match par'' |> loop lhs' min_bp with
                                                                | Error e -> Error e
                                                                | Ok (lhs'', par''') -> Ok (lhs'', par''')
                                            end
                                end
                                            

                end

and parse_assign kind min_bp lhs par = 
    match par |> get_assign_op kind with 
    | None -> par |> parse_call kind min_bp lhs (* try to parse call *)
    | Some op -> begin
                    match assign_bp op with
                    | None -> Ok (lhs, par)
                    | Some (left_bp, right_bp) ->
                        if left_bp < min_bp then Ok (lhs, par)
                        else
                            let (_, par') = next par in
                            let res = par' |> expr_bp right_bp in
                            begin 
                                match res with
                                | Error e -> Error e
                                | Ok (rhs, par'') -> let lhs' = Assign ({lhs = lhs; op = op; rhs = rhs}) in
                                                        match par'' |> loop lhs' min_bp with
                                                        | Error e -> Error e
                                                        | Ok (lhs'', par''') -> Ok (lhs'', par''')
                                            end
                                end

and parse_args args par =
    let tok = peek par in 
    match tok.kind with 
    | RightParen -> let (_, par') = next par in (* consume ) token *)
                    Ok (List.rev args, par') (* reverse list to get correct order *)
    | _ -> match expr par with 
           | Error e -> Error e
           | Ok (arg, par') -> 
                let args' = (arg :: args) in
                let tok = peek par' in 
                (match tok.kind with 
                | Comma -> let (_, par'') = next par' in (* either more args to parse, or at RightParen OR invalid token *)
                            par'' |> parse_args args' 
                | RightParen -> par' |> parse_args args'  
                | _ -> Error ("expected ',' or ')' after parameter", tok, par')) 
        
and parse_call kind min_bp lhs par = 
    match (peek par).kind with 
    | LeftParen -> let (_, par') = next par in 
                    (match par' |> parse_args [] with 
                    | Error e -> Error e
                    | Ok (args, par'') -> Ok (Call {callee = lhs; args = args}, par''))
    | _ -> Ok (lhs, par) (* otherwise, early exit *)

(* Expressions *)

and expr par =
    let tok = peek par in 
    match tok.kind with
    | If -> let (_, par') = next par in par' |> if_expr
    | While -> let (_, par') = next par in par' |> while_expr 
    | Loop -> let (_, par') = next par in par' |> loop_expr
    | Break -> let (_, par') = next par in par' |> break_expr
    | Return -> let (_, par') = next par in par' |> return_expr
    | _ -> par |> expr_bp 0 (* expression without block *)

and if_expr par = 
    match expr par with  (* parse condition *)
    | Error e -> Error e
    | Ok (expr', par') -> 
        begin
            match block par' with 
            | Error e -> Error e
            | Ok (then_expr, par'') -> 
                begin
                    let tok = peek par'' in
                    match tok.kind with 
                    | Else -> let (_, par'') = next par'' in  (* consume else keyword *)
                        begin
                            match block par'' with (* parse else branch *)
                            | Error e -> Error e
                            | Ok (else_expr, par''') -> Ok (IfExpr {cond = expr'; then_branch = then_expr; else_branch = Some else_expr}, par''')
                        end
                    | _ -> Ok (IfExpr {cond = expr'; then_branch = then_expr; else_branch = None}, par'')
                end
        end

and while_expr par = 
    match expr par with  (* parse condition *)
    | Error e -> Error e
    | Ok (expr', par') -> 
        begin
            match block par' with 
            | Error e -> Error e
            | Ok (body, par'') -> Ok (WhileLoop {cond = expr'; body = body}, par'')
        end

and loop_expr par = 
    match block par with 
    | Error e -> Error e
    | Ok (expr', par') -> Ok (InfiniteLoop expr', par')

and break_expr par = 
    match (peek par).kind with 
    | Else | RightBrace | EOF -> Ok (BreakExpr None, par)
    | _ -> (* break with expression *)
        match expr par with 
        | Error e -> Error e
        | Ok (expr', par') ->  Ok (BreakExpr (Some expr'), par')

and return_expr par =
    match (peek par).kind with 
    | Else | RightBrace | EOF -> Ok (ReturnExpr None, par)
    | _ ->
        match expr par with 
        | Error e -> Error e
        | Ok (expr', par') -> Ok (ReturnExpr (Some expr'), par')
    
(* Statements *)

and let_stmt par =
    match expect_identifier par with (* advance to the next token *)
    | Error e -> Error e
    | Ok (name, par') -> 
        begin
            match (peek par').kind with (* type annotation is optional *)
            | Colon ->  begin 
                            match parse_type par' with
                            | Error e -> Error e
                            | Ok (typ, par'') -> begin 
                                                    match (peek par'').kind with 
                                                    | Equal -> begin
                                                                let (_, par''') = next par'' in (* consume = token *)
                                                                match expr par''' with 
                                                                | Error e -> Error e
                                                                | Ok (expr', par'''') -> 
                                                                    Ok (LetStmt { name = name; typ = Some typ; expr = Some expr' }, par'''')
                                                                end
                                                    | _ -> Ok (LetStmt { name = name; typ = Some typ; expr = None }, par'')
                                                end
                        end
            | Equal -> begin
                            let (_, par'') = next par' in (* consume = token *)
                            match expr par'' with 
                            | Error e -> Error e
                            | Ok (expr', par''') -> Ok (LetStmt { name = name; typ = None; expr = Some expr' }, par''')
                        end
            | _ -> Ok (LetStmt { name = name; typ = None; expr = None }, par')

        end
                        
and const_stmt par = match expect_identifier par with (* advance to the next token *)
    | Error e -> Error e
    | Ok (name, par') -> 
        let tok = (peek par') in 
        begin 
            match tok.kind with 
            | Colon -> begin 
                            match parse_type par' with
                            | Error e -> Error e
                            | Ok (typ, par'') -> let tok' = (peek par'') in
                                                    begin 
                                                        match tok'.kind with 
                                                        | Equal -> begin
                                                                    let (_, par''') = next par'' in (* consume = token *)
                                                                    match expr par''' with 
                                                                    | Error e -> Error e
                                                                    | Ok (expr', par'''') -> 
                                                                        Ok (ConstStmt { name = name; typ = Some typ; expr = expr' }, par''')
                                                                    end
                                                        | _ -> Error ("expected expression after '='", tok', par'')
                                                    end
                        end
            | Equal -> begin
                            let (_, par'') = next par' in (* consume = token *)
                            match expr par'' with 
                            | Error e -> Error e
                            | Ok (expr', par''') -> Ok (ConstStmt { name = name; typ = None; expr = expr' }, par''')
                        end
            | _ -> Error ("expected '=' for constant declaration", tok, par')
        end

and stmt par = 
    let tok = peek par in 
    match tok.kind with
    | Let -> (match let_stmt par with
            | Error e -> Error e
            | Ok (let_stmt, par') -> Ok(let_stmt, par'))
    | Const -> (match const_stmt par with
            | Error e -> Error e
            | Ok (const_stmt, par') -> Ok(const_stmt, par'))
    | _ -> Error ("expected a statement", tok, par)  (* this SHOULDN'T happen if we check for keywords before calling stmt -> 
                                                            if I forgot to add a stmt, then this error will remind me *)

(* Block Expressions *)                                              

and parse_block stmts par = 
    let tok = peek par in 
    match tok.kind with 
    | Let | Const -> (match stmt par with (* continue to parse statements *)
                        | Error e -> Error e
                        | Ok (stmt, par) -> par |> parse_block (stmt :: stmts))
    | _ -> match expr par with (* other, parse expression *)
        | Error e -> Error e
        | Ok (expr, par') -> 
            let tok = peek par' in 
            match tok.kind with 
            | RightBrace -> let (_, par'') = next par' in Ok (Block ({stmts = stmts; expr = expr}), par'')
            | _ -> Error ("expected } after block expression", tok, par')

and block par =
    let tok = peek par in 
    match tok.kind with 
    | LeftBrace -> let (_, par') = next par in par' |> parse_block []
    | _ -> Error ("expected { before block expression", tok, par)

(* Items *)

and parse_fn_param par = 
    match expect_identifier par with 
    | Error e -> Error e
    | Ok (name, par') ->
        begin
            match (peek par').kind with 
            | Colon ->  let (_, par'') = next par' in
                        begin
                            match expect_identifier par'' with
                            | Error e -> Error e
                            | Ok (typ, par''') -> Ok ((name, Some typ), par''')
                       end
            | _ -> Ok ((name, None), par')
        end

and parse_fn_params params par = 
    let tok = peek par in 
    match tok.kind with 
    | Identifier _ -> begin (* more params to parse *)
                        match parse_fn_param par with 
                        | Error e -> Error e
                        | Ok ((param, typ), par') -> 
                                let params' = (param, typ) :: params in
                                let tok = peek par' in 
                                (match tok.kind with 
                                | Comma -> let (_, par'') = next par' in (* either more params to parse, or at RightParen OR invalid token *)
                                            par'' |> parse_fn_params params'
                                | RightParen -> par' |> parse_fn_params params'  
                                | _ -> Error ("expected ',' or ')' after parameter", tok, par'))
                              
                      end
    | RightParen -> let (_, par') = next par in (* consume ) token *)
                    Ok (List.rev params, par') (* reverse list to get correct order *)
    | _ -> Error ("expected ')' after params", tok, par)

and parse_return_type par = 
    let (_, par') = next par in 
    match expect_identifier par' with (* TODO: Update AST to parse type annotations. For now, leaving them as identifiers is fine. *)
    | Error e -> Error e
    | Ok (typ, par'') -> Ok (typ, par'')

and fn_item par = 
    match expect_identifier par with
    | Error e -> Error e
    | Ok (name, par') -> 
        let (tok, par') = next par' in 
        (match tok.kind with
        | LeftParen -> 
            begin
                match parse_fn_params [] par' with 
                | Error e -> Error e
                | Ok (params, par') -> 
                    begin
                        match (peek par').kind with
                        | MinusGreater -> (match parse_return_type par' with
                                           | Error e -> Error e
                                           | Ok (typ, par'') -> 
                                                (match block par'' with 
                                                | Error e -> Error e
                                                | Ok (body, par''') -> Ok (FnItem {name = name; params = params; return_type = Some typ; body = body} ,par''')))             
                                        | _ -> (match block par' with 
                                            | Error e -> Error e
                                            | Ok (body, par'') -> Ok (FnItem {name = name; params = params; return_type = None; body = body} ,par''))
                                    end
                    end
              
        | _ -> Error ("expected '(' after function name", tok, par'))

and parse_field_item par =
    match expect_identifier par with 
    | Error e -> Error e
    | Ok (name, par') ->
        begin 
            let tok = peek par' in
            match tok.kind with 
            | Colon ->  let (_, par'') = next par' in 
                        begin 
                            match expect_identifier par'' with (* TODO: Change to actually parsing the type instead of parsing as an identifier *)
                            | Error e -> Error e
                            | Ok (typ, par''') -> Ok ((name, typ), par''')
                        end
            | _     -> Error ("expected type after struct field.", tok, par')
        end

and parse_field_items fields par =
    let tok = peek par in 
    match tok.kind with 
    | Identifier _ -> begin (* more fields to parse *)
                        match parse_field_item par with
                        | Error e -> Error e
                        | Ok ((name, typ), par') ->
                                let fields' = (name, typ) :: fields in 
                                let tok = peek par' in
                                (match tok.kind with 
                                | Identifier _ -> par' |> parse_field_items fields'
                                | RightBrace -> par' |> parse_field_items fields'
                                | _ -> Error ("expected '}' after field item", tok, par'))
                        end
    | RightBrace -> let (_, par') = next par in (* consume } token *)
                    Ok (List.rev fields, par')
    | _ -> Error ("expected '}' after field items", tok, par)

and struct_item par =
    match expect_identifier par with
        | Error e -> Error e
        | Ok (name, par') -> 
        let (tok, par'') = next par' in 
            (match tok.kind with
            | LeftBrace -> 
                begin 
                    let (_, par'') = next par' in
                    match par'' |> parse_field_items [] with 
                    | Error e -> Error e
                    | Ok (fields, par') -> Ok (StructItem {name = name; fields = fields}, par')
                end
            | _ -> Error ("expected '{' after struct name", tok, par'))

and parse_variant_case_helper fields par = 
    let tok = peek par in 
    match tok.kind with 
    | Identifier _ -> begin (* more fields to parse *)
                        match parse_field_item par with 
                        | Error e -> Error e 
                        | Ok ((name, typ), par') -> 
                                let fields' = (name, typ) :: fields in 
                                let tok = peek par' in
                                (match tok.kind with
                                | Comma -> let (_, par'') = next par' in 
                                            par'' |> parse_variant_case_helper fields'
                                | RightParen -> par' |> parse_variant_case_helper fields'
                                | _ -> Error ("expected ',' or ')' after field", tok, par'))
                        end
    | RightParen -> let (_, par') = next par in 
                    Ok (List.rev fields, par')
    | _ -> Error ("expected ')' after fields", tok, par)

and parse_variant_case par =
    match expect_identifier par with (* variant case name *)
    | Error e -> Error e
    | Ok (case_name, par') -> 
             let _ = Printf.printf "parse_variant_case: name = %s, next token = %s\n" case_name (peek par').lexeme in
        begin 
            let tok = peek par' in
            match tok.kind with 
            | LeftParen -> let (_, par'') = next par' in
                            (match par'' |> parse_variant_case_helper [] with 
                            | Error e -> Error e
                            | Ok (fields, par''') -> Ok (case_name, fields, par'''))
            | Identifier _ -> Ok (case_name, [], par') (* indicates that variant case holds no data -> we have another case to look at *)
            | RightBrace -> Ok (case_name, [], par')
            | _ -> Error ("expected '(' after variant case name or nothing for a variant that holds no data", tok, par')
        end

and parse_variant_cases cases par =
    let _ = Printf.printf "parse_variant_cases: token = %s\n" (peek par).lexeme in

    let tok = peek par in 
    match tok.kind with 
    | Identifier _ -> begin
                        match parse_variant_case par with 
                        | Error e -> print_endline "error"; Error e
                        | Ok (case_name, fields, par') ->
                            let cases' = (case_name, fields) :: cases in
                            let tok = peek par' in 
                                let _ = Printf.printf "parse_variant_cases: after case, token = %s\n" tok.lexeme in
                            (match tok.kind with 
                            | Identifier _ -> par' |> parse_variant_cases cases' (* more variant cases to handle *)
                            | RightBrace -> par' |> parse_variant_cases cases'
                            | _ -> Error ("expected variant case name or '}'", tok ,par'))
                        end
    | RightBrace -> let (_, par') = next par in (* consume } token *)
                    Ok (List.rev cases, par')
    | _ -> Error ("expected '}' after variant cases", tok, par)

and variant_item par =
        let _ = Printf.printf "variant_item: token = %s\n" (peek par).lexeme in
    match expect_identifier par with
        | Error e -> Error e
        | Ok (name, par') -> (* variant name *)
            let _ = Printf.printf "variant_item: name = %s, next token = %s\n" name (peek par').lexeme in
            let (tok, par'') = next par' in 
                (match tok.kind with
                | LeftBrace -> 
                    begin 
                        match par'' |> parse_variant_cases [] with 
                        | Error e -> Error e
                        | Ok (cases', par''') -> Ok (VariantItem {name = name; cases = cases'}, par''')
                    end
                | _ -> Error ("expected '{' after struct name", tok, par'))

and item par = 
    let tok = peek par in
    match tok.kind with 
    | Fn -> let (_, par') = next par in 
                (match par' |> fn_item with 
                | Error e -> Error e
                | Ok (fn_item, par'') -> Ok (fn_item, par''))
    | Struct -> let (_, par') = next par in 
                (match par' |> struct_item with 
                | Error e -> Error e
                | Ok (struct_item, par'') -> Ok (struct_item, par''))
    | Variant -> let (_, par') = next par in 
                (match par' |> variant_item with 
                | Error e -> Error e
                | Ok (variant_item, par'') -> Ok (variant_item, par''))
    | _ -> Error ("expected an item", tok, par)  (* this SHOULDN'T happen if we check for keywords before calling item -> 
                                                            if I forgot to add a item, then this error will remind me *)

(* Parse Function *)

let rec parse_helper items par =
    if par |> is_at_end then        
        (items, par)
    else
        let tok = peek par in 
        match tok.kind with
        | Fn | Struct | Variant -> (match par |> item with (* ensures that in item, we CANNOT reach wildcard case *)
                | Error (msg, tok', par') -> (items, { par' with reporter = add_error tok'.pos msg par'.reporter }) (* needs better error handling *)
                | Ok (item, par') -> par' |> parse_helper (item :: items))
        | _ -> (items, { par with reporter = add_error tok.pos "expected an item: fn, struct, variant" par.reporter }) 

let parse par =
    let (items, par') = par |> parse_helper [] in 
    ({ items = items }, par')    

(* Parse Function for REPL *)

let parse_repl par = 
    let tok = peek par in 
    match tok.kind with 
    | Fn | Struct | Variant -> (match par |> item with 
                                | Error (msg, _, _) -> Error msg
                                | Ok (item, _) -> Ok (ReplItem item))
    | Let | Const -> (match par |> stmt with 
                    | Error (msg, _, _) -> Error msg
                    | Ok (stmt, _) -> Ok (ReplStmt stmt))
    | _ -> (match par |> expr with 
            | Error (msg, _, _) -> Error msg
            | Ok(expr, _) -> Ok (ReplExpr expr))