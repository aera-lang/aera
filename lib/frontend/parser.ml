open Ast
open Reporter
open Lexer
open Token
open Op
open Source
open Typ

type t = {
    source: Source.t;
    tokens: Token.t list;
    reporter: Reporter.t;
}

(* -------------------- Helper Functions -------------------- *)

let create tok rep src = {
    source = src;
    tokens = tok;
    reporter = rep;
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
    if is_at_end par then 
        false
    else 
        let token = peek par in token.kind = kind

let lexeme tok par = 
    par.source |> extract tok.span

let char_of_string s = 
    if String.length s = 1 then 
        Some s.[0]
    else
        None

let is_item_keyword kind =
    match kind with 
    | Fn | Struct | Variant -> true 
    | _                     -> false

let is_stmt_keyword kind = 
    match kind with 
    | Let | Var -> true
    | _         -> is_item_keyword kind

(* -------------------- Error Recovery / Handling -------------------- *)

let report_error msg tok par = 
    { par with reporter = add_error par.source.filename tok.span msg None par.reporter }

(* -------------------- Literals --------------------*)

let parse_int tok par =
    match Int64.of_string_opt (lexeme tok par) with
    | Some value    -> (Literal (IntLiteral value), par)
    | None          -> let par' = report_error "could not parse integer" tok par 
                       in (ErrorExpr tok.span, par')

let parse_float tok par =
    match float_of_string_opt (lexeme tok par) with
    | Some value    -> (Literal (FloatLiteral value), par)
    | None          -> let par' = report_error "could not parse float" tok par 
                       in (ErrorExpr tok.span, par')

let parse_char tok par =
    match char_of_string (lexeme tok par) with
    | Some value    -> (Literal (CharLiteral value), par)
    | None          -> let par' = report_error "could not parse character" tok par 
                       in (ErrorExpr tok.span, par')

let parse_string tok par =
    let value = lexeme tok par in (Literal (StringLiteral value), par)

let parse_bool value par = (Literal (BoolLiteral value), par)

(* -------------------- Identifier -------------------- *)

let parse_ident tok par =
    let value = lexeme tok par in (Ident value, par)

(* -------------------- Unary - Prefix -------------------- *)

let rec parse_prefix op par =
    let (rhs, par') = par |> expr_bp (prefix_bp op) in (Unary ({op = op; rhs = rhs}), par')

(* -------------------- Grouping & Tuples -------------------- *)

and parse_expr_list closing_kind args par =
    let tok = peek par in 
    if tok.kind = closing_kind then 
        let (_, par') = next par in (List.rev args, par')
    else
        let (arg, par') = par |> expr_bp 0 (* change to expr *) in 
        let args' = (arg :: args) in 
        let tok = peek par' in 
        if tok.kind = Comma then 
             let (_, par'') = next par' in par'' |> parse_expr_list closing_kind args'
        else if tok.kind = closing_kind then 
            par' |> parse_expr_list closing_kind args'
        else
            let par'' = report_error 
            (Printf.sprintf "expected ',' or '%s' after parameter" (tok_to_string closing_kind)) 
            tok par' 
            in (List.rev args, par'')

and parse_paren_expr par =
    let (first, par') = par |> expr_bp 0 in 
    let tok = peek par' in 
    match tok.kind with 
    | RightParen -> let par'' = advance par' in (Grouping first, par'') (* parse grouping first *)
    | Comma      -> let par'' = advance par' in par'' |> parse_tuple_expr first (* parse tuple *)
    | _          -> let par'' = report_error "expected ')' to close grouping" tok par' 
                    in (Grouping first, par'') (* still have a valid grouping, just not closed properly *)
and parse_tuple_expr first par =
    let (rest, par') = par |> parse_expr_list RightParen [] in (TupleExpr (first ::rest), par')

(* -------------------- Unrecognized Tokens -------------------- *)

and parse_unrecognized tok par =
    let par = 
        if tok.kind <> Illegal then 
            let msg = Printf.sprintf "unsupported token in language: %s" (lexeme tok par) in
            par |> report_error msg tok
        else par
        in 
            let par' = advance par 
            in (ErrorExpr tok.span, par')

(* -------------------- Call Expressions -------------------- *)

and parse_call lhs par =
    let par' = advance par in 
    let (args, par'') = par' |> parse_args_list [] in
    (Call {callee = lhs; args = args}, par'')

and parse_arg par =
    let tok = peek par in 
    let next_tok = peek_next par in 
    match tok.kind, next_tok.kind with 
    | Identifier, Colon  -> 
        let name = lexeme tok par in 
        let par' = advance par in (* consume identifier *)
        let par'' = advance par' in (* consume : *)
        let (value, par''') = par'' |> expr_bp 0 (* change to expr *) in
                                (Named {name = name; value = value}, par''')
    | _ ->
        let (value, par') = par |> expr_bp 0 (* change to expr *) in
        (Positional value, par')

and parse_args_list args par =
    let tok = peek par in 
    if tok.kind = RightParen then 
        let par' = advance par in (List.rev args, par')
    else
        let (arg, par') = par |> parse_arg in 
        let args' = arg :: args in 
        let tok = peek par' in 
        if tok.kind = Comma then 
            let par'' = advance par' in par'' |> parse_args_list args'
        else if tok.kind = RightParen then 
            par' |> parse_args_list args'
        else
            let par'' = report_error  "expected ',' or ')' after parameter" tok par' 
            in (List.rev args, par'')
                           
(* -------------------- Binary & Assign Expressions -------------------- *)

and parse_binary op lhs min_bp par =
    match binary_bp op with 
    | None -> (lhs, par)
    | Some (left_bp, right_bp) ->
        if left_bp < min_bp then (lhs, par)
        else
            let par' = advance par in
            let (rhs, par'') = par' |> expr_bp right_bp in
            let lhs' = Binary ({lhs = lhs; op = op; rhs}) in
            let (lhs'', par''') = par'' |> loop lhs' min_bp in 
            (lhs'', par''')

and parse_assign op lhs min_bp par =
    match assign_bp op with 
    | None -> (lhs, par)
    | Some (left_bp, right_bp) ->
        if left_bp < min_bp then (lhs, par)
        else
            let par'= advance par in
            let (rhs, par'') = par' |> expr_bp right_bp in
            let lhs' = Assign ({lhs = lhs; op = op; rhs}) in
            let (lhs'', par''') = par'' |> loop lhs' min_bp in 
            (lhs'', par''')

(* -------------------- Array Expression -------------------- *)

and parse_array_expr par =
    let par' = advance par in (* consume '[' token *)
    let (rest, par'') = par' |> parse_expr_list RightBracket [] in (ArrayExpr rest, par'')

(* -------------------- Get Identifier -------------------- *)

and get_identifier par =
    let tok = peek par in 
    match tok.kind with
    | Identifier     -> let value = lexeme tok par in 
                        let (_, par') = next par in 
                        (value, par')     
    | _              -> 
        let par' = match tok.kind with 
        | Illegal       -> let (_, par'') = next par in par'' 
        | _             -> par |> report_error "expected identifier" tok
        in
        ("<missing>", par')

(* -------------------- Expression Without Block -------------------- *)

and expr_bp min_bp par = 
    let (tok, par') = next par in 
    let (lhs, par'') = match tok.kind with 
    (* Literals *)
    | IntLiteral                -> par' |> parse_int tok
    | FloatLiteral              -> par' |> parse_float tok
    | CharLiteral               -> par' |> parse_char tok
    | StringLiteral             -> par' |> parse_string tok
    | True                      -> par' |> parse_bool true
    | False                     -> par' |> parse_bool false
    | Identifier                -> par' |> parse_ident tok
    (* Prefix Operators *)
    | Minus                     -> par' |> parse_prefix Neg
    | Exclaim                   -> par' |> parse_prefix Not
    (* Grouped Expression *)
    | LeftParen                 -> par' |> parse_paren_expr
    (* Invalid Token *)
    | _                         -> par' |> parse_unrecognized tok                       
    in
    (* Infix Operators*)
    par'' |> loop lhs min_bp 

and loop lhs min_bp par = 
    let tok = peek par in 
    match tok.kind with 
    | EOF                               -> (lhs, par)
    (* Call Expressions*)
    | LeftParen                         -> par |> parse_call lhs
    (* Binary / Assign Operators *)
    | op when is_binary_op tok.kind     -> par |> parse_binary (to_binary_op tok.kind) lhs min_bp
    | op when is_assign_op tok.kind     -> par |> parse_assign (to_assign_op tok.kind) lhs min_bp
    (* Array *)
    | LeftBracket                       -> par |> parse_array_expr
    | _                                 -> (lhs, par)

(* -------------------- Expression With Block -------------------- *)

and expr par =
    let tok = peek par in 
    match tok.kind with
    | LeftBrace -> let par' = advance par in par' |> block
    | If        -> let par' = advance par in par' |> if_expr
    | While     -> let par' = advance par in par' |> while_expr 
    | Loop      -> let par' = advance par in par' |> loop_expr
    | Match     -> let par' = advance par in par' |> match_expr
    | Break     -> let par' = advance par in par' |> break_expr
    | Return    -> let par' = advance par in par' |> return_expr
    | _         -> par |> expr_bp 0 (* expression without block *)

(* -------------------- Block -------------------- *)

and parse_block stmts par =
    let tok = peek par in
    match tok.kind with 
    | RightBrace ->
        let par' = advance par in 
        begin
            match stmts with 
            | ExprStmt e :: rest -> (List.rev rest, Some e, par') 
            | _                  -> (List.rev stmts, None, par')
        end
    | _ -> 
        let (stmt_, par') = par |> stmt in 
        par' |> parse_block (stmt_ :: stmts)        
    
and block par =
    let tok = peek par in 
    match tok.kind with 
    | LeftBrace  -> let par' = advance par in 
                    let (stmts, expr_opt, par'') = par' |> parse_block [] in 
                    (Block {stmts = stmts; expr = expr_opt}, par'')
    | _  -> let par' = report_error "expected { before block expression" tok par in 
            (ErrorExpr tok.span, par') 

(* -------------------- If Expression -------------------- *)

and if_expr par = 
    let (cond, par') = expr par in 
    let (then_block, par'') = block par' in 
    let tok = peek par'' in 
    match tok.kind with 
    | Else -> 
        let par''' = advance par'' in 
        let (else_block, par'''') = block par''' in 
        (IfExpr {cond = cond; then_branch = then_block; else_branch = Some else_block}, par'''')
    | _ -> (IfExpr {cond = cond; then_branch = then_block; else_branch = None}, par'')

(* -------------------- Loop Expressions -------------------- *)

and while_expr par = 
    let (cond, par') = expr par in 
    let (body, par'') = block par' in 
    (WhileLoop {cond = cond; body = body}, par'')

and loop_expr par = 
    let (body, par') = block par in
    (InfiniteLoop body, par')

(* -------------------- Match Expression -------------------- *)

and match_expr par =
    let (expr_, par') = expr par in 
    let tok = peek par' in 
    match tok.kind with 
    | LeftBrace -> let par'' = advance par' in par'' |> match_cases [] 
    | _ -> let par' = report_error "expected { before match expression" tok par in 
            (ErrorExpr tok.span, par') 


and match_cases cases par = []

and match_case par = ()

(* -------------------- Patterns -------------------- *)


and pattern par =
    let (tok, par') = next par in 
    match tok.kind with 
    (* Literals *)
    | IntLiteral -> 
        let (value, par'') = par' |> parse_int_pattern tok in 
        (value, par'')
    | FloatLiteral -> 
        let (value, par'') = par' |> parse_float_pattern tok in 
        (value, par'')
    | CharLiteral -> 
        let (value, par'') = par' |> parse_char_pattern tok in 
        (value, par'')
    | StringLiteral ->
        let (value, par'') = par' |> parse_string_pattern tok in 
        (value, par'')
    | True ->
        let (value, par'') = par' |> parse_bool_pattern true in 
        (value, par'')
    | False ->
        let (value, par'') = par' |> parse_bool_pattern false in 
        (value, par'')
    (* Identifier *)
    | Identifier -> (* have to check for variant here *)
        let tok' = peek par' in 
        begin 
            match tok'.kind with 
            | LeftBrace -> 
                let par'' = advance par' in par'' |> parse_variant_pattern
            LeftParen -> 
                let par'' = advance par' in par'' |> parse_struct_pattern
            | _ ->  let (value, par'') = par' |> parse_ident_pattern tok in 
                    (value, par'')
        end
    (* Wildcard *)
    | Underscore ->
        (WildcardPattern, par')
    (* Array Pattern *)
    | LeftBracket ->
        let (value, par'') = par' |> parse_array_pattern in 
        (value, par'')
    (* Tuple Pattern *)
    | LeftBrace ->
        let (value, par'') = par' |> parse_array_pattern in 
        (value, par'')
    (* Invalid Pattern*)
    | _ -> let par' = report_error "expected pattern in match case" tok par 
                    in (ErrorPattern tok.span, par')

(* -------------------- Literal Patterns -------------------- *)

and parse_int_pattern tok par =
    match Int64.of_string_opt (lexeme tok par) with
    | Some value    -> (LiteralPattern (IntLiteral value), par)
    | None          -> let par' = report_error "could not parse integer" tok par 
                        in (ErrorPattern tok.span, par')

and parse_float_pattern tok par = 
    match float_of_string_opt (lexeme tok par) with
    | Some value    -> (LiteralPattern (FloatLiteral value), par)
    | None          -> let par' = report_error "could not parse float" tok par 
                        in (ErrorPattern tok.span, par')

and parse_char_pattern tok par =
    match char_of_string (lexeme tok par) with
    | Some value    -> (LiteralPattern (CharLiteral value), par)
    | None          -> let par' = report_error "could not parse character" tok par 
                        in (ErrorPattern tok.span, par')
and parse_string_pattern tok par =
    let value = lexeme tok par in (LiteralPattern (StringLiteral value), par)

and parse_bool_pattern value par = (LiteralPattern (BoolLiteral value), par)

(* -------------------- Identifier Pattern -------------------- *)

and parse_ident_pattern tok par =
    let value = lexeme tok par in (IdentPattern value, par)

(* 

match_expression = "match" expression "{" match_case { match_case } "}" ;
match_case = pattern "=>" expression ;

pattern = literal
        | identifier
        | underscore
        | array_pattern
        | tuple_pattern
        | struct_pattern
        | variant_pattern ;

pattern_list = pattern { ","  pattern } ;

array_pattern = "[" pattern_list "]" ;
tuple_pattern = "(" tuple_pattern_list ")" ;
tuple_pattern_list = pattern "," [ pattern_list ] ;

struct_pattern = identifier "{" [ field_pattern_list ] "}" ;
field_pattern_list = field_pattern { "," field_pattern } ;
field_pattern = identifier [ ":" pattern ] ;

variant_pattern = identifier "(" [ pattern_list ] ")" ;


*)

(* -------------------- Break Expression -------------------- *)

and break_expr par = 
    match (peek par).kind with 
    | RightBrace | EOF -> (BreakExpr None, par)
    | _ ->
        let (value, par') = expr par in 
        (BreakExpr (Some value), par')

(* -------------------- Return Expression -------------------- *)

and return_expr par = 
    match (peek par).kind with 
    | RightBrace | EOF -> (ReturnExpr None, par)
    | _ -> 
        let (value, par') = expr par in 
        (ReturnExpr (Some value), par')

(* -------------------- Types -------------------- *)

and parse_typ par = ()

and parse_fixed_array_typ par = ()

and parse_tuple_type par = ()

(* -------------------- Statements -------------------- *)

and let_stmt par = 
    let (name, par') = get_identifier par in 
    let tok = peek par' in 
    let next_tok = peek_next par' in 
    match tok.kind, next_tok.kind with 
    | Identifier, Colon -> 
        (* get type *)
        let typ = lexeme tok par' in  (* NEEDS TO BE THE TYPE *)
        let (_, par'') = next par' in (* consume identifier *)
        let (_, par''') = next par'' in (* consume : *)
        let (value, par'''') = par''' |> expr_bp 0 (* change to expr *) in
        (LetStmt {name = name; typ = Some typ; expr = value}, par'''')
    | Equal ->
        let (_, par'') = next par' in (* consume = *)
        let (value, par'''') = par''' |> expr_bp 0 (* change to expr *) in
        (LetStmt {name = name; typ = None; expr = value}, par'''')       

(* var stmt would be similar *)
and var_stmt par = ()


and stmt par = 
    let tok = peek par in 
    match tok.kind with 
    (*| Fn | Struct | Variant -> expr par (* CHANGE TO ITEM ACTUALLY *)
    | Let    -> 
        let (_, par') = next par in 
        let (let_stmt, par'') = let_stmt in 
        (let_stmt, par'')
    | Var    -> 
        let (_, par') = next par in 
        let (var_stmt, par'') = var_stmt in 
        (var_stmt, par'') *)
    | _      -> 
        let (expr_stmt, par') = expr par in 
        (ExprStmt expr_stmt, par')


(* -------------------- Items -------------------- *)