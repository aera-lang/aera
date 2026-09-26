open Ast
open Reporter
open Lexer
open Token
open Op
open Source
open Typ
open Utils

type t = {
    source: Source.t;
    tokens: Token.t list;
    reporter: Reporter.t;
}

(* -------------------- Helper Functions -------------------- *)

let create src tok rep = {
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
    let raw = lexeme tok par in 
    let inner = String.sub raw 1 (String.length raw - 2) in
    match decode_char inner with 
    | Ok char        -> (Literal (CharLiteral char), par)
    | Error msg      -> let par' = report_error msg tok par 
                        in (ErrorExpr tok.span, par')
let parse_string tok par =
    let raw = lexeme tok par in 
    let inner = String.sub raw 1 (String.length raw - 2) in
    match decode_string inner with 
    | Ok str        -> (Literal (StringLiteral str), par)
    | Error msg      -> let par' = report_error msg tok par 
                        in (ErrorExpr tok.span, par')
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
        let par' = advance par in (List.rev args, par')
    else
        let (arg, par') = par |> expr (* change to expr *) in 
        let args' = (arg :: args) in 
        let tok' = peek par' in 

        if tok'.kind = Comma then 
            let par'' = advance par' in 
            par'' |> parse_expr_list closing_kind args'
        else if tok'.kind = closing_kind then 
            par' |> parse_expr_list closing_kind args'
        else
            let par'' = report_error 
            (Printf.sprintf "expected ',' or '%s' after parameter" (tok_to_string closing_kind)) 
            tok' par' 
            in (List.rev args', par'')

and parse_paren_expr par =
    let (first, par') = par |> expr_bp 0 in 
    let tok = peek par' in 
    match tok.kind with 
    | RightParen -> let par'' = advance par' in (Grouping first, par'') (* parse grouping first *)
    | Comma      -> let par'' = advance par' in par'' |> parse_tuple_expr first (* parse tuple *)
    | _          -> let par'' = report_error "expected ')' to close grouping" tok par' 
                    in (Grouping first, par'') (* still have a valid grouping, just not closed properly *)
and parse_tuple_expr first par =
    let (rest, par') = par |> parse_expr_list RightParen [] in 
    (TupleExpr (first ::rest), par')

(* -------------------- Unrecognized Tokens -------------------- *)

and parse_unrecognized tok par =
    let par = 
        if tok.kind <> Illegal then 
            let msg = Printf.sprintf "unsupported token in language: %s" (lexeme tok par) in
            par |> report_error msg tok
        else par
    in (ErrorExpr tok.span, (advance par))

(* -------------------- Index Expression -------------------- *)

and parse_index lhs min_bp par =
    let bp = postfix_bp Subscript in 
    if bp < min_bp then (lhs, par) 
    else
        let par' = advance par in (* consume [ *)
        let (index, par'') = par' |> expr_bp 0 in 
        let tok = peek par'' in 
        let par''' =
            match tok.kind with 
            | RightBracket -> advance par'' 
            | _ -> report_error "expected ']' to close index expression" tok par'' 
        in
        par''' |> loop (Index { target = lhs; index = index }) min_bp

(* -------------------- Field / Tuple Access Expression -------------------- *)

and parse_tuple_int tok par =
    match Int64.of_string_opt (lexeme tok par) with
    | Some value    -> Ok (value, advance par)
    | None          -> Error ("could not parse tuple index integer")

and parse_field_or_tuple_access lhs min_bp par = 
    let bp = postfix_bp FieldAccess in 
    if bp < min_bp then (lhs, par) 
    else
        let par' = advance par in (* consume . *)
        let tok = peek par' in 
        if tok.kind = IntLiteral then 
            match parse_tuple_int tok par' with 
            | Ok (value, par'') -> 
                    par'' |> loop (TupleAccess { target = lhs; index = value }) min_bp
            | Error msg -> 
                let par'' = report_error msg tok par' in
                (ErrorExpr tok.span, advance par'')
        else
            let (name, par'') = get_identifier par' in 
            par'' |> loop (FieldAccess { target = lhs; field = name }) min_bp

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
        let par'' = advance par' in (* consume ':' *)
        let (value, par''') = par'' |> expr in
                                (Named {name = name; value = value}, par''')
    | _ ->
        let (value, par') = par |> expr in
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
            in (List.rev args', par'')
                           
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
    let (rest, par') = par |> parse_expr_list RightBracket [] in (ArrayExpr rest, par')

(* -------------------- Closure Expression -------------------- *)

and parse_closure par = 
    let tok = peek par in 
    match tok.kind with 
    | LeftParen -> 
        let (params, par') = (advance par) |> parse_params_list ~type_required:false [] in 
        par' |> parse_closure_body params
    | _ -> 
        let par' = report_error "expected '(' to start closure parameters" tok par in 
        (ErrorExpr tok.span, advance par')

and parse_closure_body params par =
    let tok = peek par in 
    match tok.kind with 
    | EqualGreater ->
        let par' = advance par in (* consume '=>' *)
        let (expr, par'') = expr par' in 
        (Closure { params = params; body = expr; }, par'')
    | _ -> 
        let par' = report_error "expected '=>' to follow after closure parameters" tok par in 
        (Closure { params = params; body = ErrorExpr tok.span }, par')

(* -------------------- Get Identifier -------------------- *)

and get_identifier par =
    let tok = peek par in 
    match tok.kind with
    | Identifier     -> let value = lexeme tok par in 
                        let par' = advance par in 
                        (value, par')     
    | _              -> 
        let par' = match tok.kind with 
        | Illegal       -> let par'' = advance par in par'' 
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
    (* Array Expression *)
    | LeftBracket               -> par' |> parse_array_expr
    (* Closure Expression *)    
    | Fn                        -> par' |> parse_closure
    (* Invalid Token *)
    | _                         -> par' |> parse_unrecognized tok                       
    in
    (* Infix Operators*)
    par'' |> loop lhs min_bp 

and loop lhs min_bp par = 
    let tok = peek par in 
    match tok.kind with 
    | EOF                              -> (lhs, par)
    (* Index *)
    | LeftBracket                      -> par |> parse_index lhs min_bp
    (* Field Access OR Tuple Access *)
    | Period                           -> par |> parse_field_or_tuple_access lhs min_bp
    (* Call Expression *)
    | LeftParen                        -> par |> parse_call lhs
    (* Binary / Assign Operators *)
    | _ when is_binary_op tok.kind     -> par |> parse_binary (to_binary_op tok.kind) lhs min_bp
    | _ when is_assign_op tok.kind     -> par |> parse_assign (to_assign_op tok.kind) lhs min_bp
    | _                                -> (lhs, par)

(* -------------------- Expression With Block -------------------- *)

and expr par =
    let tok = peek par in 
    match tok.kind with
    | If        -> (advance par) |> if_expr
    | While     -> (advance par) |> while_expr 
    | Loop      -> (advance par) |> loop_expr
    | LeftBrace -> par |> block
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
    | LeftBrace  -> let (stmts, expr_opt, par') = (advance par) |> parse_block [] in 
                    (Block {stmts = stmts; expr = expr_opt}, par')
    | _  -> let par' = report_error "expected { before block expression" tok par in 
            (ErrorExpr tok.span, par') 

(* -------------------- If Expression -------------------- *)

and if_expr par = 
    let (cond, par') = expr par in 
    let (then_block, par'') = block par' in 
    let tok = peek par'' in 
    match tok.kind with 
    | Else -> 
        let (else_block, par''') = block (advance par'') in 
        (IfExpr {cond = cond; then_branch = then_block; else_branch = Some else_block}, par''')
    | _ -> (IfExpr {cond = cond; then_branch = then_block; else_branch = None}, par'')

(* -------------------- Loop Expressions -------------------- *)

and while_expr par = 
    let (cond, par') = expr par in 
    let (body, par'') = block par' in 
    (WhileLoop {cond = cond; body = body}, par'')

and loop_expr par = 
    let (body, par') = block par in
    (InfiniteLoop body, par')

(* -------------------- Types -------------------- *)

and parse_typ par = 
    let tok = peek par in 
    match tok.kind with 
    | LeftBracket -> let par' = advance par in par' |> parse_fixed_array_typ
    | LeftParen   -> let par' = advance par in par' |> parse_tuple_typ
    | Identifier  ->
        let (name, par') = get_identifier par in 
        begin
            match get_primitive_typ name with 
            | Some typ -> (PrimitiveType typ, par')
            | None     -> (UserType name, par')
        end
    | _ -> 
        if tok.kind <> Illegal then
            let par' = report_error "not a valid type" tok par in 
            (ErrorType tok.span, par')
        else
            let par' = advance par in 
            (ErrorType tok.span, par')

(* -------------------- Fixed Array Type -------------------- *)

and parse_fixed_array_typ par =
    let (typ, par') = par |> parse_typ in 
    let tok = peek par' in 
    match tok.kind with 
    | Comma -> 
        let par'' = advance par' in
        par'' |> parse_array_size typ
    | _ -> 
        let par'' = report_error "expected ',' after array element type" tok par' in 
        (ArrayType (typ, Unknown), par'')

and parse_array_size typ par =
    let tok = peek par in 
    match tok.kind with 
    | IntLiteral ->
        par |> parse_size_and_closing typ tok
    | _ -> 
        let par' = report_error "expected array size" tok par in 
        (ArrayType (typ, Unknown), par')
        
and parse_size_and_closing typ tok par =
    match Int64.of_string_opt (lexeme tok par) with
    | Some size -> 
        let par' = advance par in 
        let tok = peek par' in 
        begin
            match tok.kind with
            | RightBracket ->
                let par'' = advance par' in
                (ArrayType (typ, Known size), par'')
            | _ ->
                let par'' = report_error "expected ']' to close array type" tok par' in 
                (ArrayType (typ, Known size), par'')
        end
    | None          -> let par' = report_error "could not parse array size" tok par 
                       in (ErrorType tok.span, par')

(* -------------------- Tuple Type -------------------- *)

and parse_tuple_typ par = 
    let (typ, par') = par |> parse_typ in 
    let tok = peek par' in 
    match tok.kind with 
    | Comma -> 
        let par'' = advance par' in
        let (rest, par''') = par'' |> parse_type_list [] in 
        (TupleType (typ :: rest), par''')
    | _ -> 
        let par'' = report_error "expected ',' after tuple element type" tok par' in 
        (TupleType [typ], par'')

and parse_type_list types par =
    let tok = peek par in 
    if tok.kind = RightParen then 
        let par' = advance par in (List.rev types, par')
    else
        let (typ, par') = par |> parse_typ in 
        let types' = (typ :: types) in 
        let tok' = peek par' in 
        
        if tok'.kind = Comma then 
            let par'' = advance par' in 
            par'' |> parse_type_list types'
        else if tok'.kind = RightParen then 
            par' |> parse_type_list types'
        else
            let par'' = report_error "expected ',' or ')' after type" tok' par' 
            in (List.rev types', par'')

(* -------------------- Let Statement  -------------------- *)

and let_stmt par = 
    let (name, par') = get_identifier par in 
    let tok = peek par' in 
    let (typ, par'') =  
        if tok.kind = Colon then 
            let par'' = advance par' in (* consume : *)
            let (typ, par''') = par'' |> parse_typ in (* consume typ *)
            (Some typ, par''')
        else
            (None, par')
    in
    let (_, par''') = next par'' in (* consume = *)
    let (value, par'''') = par''' |> expr in
    (LetStmt {name = name; typ = typ; expr = value}, par'''')  

(* -------------------- Var (Mutable) Statement -------------------- *)

and var_stmt par = 
    let (name, par') = get_identifier par in 
    let tok = peek par' in 
    let (typ, par'') =  
        if tok.kind = Colon then 
            let par'' = advance par' in (* consume : *)
            let (typ, par''') = par'' |> parse_typ in (* consume typ *)
            (Some typ, par''')
        else
            (None, par')
    in
    let (_, par''') = next par'' in (* consume = *)
    let (value, par'''') = par''' |> expr in
    (VarStmt {name = name; typ = typ; expr = value}, par'''')  

(* -------------------- Statement -------------------- *)

and stmt par = 
    let tok = peek par in 
    match tok.kind with 
    | Fn | Struct | Const -> 
        let (item_stmt, par') = item par in
        (Item item_stmt, par')
    | Let -> let_stmt (advance par)
    | Var -> var_stmt (advance par)
    | _   -> 
        let (expr_stmt, par') = expr par in 
        (ExprStmt expr_stmt, par')

(* -------------------- Function Item -------------------- *)

and fn_item par = 
    let (name, par') = get_identifier par in 
    let (tok, par'') = next par' in 
    match tok.kind with 
    | LeftParen -> 
        let (params, par''') = par'' |> parse_params_list ~type_required:true [] in 
        par''' |> parse_fn_body name params
    | _ -> 
        let par''' = report_error "expected '(' after function name" tok par'' in 
        (FnItem { name = name; params = []; return_type = None; body = ErrorExpr tok.span; }, par''')

and parse_fn_body name params par =
    let tok = peek par in 
    match tok.kind with 
    | MinusGreater -> (* has return type *)
        let par' = advance par in 
        let (typ, par'') = parse_typ par' in 
        let (block_expr, par''') = block par'' in
        (FnItem { name = name; params = params; return_type = Some typ; body = block_expr; }, par''')
    | _ -> (* no return type -> infer later *)
        let (block_expr, par') = block par in 
        (FnItem { name = name; params = params; return_type = None; body = block_expr; }, par')

(* -------------------- Parameters -------------------- *)

and parse_param ~type_required par =
    let (name, par') = get_identifier par in 
    let tok = peek par' in 
    match tok.kind with 
    | Colon -> 
        let par'' = advance par' in 
        let (typ, par''') = parse_typ par'' in 
        ({ param_name = name; param_typ = Some typ; }, par''')
    | _ -> 
        if type_required then 
            let par'' = report_error "expected ':' after parameter name" tok par' in 
            ({param_name = name; param_typ = Some (ErrorType tok.span); }, par'')   
        else
            ({param_name = name; param_typ = None; }, par')
        
and parse_params_list ~type_required params par =
    let tok = peek par in 
    if tok.kind = RightParen then 
        let par' = advance par in (List.rev params, par')
    else
        let (param, par') = par |> parse_param ~type_required in 
        let params' = param :: params in 
        let tok = peek par' in 
        if tok.kind = Comma then 
            let par'' = advance par' in par'' |> parse_params_list ~type_required params'
        else if tok.kind = RightParen then 
            par' |> parse_params_list ~type_required params'
        else
            let par'' = report_error  "expected ',' or ')' after parameter" tok par' 
            in (List.rev params', par'')

(* -------------------- Struct Item -------------------- *)

and struct_item par =
    let (name, par') = get_identifier par in
    let (tok, par'') = next par' in
    match tok.kind with 
    | LeftBrace -> 
        let (fields, par''') = par'' |> parse_field_decls [] in 
        (StructItem { name = name; fields = fields; }, par''')
    | _ -> 
        let par''' = report_error "expected '{' after struct name" tok par'' in 
        (StructItem { name = name; fields = [] }, par''')

(* -------------------- Field Declarations -------------------- *)

and parse_field par =
    let (name, par') = get_identifier par in 
    let tok = peek par' in 
    match tok.kind with 
    | Colon -> 
        let par'' = advance par' in 
        let (typ, par''') = parse_typ par'' in 
        let tok' = peek par''' in 
        begin 
            match tok'.kind with 
            | Equal -> 
                let (expr, par'''') = expr (advance par''') in 
                ({ field_name = name; field_typ = typ; expr = Some expr; }, par'''')
            | _ ->
                ({ field_name = name; field_typ = typ; expr = None; }, par''')
        end
    | _ -> let par'' = report_error "expected ':' after field name" tok par' in 
            ({field_name = name; field_typ = ErrorType tok.span; expr = None; }, par'')   

and parse_field_decls fields par =
    let tok = peek par in 
    if tok.kind = RightBrace then 
        let par' = advance par in (List.rev fields, par')
    else if is_at_end par then 
        let par' = report_error "expected '}' to close struct fields" tok par
        in (List.rev fields, par')
    else
        let (field, par') = par |> parse_field in
        let fields' = field :: fields in 
        par' |> parse_field_decls fields' 

(* -------------------- Const Item -------------------- *)

and const_item par =
    let (name, par') = get_identifier par in 
    let (tok, par'') = next par' in 
    match tok.kind with 
    | Colon -> 
        let (typ, par''') = parse_typ par'' in 
        let tok' = peek par''' in 
        begin
            match tok'.kind with 
            | Equal -> 
                let (expr, par'''') = expr (advance par''') in 
                (ConstItem { name = name; typ = Some typ; expr = expr }, par'''')
            | _ -> 
                let par'''' = report_error "expected '=' after type in const item" tok' par''' in 
                (ConstItem { name = name; typ = Some typ; expr = ErrorExpr tok'.span }, par'''')
        end
    | Equal -> 
        let (expr, par''') = expr par'' in 
        (ConstItem { name = name; typ = None; expr = expr }, par''')
    | _ -> 
        let par''' = report_error "expected '=' after identifier in const item" tok par'' in 
        (ConstItem { name = name; typ = None; expr = ErrorExpr tok.span }, par''')

(* -------------------- Item -------------------- *)

and item par = 
    let tok = peek par in 
    match tok.kind with 
    | Fn        -> fn_item (advance par)
    | Struct    -> struct_item (advance par)
    | Const     -> const_item (advance par)
    | _         -> failwith "todo: implement variant_item"

(* -------------------- Parser Function -------------------- *)

let rec parse_helper items par =
    if par |> is_at_end then        
        (List.rev items, par)
    else
        let tok = peek par in 
        match tok.kind with
        | Fn | Struct | Const -> 
            let (item, par') = item par in 
            par' |> parse_helper ( item :: items )
        | _ -> 
            let par' = report_error "expected a top level item: function, struct, const" tok par in 
            let par'' = advance par' in (* skip bad token *)
            par'' |> parse_helper items
            
let parse par = par |> parse_helper []


