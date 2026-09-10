open Ast
open Reporter
open Lexer
open Token
open Op
open Source
   
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

(* -------------------- Error Recovery / Handling -------------------- *)

let report_error msg tok par = 
    { par with reporter = add_error par.source.filename tok.span msg None par.reporter }

(* -------------------- Pratt Parser -------------------- *)

(* Literals *)

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

let parse_identifier tok par =
    let value = lexeme tok par in (Ast.Identifier value, par)

(* Unary - Prefix *)

let rec parse_prefix op par =
    let (rhs, par') = par |> expr_bp (prefix_bp op) in (Unary ({op = op; rhs = rhs}), par')

(* Grouping *)

and parse_grouping par =
    let (expr, par') = par |> expr_bp 0 in (* get the grouping expr *)
    let tok = peek par' in 
    match tok.kind with 
    | RightParen -> let (_, par'') = next par' in (Grouping expr, par'')
    | _          -> let par'' = report_error "expected ')' to close grouping" tok par' 
                    in (Grouping expr, par'') (* still have a valid grouping, just not closed properly *)

(* Unrecognized Tokens - includes illegal tokens *)

and parse_unrecognized tok par =
    let par = 
        if tok.kind <> Illegal then 
            let msg = Printf.sprintf "unsupported token in language: %s" (lexeme tok par) in
            par |> report_error msg tok
        else par
        in 
            let (_, par') = next par 
            in (ErrorExpr tok.span, par')

(* Call *)

(* and parse_call lhs par =
    let (_, par') = next par in 
    let (args, par'') = par' |> parse_args [] in
    (Call {callee = lhs; args = args}, par'') *)

(* Binary & Assign *)

and parse_binary op lhs min_bp par =
    match binary_bp op with 
    | None -> (lhs, par)
    | Some (left_bp, right_bp) ->
        if left_bp < min_bp then (lhs, par)
        else
            let (_, par') = next par in
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
            let (_, par') = next par in
            let (rhs, par'') = par' |> expr_bp right_bp in
            let lhs' = Assign ({lhs = lhs; op = op; rhs}) in
            let (lhs'', par''') = par'' |> loop lhs' min_bp in 
            (lhs'', par''')

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
    | Identifier                -> par' |> parse_identifier tok
    (* Prefix Operators *)
    | Minus                     -> par' |> parse_prefix Neg
    | Exclaim                   -> par' |> parse_prefix Not
    (* Grouped Expression *)
    | LeftParen                 -> par' |> parse_grouping
    (* Invalid Token *)
    | _                         -> par' |> parse_unrecognized tok                       
    in
    (* Infix Operators*)
    par'' |> loop lhs min_bp 

and loop lhs min_bp par = 
    let tok = peek par in 
    match tok.kind with 
    | EOF                               -> (lhs, par)
    (* Call Expressions *)
    | LeftParen                         -> par |> parse_call lhs
    (* Binary / Assign Operators *)
    | op when is_binary_op tok.kind     -> par |> parse_binary (to_binary_op tok.kind) lhs min_bp
    | op when is_assign_op tok.kind     -> par |> parse_assign (to_assign_op tok.kind) lhs min_bp
    | _                                 -> (lhs, par)


