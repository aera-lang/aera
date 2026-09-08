open Reporter
open Span
open Source
open Token

type t = {
    source: Source.t;
    start: int;
    curr: int;
    tokens: Token.t list;
    reporter: Reporter.t; (* the final reporter is the lexer + the parser, so lexer.reporter @ parser.reporter (they are lists) *)
}

let create src = {
	source = src;
    start = 0;
    curr = 0;    
    tokens = [];
    reporter = [];
} 

let is_digit c = 
    c >= '0' && c <= '9'

let is_hex_digit c =
    is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let is_binary_digit c =
    c = '0' || c = '1'

let is_octal_digit c =
	c >= '0' && c <= '7'

let is_alpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_alnum c =
    is_alpha c || is_digit c || c = '_'

let is_symbol c =
    (c >= '!' && c <= '/') || (c >= ':' && c <= '@') || (c >= '[' && c <= '`') || (c >= '{' && c <= '~')

let is_space c =
    c = ' '
	
let is_printable c =
    is_alpha c || is_digit c || is_symbol c || is_space c

let is_at_end lex =
    lex.curr >= String.length lex.source.contents

let lexeme lex = String.sub lex.source.contents lex.start (lex.curr - lex.start)

let peek lex = 
    if is_at_end lex then
        None
    else
        Some lex.source.contents.[lex.curr] (* returns the character at the current pointer *)

let peek_next lex = 
    if lex.curr + 1 >= String.length lex.source.contents then
        None
    else
        Some lex.source.contents.[lex.curr + 1] (* returns the character 1 position ahead of the current pointer *)

let advance lex = (lex.source.contents.[lex.curr], { lex with curr = lex.curr + 1 })

let bump lex = { lex with curr = lex.curr + 1 } (* moves the current pointer by one, ONLY returns the updated lexer state, NOT the character *)

let add_token kind lex =
    { lex with tokens = { kind = kind; span = { start_ = lex.start; end_ = lex.curr } } :: lex.tokens }

let report_error msg lex =
    let span = { start_ = lex.start; end_ = lex.curr } in
    let lex = { lex with reporter = add_error lex.source.filename span msg None lex.reporter } in
    add_token Illegal lex

let rec read_line_comment lex = 
    if peek lex = Some '\n' || is_at_end lex then lex
    else
        let (_, lex') = advance lex in
        lex' |> read_line_comment

let resolve_char lex c =
    if c = '\\' && not (is_at_end lex) then
        let (c', lex') = advance lex in
        match c' with
        | 'n' | 't' | 'r' | '\\' | '\'' | '"' -> Ok lex'
        | _ -> Error ("invalid escape sequence", lex')
    else if not (is_printable c) then
        if c = '\\' then Error ("unterminated escape sequence in character literal", lex)
        else Error ("invalid character in literal: " ^ String.make 1 c, lex)
    else
        Ok lex

let rec skip_until_closing_quote lex quote =
    if is_at_end lex then lex
    else if peek lex = Some quote then 
        let (_, lex') = advance lex in lex'
    else
        let (_, lex') = advance lex in
        skip_until_closing_quote lex' quote

let close_char lex =
    if peek lex <> Some '\'' then
        if is_at_end lex then Error ("unterminated character literal", lex)
        else
            let lex' = skip_until_closing_quote lex '\'' in
            Error ("character literal must contain only one character", lex')
    else
        let (_, lex') = advance lex in
        Ok lex'
    
let read_char lex =
    let open Result.Syntax in
    if peek lex = Some '\'' then
        let (_, lex') = advance lex in Error ("empty character literal", lex')
    else
        let (c, lex') = advance lex in
        let* lex'' = resolve_char lex' c in 
        let+ lex''' = close_char lex'' in 
        lex''' |> add_token CharLiteral

let rec read_string lex =
    if is_at_end lex then Error ("unterminated string literal", lex)
    else if peek lex = Some '"' then
        let (_, lex') = advance lex in lex' |> add_token StringLiteral |> Result.ok
    else
        let (c, lex') = advance lex in
        if c = '\\' then
            if is_at_end lex' then
                Error ("unterminated string literal", lex')
            else
                let (c', lex'') = advance lex' in
                match c' with
                | 'n' | 't' | 'r' | '/' | '\'' | '\\' | '"' -> read_string lex''
                | _ ->
                    let lex''' = skip_until_closing_quote lex'' '"' in
                    Error ("invalid escape sequence", lex''')
        else
            read_string lex'

let rec read_radix_number_helper lex ~pred ~name =
    match peek lex with
    | Some '.' -> Error (Printf.sprintf "%s numbers cannot have decimal points" name, lex)
    | Some c when pred c ->
        let (_, lex') = advance lex in
        read_radix_number_helper lex' ~pred ~name
    | _ -> Ok lex

let read_radix_number lex ~pred ~notation ~name = 
    let open Result.Syntax in
    let (_, lex') = advance lex in
    match peek lex' with
    | None -> Error (Printf.sprintf "%s number must have at least one digit after 0%s" name notation, lex')
    | Some c ->
        if not (pred c) then
            Error (Printf.sprintf "%s number must have at least one digit after 0%s" name notation, lex')
        else
            let+ lex'' = read_radix_number_helper lex' ~pred ~name in
            lex'' |> add_token IntLiteral
            
let rec read_hexadecimal_number_helper lex =
    match peek lex with
    | Some '.' -> Error ("hexadecimal numbers cannot have decimal points", lex)
    | Some c when is_hex_digit c ->
        let (_, lex') = advance lex in
        read_hexadecimal_number_helper lex'
    | _ -> Ok lex

let read_hexadecimal_number lex = 
    let open Result.Syntax in
    let (_, lex') = advance lex in
    match peek lex' with
    | None -> Error ("hexadecimal number must have at least one digit after 0x", lex')
    | Some c ->
        if not (is_hex_digit c) then
            Error ("hexadecimal number must have at least one digit after 0x", lex')
        else
            let+ lex'' = read_hexadecimal_number_helper lex' in
            lex'' |> add_token IntLiteral

let rec read_binary_number_helper lex =
    match peek lex with
    | Some '.' -> Error ("binary numbers cannot have decimal points", lex)
    | Some c when is_binary_digit c ->
        let (_, lex') = advance lex in
        read_binary_number_helper lex'
    | _ -> Ok lex

let read_binary_number lex = 
    let open Result.Syntax in
    let (_, lex') = advance lex in
    match peek lex' with
    | None -> Error ("binary number must have at least one digit after 0b", lex')
    | Some c ->
        if not (is_binary_digit c) then
            Error ("binary number must have at least one digit after 0b", lex')
        else
            let+ lex'' = read_binary_number_helper lex' in
            lex'' |> add_token IntLiteral 

let rec read_octal_number_helper lex =
    match peek lex with
    | Some '.' -> Error ("octal numbers cannot have decimal points", lex)
    | Some c when is_octal_digit c ->
        let (_, lex') = advance lex in
        read_octal_number_helper lex'
    | _ -> Ok lex

let read_octal_number lex =
    let open Result.Syntax in 
    let (_, lex') = advance lex in
    match peek lex' with
    | None -> Error ("octal number must have at least one digit after 0o", lex')
    | Some c ->
        if not (is_octal_digit c) then
            Error ("octal number must have at least one digit after 0o", lex')
        else
            let+ lex'' = read_octal_number_helper lex' in
            lex'' |> add_token IntLiteral
   
let is_valid_fractional_part lex =
    if peek lex = Some '.' then
        if peek_next lex = Some '.' then
            let (_, lex') = advance lex in
            let (_, lex'') = advance lex' in
            Error ("range operator cannot follow a float literal: ", lex'')
        else
            let (_, lex') = advance lex in
            Error ("malformed float literal: ", lex')
    else
        Ok lex
        
let rec read_decimal_number_helper lex is_float =
    match peek lex with 
    (* Integer part *)
    | Some c when is_digit c -> 
        let (_, lex') = advance lex in 
        read_decimal_number_helper lex' is_float
    (* Range operator OR invalid float with extra dot *)
    | Some '.' -> 
        if peek_next lex = Some '.'  || is_float then Ok (lex, is_float) (* values like 3.14. are accepted here but rejected later on *)
        (* Fractional part *)
        else let (_, lex') = advance lex in
            read_decimal_number_helper lex' true
    (* Scientific notation *)
    | Some 'e' | Some 'E' -> 
        let (_, lex') = advance lex in 
        let lex'' = if peek lex' = Some '+' || peek lex' = Some '-' then
                    let (_, lex'') = advance lex' in lex'' 
                    else lex' in
                    begin
                        match peek lex'' with
                        | Some c when not (is_digit c) -> Error ("malformed scientific notation", lex'')
                        | _ -> read_decimal_number_helper lex'' true
                    end
    | _ -> Ok (lex, is_float)
    
let read_decimal_number lex =
    let open Result.Syntax in 
    let* (lex', is_float) = read_decimal_number_helper lex false in
    if peek lex' = Some '.' && peek_next lex' = Some '.' then
        Ok (lex' |> add_token IntLiteral)  (* main loop consumes the range operator ..*)
    else
        let+ lex'' = is_valid_fractional_part lex' in 
        if is_float then 
            lex'' |> add_token FloatLiteral 
        else
            lex'' |> add_token IntLiteral

let rec read_identifier_helper lex =
     match peek lex with
    | Some c when is_alnum c ->
        let (_, lex') = advance lex in
        read_identifier_helper lex'
    | _ -> Ok lex

let read_identifier lex =
    let open Result.Syntax in
    let+ lex' = read_identifier_helper lex in 
    match String.lowercase_ascii (lexeme lex') with 
    (* Bool Keywords*)
    | "true"        -> lex' |> add_token True
    | "false"       -> lex' |> add_token False
    (* Unit Type *)
    | "unit"        -> lex' |> add_token Unit
    (* Function / Statement Keywords *)
    | "fn"          -> lex' |> add_token Fn
    | "let"         -> lex' |> add_token Let
    | "var"         -> lex' |> add_token Var
    | "mut"         -> lex' |> add_token Mut
    | "const"       -> lex' |> add_token Const
    | "return"      -> lex' |> add_token Return
    (* If / Loop / Match Keywords *)
    | "if"          -> lex' |> add_token If
    | "else"        -> lex' |> add_token Else
    | "for"         -> lex' |> add_token For
    | "while"       -> lex' |> add_token While
    | "loop"        -> lex' |> add_token Loop
    | "match"       -> lex' |> add_token Match
    | "in"          -> lex' |> add_token In
    | "break"       -> lex' |> add_token Break
    (* User Type Keywords *)
    | "struct"      -> lex' |> add_token Struct
    | "variant"     -> lex' |> add_token Variant
    | "use"         -> lex' |> add_token Use
    (* Algebraic Effects Keywords *)
    | "effect"      -> lex' |> add_token Effect
    | "uses"        -> lex' |> add_token Uses
    | "with"        -> lex' |> add_token With
    | "do"          -> lex' |> add_token Do
    | "resume"      -> lex' |> add_token Resume
    (* Other Keywords *)
    | "as"          -> lex' |> add_token As
    (* Identifier *)
    | _             -> lex' |> add_token Identifier

let read_number lex c = 
    if c = '0' then
        if peek lex = Some 'x' || peek lex = Some 'X' then
            read_radix_number lex ~pred:is_hex_digit ~notation:"x" ~name:"hexadecimal"
        else if peek lex = Some 'b' || peek lex = Some 'B' then
            read_radix_number lex ~pred:is_binary_digit ~notation:"b" ~name:"binary"
        else if peek lex = Some 'o' || peek lex = Some 'O' then
            read_radix_number lex ~pred:is_octal_digit ~notation:"o" ~name:"octal"
        else
            read_decimal_number lex
    else
          read_decimal_number lex
          
let with_eq tok tok_eq lex =
  match peek lex with
  | Some '=' -> lex |> bump |> add_token tok_eq
  | _ -> lex |> add_token tok

let read_token lex = 
    let (c, lex) = advance lex in 
    match c with 
    (* Punctutation *)
    | '(' -> lex |> add_token LeftParen
    | ')' -> lex |> add_token RightParen
    | '{' -> lex |> add_token LeftBrace
    | '}' -> lex |> add_token RightBrace
    | '[' -> lex |> add_token LeftBracket
    | ']' -> lex |> add_token RightBracket 
    | ',' -> lex |> add_token Comma
    | ':' -> lex |> add_token Colon
    | ' ' | '\r'  | '\t' | '\n' -> lex (* skip these characters *)
    (* Operators *)
    | '<' -> 
        begin 
            match peek lex with
            | Some '<' -> let lex' = bump lex in (* looking now at << *)
                (match peek lex' with
                | Some '=' -> lex' |> bump |> add_token LessLessEqual
                | _ -> lex' |> add_token LessLess)
            | Some '=' -> lex |> bump |> add_token LessEqual (* we already looked at <<= / <<, now we look for <=, otherwise <*)
            | _ -> lex |> add_token Less
        end     
    | '>' ->
        begin
            match peek lex with
            | Some '>' -> let lex' = bump lex in (* looking now at >> *)
                (match peek lex' with
                | Some '=' -> lex' |> bump |> add_token GreaterGreaterEqual (* >>= *)
                | _ -> lex' |> add_token GreaterGreater) (* >> *) 
            | Some '=' -> lex |> bump |> add_token GreaterEqual (* we already looked at >>= / >>, now we look for >=, otherwise >*)
            | _ -> lex |> add_token Greater
        end
    | '.' -> 
        begin
            match peek lex with
            | Some '.' -> let lex' = bump lex in (* looking now at .. *)
                (match peek lex' with
                | Some '=' -> lex' |> bump |> add_token PeriodPeriodEqual (* ..= *)
                | _ -> lex' |> add_token PeriodPeriod) (* .. *) 
            | _ -> lex |> add_token Period
        end
    | '+' -> lex |> with_eq Plus PlusEqual
    | '-' -> 
        begin
            match peek lex with 
            | Some '=' -> lex |> bump |> add_token MinusEqual
            | Some '>' -> lex |> bump |> add_token MinusGreater
            | _ -> lex |> add_token Minus
        end   
    | '*' -> lex |> with_eq Star StarEqual          
    | '/' -> lex |> with_eq Slash SlashEqual  
    | '%' -> lex |> with_eq Percent PercentEqual
    | '!' -> lex |> with_eq Exclaim ExclaimEqual
    | '=' -> 
        begin 
            match peek lex with 
            | Some '=' -> lex |> bump |> add_token EqualEqual
            | Some '>' -> lex |> bump |> add_token EqualGreater
            | _ -> lex |> add_token Equal
        end
    | '&' -> 
        begin 
            match peek lex with 
            | Some '&' -> lex |> bump |> add_token AmpAmp
            | Some '=' -> lex |> bump |> add_token AmpEqual
            | _ -> lex |> add_token Amp
        end
    | '|' -> 
        begin
            match peek lex with 
            | Some '|' -> lex |> bump |> add_token PipePipe
            | Some '=' -> lex |> bump |> add_token PipeEqual
            | _ -> lex |> add_token Pipe
        end
    | '^' -> lex |> with_eq Caret CaretEqual
    | '~' -> lex |> add_token Tilde
    | '?' -> 
        begin 
            match peek lex with 
            | Some '?' -> lex |> bump |> add_token QuestionQuestion
            | _ -> lex |> add_token Question
        end
    | '@' -> lex |> add_token At
    (* Line Comment *)
    | '#' -> read_line_comment lex
    (* Literals *)
    | '\'' -> 
        begin 
            match read_char lex with 
            | Ok lex -> lex
            | Error (msg, lex) -> lex |> report_error msg
        end
    | '"' -> 
        begin
            match read_string lex with
            | Ok lex -> lex
            | Error (msg, lex) -> lex |> report_error msg
        end
    | c when is_digit c -> 
        begin
            match read_number lex c with
            | Ok lex -> lex
            | Error (msg, lex) -> lex |> report_error msg
        end
    | c when is_alpha c -> 
        begin
            match read_identifier lex with
            | Ok lex -> lex
            | Error (msg, lex) -> lex |> report_error msg
        end
    (* Character not supported in language, report error *)
    | _ -> lex |> report_error (Printf.sprintf "unexpected character '%c'" c)
        
let rec read_tokens lex =
    if is_at_end lex then
        let lex = { lex with start = lex.curr } in
        { lex with tokens = List.rev (add_token EOF lex).tokens }
    else
        let lex = { lex with start = lex.curr } in
        read_tokens (read_token lex)
