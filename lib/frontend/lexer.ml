open Reporter
open Span
open Source
open Token

type t = {
    source: Source.t;
    start: int;
    curr: int;
    tokens: Token.t list;
    reporter: Reporter.t;
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

let advance lex =
    let c = lex.source.contents.[lex.curr] in (* creates a NEW lexer record with the updated current pointer and other possible values *)
    match c with 
    | '\n' -> (c, { lex with curr = lex.curr + 1; })
    | '\t' -> (c, { lex with curr = lex.curr + 1; })
    | '\r' -> (c, { lex with curr = lex.curr + 1; })
    | _ -> (c, { lex with curr = lex.curr + 1; })

let bump lex = { lex with curr = lex.curr + 1 } (* moves the current pointer by one, ONLY returns the updated lexer state, NOT the character *)

let add_token kind lex =
    let token = { kind = kind; span = { start_ = lex.start; end_ = lex.curr } } in
    { lex with tokens = token :: lex.tokens }

let report_error msg lex =
    let span = { start_ = lex.start; end_ = lex.curr } in
    let lex = { lex with reporter = add_error lex.source.filename span msg None lex.reporter } in
    add_token Illegal lex

let rec read_line_comment lex = 
    if peek lex = Some '\n' || is_at_end lex then lex
    else
        let (_, lex') = advance lex in
        read_line_comment lex'

let rec read_block_comment lex =
    if is_at_end lex then Error ("unterminated block comment", lex)
    else if peek lex = Some '\n' then Error ("unterminated block comment", lex)
    else if peek lex = Some '#' && peek_next lex = Some '>' then
        let (_, lex') = advance lex in 
        let (_, lex'') = advance lex' in
        Ok lex''
    else 
        let (_, lex') = advance lex in
        read_block_comment lex'

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
    if peek lex = Some '\'' then
        let (_, lex') = advance lex in Error ("empty character literal", lex')
    else
        let (c, lex') = advance lex in
        match resolve_char lex' c with
        | Error e -> Error e
        | Ok lex'' ->
            match close_char lex'' with
            | Error e -> Error e
            | Ok lex''' -> 
                lex''' |> 
                add_token CharLiteral 
                |> Result.ok

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

let rec read_hexadecimal_number_helper lex =
    match peek lex with
    | Some '.' -> Error ("hexadecimal numbers cannot have decimal points", lex)
    | Some c when is_hex_digit c ->
        let (_, lex') = advance lex in
        read_hexadecimal_number_helper lex'
    | _ -> Ok lex

let read_hexadecimal_number lex = 
    let (_, lex') = advance lex in
    match peek lex' with
    | None -> Error ("hexadecimal number must have at least one digit after 0x", lex')
    | Some c ->
        if not (is_hex_digit c) then
            Error ("hexadecimal number must have at least one digit after 0x", lex')
        else
            match read_hexadecimal_number_helper lex' with
            | Error e -> Error e
            | Ok lex'' -> 
                lex'' |> 
                add_token IntLiteral 
                |> Result.ok

let rec read_binary_number_helper lex =
    match peek lex with
    | Some '.' -> Error ("binary numbers cannot have decimal points", lex)
    | Some c when is_binary_digit c ->
        let (_, lex') = advance lex in
        read_binary_number_helper lex'
    | _ -> Ok lex

let read_binary_number lex = 
    let (_, lex') = advance lex in
    match peek lex' with
    | None -> Error ("binary number must have at least one digit after 0b", lex')
    | Some c ->
        if not (is_binary_digit c) then
            Error ("binary number must have at least one digit after 0b", lex')
        else
            match read_binary_number_helper lex' with
            | Error e -> Error e
            | Ok lex'' -> 
                lex'' |> 
                add_token IntLiteral 
                |> Result.ok

let rec read_octal_number_helper lex =
    match peek lex with
    | Some '.' -> Error ("octal numbers cannot have decimal points", lex)
    | Some c when is_octal_digit c ->
        let (_, lex') = advance lex in
        read_octal_number_helper lex'
    | _ -> Ok lex

let read_octal_number lex = 
    let (_, lex') = advance lex in
    match peek lex' with
    | None -> Error ("octal number must have at least one digit after 0o", lex')
    | Some c ->
        if not (is_octal_digit c) then
            Error ("octal number must have at least one digit after 0o", lex')
        else
            match read_octal_number_helper lex' with
            | Error e -> Error e
            | Ok lex'' -> 
                lex'' |> 
                add_token IntLiteral
                |> Result.ok
   
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
    | Some c when is_digit c -> 
        let (_, lex') = advance lex in 
        read_decimal_number_helper lex' is_float
    | Some '.' -> if peek_next lex = Some '.'  || is_float then
            Ok (lex, is_float) (* range operator OR extra dot and invalid float *)
        else let (_, lex') = advance lex in (* fractional part*)
            read_decimal_number_helper lex' true
    | Some 'e' | Some 'E' ->  (* scientific part *)
        let (_, lex') = advance lex in 
        let lex'' = 
            if peek lex' = Some '+' || peek lex' = Some '-' 
                then
                let (_, lex'') = advance lex' in lex'' 
            else lex' in
            (match peek lex'' with
            | Some c when not (is_digit c) -> Error ("malformed scientific notation", lex'')
            | _ -> read_decimal_number_helper lex'' true)
    | _ -> Ok (lex, is_float)
    
let read_decimal_number lex =
    match read_decimal_number_helper lex false with
    | Error e -> Error e
    | Ok (lex', is_float) -> if peek lex' = Some '.' && peek_next lex' = Some '.' then
        lex' |> 
        add_token IntLiteral
        |> Result.ok (* return early, main loop consumes the range operator ..*)
    else
        (match is_valid_fractional_part lex' with
        | Error e -> Error e
        | Ok lex'' -> if is_float then 
            lex'' |> 
            add_token FloatLiteral
            |> Result.ok
        else
           lex'' |> 
           add_token IntLiteral
           |> Result.ok)

let rec read_identifier_helper lex =
     match peek lex with
    | Some c when is_alnum c ->
        let (_, lex') = advance lex in
        read_identifier_helper lex'
    | _ -> Ok lex

let read_identifier lex =
    match read_identifier_helper lex with
    | Error e -> Error e
    | Ok lex' -> let lexeme =
        String.sub lex.source.contents lex'.start (lex'.curr - lex'.start) in
        match String.lowercase_ascii lexeme with
        (* Bool Keywords*)
        | "true"        -> lex' |> add_token True |> Result.ok
        | "false"       -> lex' |> add_token False |> Result.ok

        (* Unit Type *)
        | "unit"        -> lex' |> add_token Unit |> Result.ok

        (* Function / Statement Keywords *)
        | "fn"          -> lex' |> add_token Fn |> Result.ok
        | "let"         -> lex' |> add_token Let |> Result.ok
        | "in"          -> lex' |> add_token In |> Result.ok
        | "mut"         -> lex' |> add_token Mut |> Result.ok
        | "const"       -> lex' |> add_token Const |> Result.ok
        | "return"      -> lex' |> add_token Return |> Result.ok

        (* If / Loop / Match Keywords *)
        | "if"          -> lex' |> add_token If |> Result.ok
        | "else"        -> lex' |> add_token Else |> Result.ok
        | "for"         -> lex' |> add_token For |> Result.ok
        | "while"       -> lex' |> add_token While |> Result.ok
        | "loop"        -> lex' |> add_token Loop |> Result.ok
        | "match"       -> lex' |> add_token Match |> Result.ok
        | "break"       -> lex' |> add_token Break |> Result.ok

        (* User Type Keywords *)
        | "struct"      -> lex' |> add_token Struct |> Result.ok
        | "variant"     -> lex' |> add_token Variant |> Result.ok
        | "use"         -> lex' |> add_token Use |> Result.ok

        (* Other Keywords *)
        | "as"          -> lex' |> add_token As |> Result.ok

        (* Identifier *)
        | _             -> lex' |> add_token Identifier |> Result.ok
    
let read_number lex c = 
    if c = '0' then
        if peek lex = Some 'x' || peek lex = Some 'X' then
            read_hexadecimal_number lex
        else if peek lex = Some 'b' || peek lex = Some 'B' then
            read_binary_number lex
        else if peek lex = Some 'o' || peek lex == Some 'O' then
            read_octal_number lex
        else
            read_decimal_number lex
    else
          read_decimal_number lex
         
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
    | ' ' | '\r'  | '\t' | '\n' -> lex (* skip these characters, return same lexer state *)
    (* Operators *)
    | '<' -> (match peek lex with
            | Some '#' -> (match read_block_comment (bump lex) with (* looking at block comment <# *)
                | Ok lex -> lex
                | Error (msg, lex) -> lex |> report_error msg)
            | Some '<' -> let lex' = bump lex in (* looking now at << *)
                (match peek lex' with
                | Some '=' -> lex' |> bump |> add_token LessLessEqual
                | _ -> lex' |> add_token LessLess)
            | Some '=' -> lex |> bump |> add_token LessEqual (* we already looked at <# and <<= / <<, now we look for <=, otherwise <*)
            | _ -> lex |> add_token Less
            )    
    | '>' -> (match peek lex with
            | Some '>' -> let lex' = bump lex in (* looking now at >> *)
                (match peek lex' with
                | Some '=' -> lex' |> bump |> add_token GreaterGreaterEqual (* >>= *)
                | _ -> lex' |> add_token GreaterGreater) (* >> *) 
            | Some '=' -> lex |> bump |> add_token GreaterEqual (* we already looked at >>= / >>, now we look for >=, otherwise >*)
            | _ -> lex |> add_token Greater
            ) 
    | '.' -> (match peek lex with
            | Some '.' -> let lex' = bump lex in (* looking now at .. *)
                (match peek lex' with
                | Some '=' -> lex' |> bump |> add_token PeriodPeriodEqual (* ..= *)
                | _ -> lex' |> add_token PeriodPeriod) (* .. *) 
            | _ -> lex |> add_token Period) 
    | '+' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token PlusEqual
            | _ -> lex |> add_token Plus)     

    | '-' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token MinusEqual
            | Some '>' -> lex |> bump |> add_token MinusGreater (* issue here *)
            | _ -> lex |> add_token Minus)       
    | '*' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token StarEqual
            | _ -> lex |> add_token Star)            
    | '/' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token SlashEqual
            | _ -> lex |> add_token Slash)
    | '%' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token PercentEqual
            | _ -> lex |> add_token Percent)
    | '!' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token ExclaimEqual
            | _ -> lex |> add_token Exclaim)
    | '=' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token EqualEqual
            | Some '>' -> lex |> bump |> add_token EqualGreater (* issue here *)
            | _ -> lex |> add_token Equal)
    | '&' -> (match peek lex with 
            | Some '&' -> lex |> bump |> add_token AmpAmp
            | Some '=' -> lex |> bump |> add_token AmpEqual
            | _ -> lex |> add_token Amp)
    | '|' -> (match peek lex with 
            | Some '|' -> lex |> bump |> add_token PipePipe
            | Some '=' -> lex |> bump |> add_token PipeEqual
            | _ -> lex |> add_token Pipe)
    | '^' -> (match peek lex with 
            | Some '=' -> lex |> bump |> add_token CaretEqual
            | _ -> lex |> add_token Caret)
    | '~' -> lex |> add_token Tilde
    | '?' -> (match peek lex with 
            | Some '?' -> lex |> bump |> add_token QuestionQuestion
            | _ -> lex |> add_token Question)
    | '@' -> lex |> add_token At
    (* Line Comment *)
    | '#' -> read_line_comment lex
    (* Literals *)
    | '\'' -> (match read_char lex with 
        | Ok lex -> lex
        | Error (msg, lex) -> lex |> report_error msg)
    | '"' -> (match read_string lex with
        | Ok lex -> lex
        | Error (msg, lex) -> lex |> report_error msg)
    | c when is_digit c -> (match read_number lex c with
        | Ok lex -> lex
        | Error (msg, lex) -> lex |> report_error msg)
    | c when is_alpha c -> (match read_identifier lex with
        | Ok lex -> lex
        | Error (msg, lex) -> lex |> report_error msg)
    (* Character not supported in language, report error *)
    | _ -> lex |> report_error (Printf.sprintf "unexpected character '%c'" c)
        
let rec read_tokens lex =
    if is_at_end lex then
        let lex = { lex with start = lex.curr } in
        { lex with tokens = List.rev (add_token EOF lex).tokens }
    else
        let lex = { lex with start = lex.curr } in
        read_tokens (read_token lex)
