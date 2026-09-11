open Span

type token_kind = 
(* Literals *)
| Identifier
| IntLiteral
| FloatLiteral
| CharLiteral
| StringLiteral
| Unit (* the unit type *)
(* Bool Keywords*)
| True
| False
(* Function / Statement Keywords *)
| Fn
| Let
| Var
| Mut
| Const
| Return
(* If / Loop / Match Keywords *)
| If
| Else
| For
| While
| Loop
| Match
| In
| Break
(* User Type Keywords *)
| Struct
| Variant
| Use
(* Algebraic Effects Keywords *)
| Effect
| Uses
| With
| Do
| Resume
(* Other Keywords *)
| As (* safe casting *)
(* Punctuation *)
| LeftParen
| RightParen
| LeftBrace
| RightBrace
| LeftBracket
| RightBracket
| Comma
| Period
| Colon
| LessLessEqual
| GreaterGreaterEqual
| PeriodPeriodEqual
| AmpAmp
| PipePipe
| EqualEqual
| ExclaimEqual
| LessEqual
| GreaterEqual
| LessLess
| GreaterGreater
| PlusEqual
| MinusEqual
| StarEqual
| SlashEqual
| PercentEqual
| AmpEqual
| PipeEqual
| CaretEqual
| MinusGreater
| PeriodPeriod
| EqualGreater
| QuestionQuestion
| Amp
| Pipe
| Caret
| Tilde
| Plus
| Minus
| Star
| Slash
| Percent
| Question
| At
| Exclaim
| Less
| Greater
| Equal
(* Comments *)
| LineComment  (* # *)
(* Error *)
| Illegal
(* End Token *)
| EOF

let tok_to_string kind =
    match kind with
    (* Literals *)
    | Identifier       -> "identifier"
    | IntLiteral       -> "integer"
    | FloatLiteral     -> "float"
    | CharLiteral      -> "character"
    | StringLiteral    -> "string"
    | Unit             -> "unit"
    (* Bool Keywords *)
    | True             -> "true"
    | False            -> "false"
    (* Function / Statement Keywords *)
    | Fn                -> "fn"
    | Let               -> "let"
    | Var               -> "var"
    | Mut               -> "mut"
    | Const             -> "const"
    | Return            -> "return"
    (* If / Loop / Match Keywords *)
    | If                -> "if"
    | Else              -> "else"
    | For               -> "for"
    | While             -> "while"
    | Loop              -> "loop"
    | Match             -> "match"
    | In                -> "in"
    | Break             -> "break"
    (* User Type Keywords *)
    | Struct             -> "struct"
    | Variant            -> "variant"
    | Use                -> "use"
    (* Algebraic Effects Keywords *)
    | Effect             -> "effect"
    | Uses               -> "uses"
    | With               -> "with"
    | Do                 -> "do"
    | Resume             -> "resume"
    (* Other Keywords *)
    | As                 -> "as"
    (* Punctuation *)
    | LeftParen          -> "("
    | RightParen         -> ")"
    | LeftBrace          -> "{"
    | RightBrace         -> "}"
    | LeftBracket        -> "["
    | RightBracket       -> "]"
    | Comma              -> ","
    | Period             -> "."
    | Colon              -> ":"
    | LessLessEqual      -> "<<="
    | GreaterGreaterEqual -> ">>="
    | PeriodPeriodEqual  -> "..="
    | AmpAmp             -> "&&"
    | PipePipe           -> "||"
    | EqualEqual         -> "=="
    | ExclaimEqual       -> "!="
    | LessEqual          -> "<="
    | GreaterEqual       -> ">="
    | LessLess           -> "<<"
    | GreaterGreater     -> ">>"
    | PlusEqual          -> "+="
    | MinusEqual         -> "-="
    | StarEqual          -> "*="
    | SlashEqual         -> "/="
    | PercentEqual       -> "%="
    | AmpEqual           -> "&="
    | PipeEqual          -> "|="
    | CaretEqual         -> "^="
    | MinusGreater       -> "->"
    | PeriodPeriod       -> ".."
    | EqualGreater       -> "=>"
    | QuestionQuestion   -> "??"
    | Amp                -> "&"
    | Pipe               -> "|"
    | Caret              -> "^"
    | Tilde              -> "~"
    | Plus               -> "+"
    | Minus              -> "-"
    | Star               -> "*"
    | Slash              -> "/"
    | Percent            -> "%"
    | Question            -> "?"
    | At                 -> "@"
    | Exclaim             -> "!"
    | Less                -> "<"
    | Greater             -> ">"
    | Equal               -> "="
    (* Comments *)
    | LineComment        -> "#"
    (* Error *)
    | Illegal            -> "illegal token"
    (* End Token *)
    | EOF                -> "end of file"

type t = {
    kind: token_kind;
    span: Span.t;
}