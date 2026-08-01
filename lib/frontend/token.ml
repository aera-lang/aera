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
| In (* e.g., let x = 5 in x * x -> parses to let <name> = <expr> [ in <expr> *)
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
| Break
(* User Type Keywords *)
| Struct
| Variant
(* Other Keywords *)
| As (* safe casting *)
| Use (* to use a module *)
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
| BlockComment (* <# ... #>*)
(* Error *)
| Illegal
(* End Token *)
| EOF

type t = {
    kind: token_kind;
    span: Span.t;
}