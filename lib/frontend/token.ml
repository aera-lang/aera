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
| Module
| Use
(* Other Keywords *)
| In (* e.g., let x = 5 in x * x -> parses to let <name> = <expr> [ in <expr> *)
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
| BlockComment (* <# ... #>*)
(* Error *)
| Illegal
(* End Token *)
| EOF

type token = {
    kind: token_kind;
    span: span;
}