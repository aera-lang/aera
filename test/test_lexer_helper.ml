open Frontend
open Alcotest

(* Helper Functions *)

let make_lexer src = {
	Lexer.source = src;
	start = 0;
	curr = 0;
	tokens = [];
	reporter = [];
}

let make_source input filename = Source.create input filename

let lex_string input =
	let src = make_source input "test.aera" in
  	let lex = make_lexer src in
  	let lex' = Lexer.read_tokens lex in
  	lex'.tokens

let make_token kind start_ end_ = { Token.kind; span = { start_; end_ } }

let token_to_string kind =
	match kind with
	| Token.Identifier                -> "identifier"
	| IntLiteral                -> "int literal"
	| FloatLiteral              -> "float literal"
	| CharLiteral               -> "char literal"
	| StringLiteral             -> "string literal"
	| Unit                      -> "unit"
	| True                      -> Printf.sprintf "bool(%b)" true
	| False                     -> Printf.sprintf "bool(%b)" false
	(* Function / Statement Keywords *)
	| Fn						-> "fn"
	| Let						-> "let"
	| In						-> "in"
	| Mut                       -> "mut"
	| Const                     -> "const"
  	| Return                    -> "return"
	(* If / Loop / Match Keywords *)
	| If                        -> "if"
	| Else                      -> "else"
	| For                       -> "for"
	| While                     -> "while"
	| Loop                      -> "loop"
	| Match                     -> "match"
	| Break                     -> "break"
	(* User Type Keywords *)
	| Struct					-> "struct"
	| Variant					-> "variant"
	(* Other Keywords *)
	| As                        -> "as"
	| Use						-> "use"
	(* Punctuation *)
	| LeftParen                 -> "("
	| RightParen                -> ")"
	| LeftBrace                 -> "{"
	| RightBrace                -> "}"
	| LeftBracket               -> "["
	| RightBracket              -> "]"
	| Comma                     -> ","
	| Period                    -> "."
	| Colon                     -> ":"
	| LessLessEqual             -> "<<="
	| GreaterGreaterEqual       -> ">>="
	| PeriodPeriodEqual         -> "..="
	| AmpAmp                    -> "&&"
	| PipePipe                  -> "||"
	| EqualEqual                -> "=="
	| ExclaimEqual              -> "!="
	| LessEqual                 -> "<="
	| GreaterEqual              -> ">="
	| LessLess                  -> "<<"
	| GreaterGreater            -> ">>"
	| PlusEqual                 -> "+="
	| MinusEqual                -> "-="
	| StarEqual                 -> "*="
	| SlashEqual                -> "/="
	| PercentEqual              -> "%="
	| AmpEqual                  -> "&="
	| PipeEqual                 -> "|="
	| CaretEqual                -> "^="
	| MinusGreater              -> "->"
	| PeriodPeriod              -> ".."
	| EqualGreater              -> "=>"
	| QuestionQuestion          -> "??"
	| Amp                       -> "&"
	| Pipe                      -> "|"
	| Caret                     -> "^"
	| Tilde                     -> "~"
	| Plus                      -> "+"
	| Minus                     -> "-"
	| Star                      -> "*"
	| Slash                     -> "/"
	| Percent                   -> "%"
	| Question                  -> "?"
	| At                        -> "@"
	| Exclaim                   -> "!"
	| Less                      -> "<"
	| Greater                   -> ">"
	| Equal                     -> "="
	| Illegal				    -> "illegal"
	| EOF                       -> "eof"
	| _						    -> ""

(* Shortcut Constructors *)

let identifier start_ end_                 		= make_token Token.Identifier start_ end_
let int_lit start_ end_             			= make_token Token.IntLiteral start_ end_
let float_lit start_ end_           			= make_token Token.FloatLiteral start_ end_
let char_lit start_ end_                  		= make_token Token.CharLiteral start_ end_
let str_lit start_ end_                    		= make_token Token.StringLiteral start_ end_
let unit start_ end_                           = make_token Token.Unit start_ end_
let true_lit start_ end_                       = make_token Token.True start_ end_
let false_lit start_ end_                      = make_token Token.True start_ end_
(* Function / Statement Keywords *)
let fn start_ end_                             = make_token Token.Fn start_ end_
let let_ start_ end_                           = make_token Token.Let start_ end_
let in_ start_ end_                            = make_token Token.In start_ end_
let mut start_ end_                            = make_token Token.Mut start_ end_
let const start_ end_                          = make_token Token.Const start_ end_
let return_ start_ end_                        = make_token Token.Return start_ end_
(* If / Loop / Match Keywords *)
let if_ start_ end_                            = make_token Token.If start_ end_
let else_ start_ end_                          = make_token Token.Else start_ end_
let for_ start_ end_                           = make_token Token.For start_ end_
let while_ start_ end_                         = make_token Token.While start_ end_
let loop start_ end_                           = make_token Token.Loop start_ end_
let match_ start_ end_                         = make_token Token.Match start_ end_
let break_ start_ end_                         = make_token Token.Break start_ end_
(* User Type Keywords *)
let struct_ start_ end_                         = make_token Token.Struct start_ end_
let variant start_ end_                         = make_token Token.Variant start_ end_
(* Other Keywords *)
let as_ start_ end_                            = make_token Token.As start_ end_
let use start_ end_                            = make_token Token.Use start_ end_
(* Punctuation*)
let left_paren start_ end_                     = make_token Token.LeftParen start_ end_
let right_paren start_ end_                    = make_token Token.RightParen start_ end_
let left_brace start_ end_                     = make_token Token.LeftBrace start_ end_
let right_brace start_ end_                    = make_token Token.RightBrace start_ end_
let left_bracket start_ end_                   = make_token Token.LeftBracket start_ end_
let right_bracket start_ end_                  = make_token Token.RightBracket start_ end_
let comma start_ end_                          = make_token Token.Comma start_ end_
let period start_ end_                         = make_token Token.Period start_ end_
let colon start_ end_                          = make_token Token.Colon start_ end_
let less_less_equal start_ end_                = make_token Token.LessLessEqual start_ end_
let greater_greater_equal start_ end_          = make_token Token.GreaterGreaterEqual start_ end_
let period_period_equal start_ end_            = make_token Token.PeriodPeriodEqual start_ end_
let amp_amp start_ end_                        = make_token Token.AmpAmp start_ end_
let pipe_pipe start_ end_                      = make_token Token.PipePipe start_ end_
let equal_equal start_ end_                    = make_token Token.EqualEqual start_ end_
let exclaim_equal start_ end_                  = make_token Token.ExclaimEqual start_ end_
let less_equal start_ end_                     = make_token Token.LessEqual start_ end_
let greater_equal start_ end_                  = make_token Token.GreaterEqual start_ end_
let less_less start_ end_                      = make_token Token.LessLess start_ end_
let greater_greater start_ end_                = make_token Token.GreaterGreater start_ end_
let plus_equal start_ end_                     = make_token Token.PlusEqual start_ end_
let minus_equal start_ end_                    = make_token Token.MinusEqual start_ end_
let star_equal start_ end_                     = make_token Token.StarEqual start_ end_
let slash_equal start_ end_                    = make_token Token.SlashEqual start_ end_
let percent_equal start_ end_                  = make_token Token.PercentEqual start_ end_
let amp_equal start_ end_                      = make_token Token.AmpEqual start_ end_
let pipe_equal start_ end_                     = make_token Token.PipeEqual start_ end_
let caret_equal start_ end_                    = make_token Token.CaretEqual start_ end_
let minus_greater start_ end_                  = make_token Token.MinusGreater start_ end_
let period_period start_ end_                  = make_token Token.PeriodPeriod start_ end_
let equal_greater start_ end_                  = make_token Token.EqualGreater start_ end_
let question_question start_ end_              = make_token Token.QuestionQuestion start_ end_
let amp start_ end_                            = make_token Token.Amp start_ end_
let pipe start_ end_                           = make_token Token.Pipe start_ end_
let caret start_ end_                          = make_token Token.Caret start_ end_
let tilde start_ end_                          = make_token Token.Tilde start_ end_
let plus start_ end_                           = make_token Token.Plus start_ end_
let minus start_ end_                          = make_token Token.Minus start_ end_
let star start_ end_                           = make_token Token.Star start_ end_
let slash start_ end_                          = make_token Token.Slash start_ end_
let percent start_ end_                        = make_token Token.Percent start_ end_
let question start_ end_                       = make_token Token.Question start_ end_
let at start_ end_                             = make_token Token.At start_ end_
let exclaim start_ end_                        = make_token Token.Exclaim start_ end_
let less start_ end_                           = make_token Token.Less start_ end_
let greater start_ end_                        = make_token Token.Greater start_ end_
let equal start_ end_                          = make_token Token.Equal start_ end_
let illegal start_ end_                 = make_token Token.Illegal start_ end_
let eof start_ end_                            = make_token Token.EOF start_ end_

let token_equal a b =
    a.Token.kind = b.Token.kind &&
	a.Token.span.start_ = b.Token.span.start_ &&
	a.Token.span.end_ = b.Token.span.end_

let token_pp fmt tok =
    Format.fprintf fmt "%s at %d:%d"
        (token_to_string tok.Token.kind)
        tok.Token.span.start_
        tok.Token.span.end_

let token_testable = Alcotest.testable token_pp token_equal

let expect_tokens actual expected =
    Alcotest.(check (list token_testable)) "tokens" expected actual