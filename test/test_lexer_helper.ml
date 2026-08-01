open Frontend
open Alcotest

(* Helper functions *)

let make_lexer source = {
  Lexer.source;
  start = 0;
  curr = 0;
  start_pos = { line = 1; col = 1 };
	pos = { line = 1; col = 1 };
  tokens = [];
  reporter = { Error.errors = [] };
}

let lex_string input =
  let lex = make_lexer input in
  let lex' = Lexer.read_tokens lex in
  lex'.tokens

let make_token kind lexeme line col =   { Token.kind; lexeme; pos = { line; col } }

let token_to_string kind =
  match kind with
  | Token.Identifier str      -> "identifier(%s)"
  | IntLiteral num            -> Printf.sprintf "int(%d)" num
  | FloatLiteral num          -> Printf.sprintf "float(%f)" num
  | CharLiteral c             -> Printf.sprintf "char(%c)" c
  | StringLiteral str         -> Printf.sprintf "str(%s)" str
  | True                      -> Printf.sprintf "bool(%b)" true
  | False                     -> Printf.sprintf "bool(%b)" false
  | Fn                        -> "fn"
  | Let                       -> "let"
  | Mut                       -> "mut"
  | Const                     -> "const"
  | If                        -> "if"
  | Else                      -> "else"
  | For                       -> "for"
  | While                     -> "while"
  | Loop                      -> "loop"
  | Match                     -> "match"
  | Break                     -> "break"
  | Continue                  -> "continue"
  | Return                    -> "return"
  | In                        -> "in"
  | As                        -> "as"
  | Unit                      -> "unit"
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
  | Illegal				            -> "illegal"
  | EOF                       -> "eof"
  | _						              -> ""

(* Shortcut constructors *)

let identifier str line col                 = make_token (Token.Identifier str) str line col
let int_lit num lexeme line col             = make_token (Token.IntLiteral num) lexeme line col
let float_lit num lexeme line col           = make_token (Token.FloatLiteral num) lexeme line col
let char_lit char line col                  = make_token (Token.CharLiteral char) (String.make 1 char) line col
let str_lit str line col                    = make_token (Token.StringLiteral str) str line col
let true_lit line col                       = make_token Token.True "true" line col
let false_lit line col                      = make_token Token.True "false" line col
let fn line col                             = make_token Token.Fn "fn" line col
let let_ line col                           = make_token Token.Let "let" line col
let mut line col                            = make_token Token.Mut "mut" line col
let const line col                          = make_token Token.Const "const" line col
let if_ line col                            = make_token Token.If "if" line col
let else_ line col                          = make_token Token.Else "else" line col
let for_ line col                           = make_token Token.For "for" line col
let while_ line col                         = make_token Token.While "while" line col
let loop line col                           = make_token Token.Loop "loop" line col
let match_ line col                         = make_token Token.Match "match" line col
let break_ line col                         = make_token Token.Break "break" line col
let continue_ line col                      = make_token Token.Continue "continue" line col
let return_ line col                        = make_token Token.Return "return" line col
let in_ line col                            = make_token Token.In "in" line col
let as_ line col                            = make_token Token.As "as" line col
let unit line col                           = make_token Token.Unit "unit" line col
let left_paren line col                     = make_token Token.LeftParen "(" line col
let right_paren line col                    = make_token Token.RightParen ")" line col
let left_brace line col                     = make_token Token.LeftBrace "{" line col
let right_brace line col                    = make_token Token.RightBrace "}" line col
let left_bracket line col                   = make_token Token.LeftBracket "[" line col
let right_bracket line col                  = make_token Token.RightBracket "]" line col
let comma line col                          = make_token Token.Comma "," line col
let period line col                         = make_token Token.Period "." line col
let colon line col                          = make_token Token.Colon ":" line col
let less_less_equal line col                = make_token Token.LessLessEqual "<<=" line col
let greater_greater_equal line col          = make_token Token.GreaterGreaterEqual ">>=" line col
let period_period_equal line col            = make_token Token.PeriodPeriodEqual "..=" line col
let amp_amp line col                        = make_token Token.AmpAmp "&&" line col
let pipe_pipe line col                      = make_token Token.PipePipe "||" line col
let equal_equal line col                    = make_token Token.EqualEqual "==" line col
let exclaim_equal line col                  = make_token Token.ExclaimEqual "!=" line col
let less_equal line col                     = make_token Token.LessEqual "<=" line col
let greater_equal line col                  = make_token Token.GreaterEqual ">=" line col
let less_less line col                      = make_token Token.LessLess "<<" line col
let greater_greater line col                = make_token Token.GreaterGreater ">>" line col
let plus_equal line col                     = make_token Token.PlusEqual "+=" line col
let minus_equal line col                    = make_token Token.MinusEqual "-=" line col
let star_equal line col                     = make_token Token.StarEqual "*=" line col
let slash_equal line col                    = make_token Token.SlashEqual "/=" line col
let percent_equal line col                  = make_token Token.PercentEqual "%=" line col
let amp_equal line col                      = make_token Token.AmpEqual "&=" line col
let pipe_equal line col                     = make_token Token.PipeEqual "|=" line col
let caret_equal line col                    = make_token Token.CaretEqual "^=" line col
let minus_greater line col                  = make_token Token.MinusGreater "->" line col
let period_period line col                  = make_token Token.PeriodPeriod ".." line col
let equal_greater line col                  = make_token Token.EqualGreater "=>" line col
let question_question line col              = make_token Token.QuestionQuestion "??" line col
let amp line col                            = make_token Token.Amp "&" line col
let pipe line col                           = make_token Token.Pipe "|" line col
let caret line col                          = make_token Token.Caret "^" line col
let tilde line col                          = make_token Token.Tilde "~" line col
let plus line col                           = make_token Token.Plus "+" line col
let minus line col                          = make_token Token.Minus "-" line col
let star line col                           = make_token Token.Star "*" line col
let slash line col                          = make_token Token.Slash "/" line col
let percent line col                        = make_token Token.Percent "%" line col
let question line col                       = make_token Token.Question "?" line col
let at line col                             = make_token Token.At "@" line col
let exclaim line col                        = make_token Token.Exclaim "!" line col
let less line col                           = make_token Token.Less "<" line col
let greater line col                        = make_token Token.Greater ">" line col
let equal line col                          = make_token Token.Equal "=" line col
let illegal lexeme line col                 = make_token Token.Illegal lexeme line col
let eof line col                            = make_token Token.EOF "" line col

let token_equal a b =
    a.Token.kind = b.Token.kind &&
    a.Token.lexeme = b.Token.lexeme &&
    a.Token.pos.line = b.Token.pos.line &&
    a.Token.pos.col = b.Token.pos.col

let token_pp fmt tok =
    Format.fprintf fmt "%s at %d:%d with lexeme: %s"
        (token_to_string tok.Token.kind)
        tok.Token.pos.line
        tok.Token.pos.col
        tok.Token.lexeme

let token_testable = Alcotest.testable token_pp token_equal

let expect_tokens actual expected =
    Alcotest.(check (list token_testable)) "tokens" expected actual