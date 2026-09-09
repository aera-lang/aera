open Ast
open Reporter
open Lexer
open Token
open Op
   
type t = {
    tokens: Token.t list;
    reporter: Reporter.t;
}

(* Helper Functions *)

let create tok rep = {
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
    if is_at_end par then false
    else 
        let token = peek par in token.kind = kind

(* *)