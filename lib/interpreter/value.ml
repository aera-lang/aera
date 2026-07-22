open Frontend.Ast

module StringMap = Map.Make(String)

type value = 
| VInt of int
| VFloat of float
| VChar of char
| VString of string
| VBool of bool
| VFunction of (string * string option) list * expr * environment (* expr = block *)
| VUnit

and environment = value StringMap.t list

let print_value v =
    match v with 
    | VInt n                        -> print_endline (string_of_int n)
    | VFloat f                      -> print_endline (string_of_float f)
    | VBool b                       -> print_endline (string_of_bool b)
    | VChar c                       -> print_endline ("\'" ^ (String.make 1 c) ^ "\'")
    | VString s                     -> print_endline ("\"" ^ s ^ "\"")
    | VFunction (params, expr, env) -> print_endline (" Print the function result!")
    | VUnit                         -> print_endline "unit"

(*
type enclosing = Empty | Enclosing of environment
and environment = { enclosing: enclosing; values: value StringMap.t }
*)