open Frontend.Ast

module StringMap = Map.Make(String)

type value = 
| VInt of int
| VFloat of float
| VChar of char
| VString of string
| VBool of bool
| VFunction of (string * string option) list * expr * environment * string option (* expr = block *)
| VUnit

and environment = value StringMap.t list

let string_of_param (name, ty) =
    match ty with
    | Some t -> name ^ ": " ^ t
    | None -> name

let print_value v =
    match v with 
    | VInt n                                    -> print_endline (string_of_int n)
    | VFloat f                                  -> print_endline (string_of_float f)
    | VBool b                                   -> print_endline (string_of_bool b)
    | VChar c                                   -> print_endline ("\'" ^ (String.make 1 c) ^ "\'")
    | VString s                                 -> print_endline ("\"" ^ s ^ "\"")
    | VFunction (params, expr, env, Some name) ->
        print_endline
            ("<function " ^ name ^ "(" ^
            (String.concat ", " (List.map string_of_param params)) ^
            ")>")

    | VFunction (params, expr, env, None) ->
        print_endline
            ("<anonymous function(" ^
            (String.concat ", " (List.map string_of_param params)) ^
            ")>")
    | VUnit                                     -> print_endline "unit"