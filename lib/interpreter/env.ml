open Frontend.Token
open Value

let bind name value env = (* similar to the define function in Crafting Interpreters *)
    match env with 
    | [] -> failwith "the environment should never be empty"
    | scope :: rest -> (StringMap.add name value scope) :: rest (* adds a binding to the CURRENT scope *)

let rec find name env =
    match env with
    | [] -> None
    | scope :: rest -> (match StringMap.find_opt name scope with
                    | Some value -> Some value
                    | None -> find name rest)