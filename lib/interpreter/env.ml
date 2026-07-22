open Frontend.Token
open Value

let bind name value env = (* similar to DEFINE *)
    match env with 
    | [] -> failwith "the environment should never be empty"
    | scope :: rest -> (StringMap.add name value scope) :: rest (* adds a binding to the CURRENT scope *)

let rec assign name value env = (* similar to ASSIGN
                                NOTE: this makes the language MUTABLE
                                Need to only use this if 'mut' keyword is applied,
                                otherwise, need new binding *)
    match env with 
    | [] -> None
    | scope :: rest -> (match StringMap.find_opt name scope with
                        | Some value' -> Some ( (StringMap.add name value scope) :: rest) 
                        | None -> assign name value rest)

let rec find name env = (* similar to GET *)
    match env with
    | [] -> None (* returns None but should probably be an error *)
    | scope :: rest -> (match StringMap.find_opt name scope with
                    | Some value -> Some value
                    | None -> find name rest)


let enter_scope env = 
    match env with 
    | [] -> failwith "the environment should never be empty"
    | scope :: rest -> StringMap.empty :: rest (* creates an empty scope *)

let leave_scope env =
    match env with 
    | [] -> failwith "the environment should never be empty"
    | _ :: rest -> rest