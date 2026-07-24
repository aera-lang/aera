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

let ancestor d env = 
    List.nth_opt env d

let find_at d name env =
    match env |> ancestor d with 
    | None -> None 
    | Some scope -> (match StringMap.find_opt name scope with 
                    | None -> None 
                    | Some value -> Some value)

                    
let rec assign_at d name value env =
    match env |> ancestor d with 
    | None -> None 
    | Some scope -> (match StringMap.find_opt name scope with 
                    | None -> None 
                    | Some value' -> Some value)

                    (* a little tricky
                    Some ( (StringMap.add name value scope) :: rest) 
                    but we can't just car it to the beginning of the list
                    -> has to be similar structure to assign
                    *)

let enter_scope env = 
    StringMap.empty :: env

let leave_scope env =
    match env with 
    | [] -> failwith "the environment should never be empty"
    | _ :: rest -> rest