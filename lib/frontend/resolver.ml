open Token 

module StringMap = Map.Make(String)

type scopes = bool StringMap.t list

(* scope = Map <String, Bool>*)

module Stack = struct
    type 'a stack = 'a list
    let empty = []
    let push x s = x :: s
    let pop s = match s with 
                | [] -> None 
                | h :: t -> Some(h, t)
    let peek s = match s with 
                | [] -> None 
                | h :: _ -> Some(h)
    let is_empty s = match s with
                | [] -> true
                | _ -> false
end


let begin_scope scopes = 
    Stack.push StringMap.empty scopes 

let end_scope scopes =
    Stack.pop scopes 


let declare name scopes = 
    if Stack.is_empty scopes then 
        Ok (scopes) (* return early *)
    else
        match Stack.peek scopes with 
        | None -> Ok (scopes) (* idk if this is right *)
        | Some scope ->
            begin
                match StringMap.find_opt name scope with 
                | None -> Ok (Stack.push (StringMap.add name false scope) scopes)
                | Some name' -> Error ("error: a variable already exists with this name in this scope")
            end 
        
let define name scopes = 
    if Stack.is_empty scopes then 
        scopes (* return early *)
    else
        match Stack.peek scopes with 
        | None -> scopes (* idk if this is right *)
        | Some scope -> Stack.push (StringMap.add name false scope) scopes

let resolve_expr = () (* follow structure of eval_expr *)
let resolve_stmt = () (* follow structure of eval_stmt *)

let resolve_item = () (* follow structure of eval_item *)

let resolve = () (* follow structure of eval *)