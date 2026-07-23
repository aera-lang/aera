(*

RESOLVER IS GONNA HAVE TO BE DIFFERENT

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

let resolve_expr expr scopes = ()

and resolve_stmt stmt scopes =
    match stmt with 
    | Ast.LetStmt { name; typ; expr; } -> scopes |> resolve_let name typ expr

and resolve_let name typ expr env = (* for now, we are ignoring the typ annotation 
                                        because we haven't implemented a type checker,
                                        and it adds unecessary complexitiy *)
    match env |> eval_expr expr with
    | Error e -> Error e
    | Ok (value, _) -> 
        Ok (VUnit, env |> bind name value)

and resolve_item item scopes =
    match item with 
    | Ast.ConstItem { name; typ; expr }                 -> scopes |> resolve_const name typ expr
    | FnItem { name; params; return_type; body }        -> scopes |> resolve_fn name params return_type body
    | _ -> failwith "not implemented yet"

let rec resolve items scopes = 
    match items with 
    | [] -> failwith "the program cannot be empty."
    | [ item ] -> 
            begin
                match scopes |> resolve_item item with 
                | Error e -> Error e
                | Ok (value, scopes') -> Ok (value, scopes')
            end
    | item :: rest -> 
        begin
            match scopes |> resolve_item item with 
            | Error e -> Error e
            | Ok (_, scopes') -> scopes' |> resolve rest
        end
 *)
