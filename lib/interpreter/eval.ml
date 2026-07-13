open Frontend
open Value
open Env
open Lexer

(* Helper Functions *)

let get_identifier_name expr =
    match expr with 
    | Frontend.Ast.Identifier name -> Ok (name)
    | _ -> Error ("error: expected identifier.")

let rec eval_expr expr env =
    match expr with 
    | Frontend.Ast.Literal (LitInt n)                   -> Ok (VInt n)
    | Literal (LitFloat f)                              -> Ok (VFloat f)
    | Literal (LitChar c)                               -> Ok (VChar c)
    | Literal (LitString s)                             -> Ok (VString s)
    | Literal (LitBool b)                               -> Ok (VBool b)
    | Identifier name                                   -> env |> eval_identifier name
    | Grouping expr'                                    -> env |> eval_expr expr'
    (*| Call { callee; args }                             -> env |> eval_call callee args*)
    | Binary { lhs; op; rhs }                           -> env |> eval_binary lhs op rhs  
    | Assign { lhs; op; rhs }                           -> env |> eval_assign lhs op rhs       
    | Unary { op; rhs }                                 -> env |> eval_unary op rhs
    | _ -> failwith "not implemented yet"

and lvalue expr env = (* for now, an lvalue is an identifier, but field access and eventually array access are lvalues *)
    match expr with 
    | Frontend.Ast.Identifier name -> env |> eval_identifier name
    | _ -> Error ("error: expected identifier as lvalue.")

and eval_stmt stmt env =
    match stmt with 
    | Frontend.Ast.LetStmt { name; typ; expr; } -> env |> eval_let_stmt name typ expr
    | ConstStmt { name; typ; expr; } -> env |> eval_const_stmt name typ expr

and eval_let_stmt name typ expr env = (* for now, we are ignoring the typ annotation 
                                        because we haven't implemented a type checker,
                                        and it adds unecessary complexitiy *)
    match expr with 
    | None -> Error("idk")
    | Some expr' -> 
        (match env |> eval_expr expr' with
        | Error e -> Error e
        | Ok value -> 
            Ok (env |> bind name value)) (* return the UNIT type and env *)

and eval_const_stmt name typ expr env = (* same as let statement *)
    match env |> eval_expr expr with
    | Error e -> Error e
    | Ok value -> 
        Ok (env |> bind name value)

(* and eval_call callee args env = 
    let callee' = env |> eval_expr callee in
    let args' = env |> eval_call_helper args [] in 
    (* somehow do something with this *)
    (* need to call a function *)



and eval_call_helper args eval_args env =
    match args with 
    | [] -> eval_args
    | [ h ] -> env |> eval_expr h :: eval_args
    | h :: t -> env |> eval_expr h :: env |> eval_call_helper args t *)


(* Binary Functions *)
and eval_binary lhs op rhs env =
    let l = env |> eval_expr lhs in 
    let r = env |> eval_expr rhs in 
    match l, r with
    | Ok (VInt a), Ok (VInt b)                          -> eval_binary_int a op b
    | Ok (VFloat a), Ok (VFloat b)                      -> eval_binary_float a op b
    | Ok (VChar a), Ok (VChar b)                        -> eval_binary_char a op b
    | Ok (VString a), Ok (VString b)                    -> eval_binary_string a op b
    | Ok (VBool a), Ok (VBool b)                        -> eval_binary_bool a op b
    | _                                                 -> Error ("error: expected int, float, char, string or bool literal")

and eval_binary_int lhs op rhs =
    match op with
    | Add       -> Ok (VInt (lhs + rhs))
    | Sub       -> Ok (VInt (lhs - rhs))
    | Mul       -> Ok (VInt (lhs * rhs))
    | Div       -> if rhs = 0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VInt (lhs / rhs))
    | Mod       -> if rhs = 0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VInt (lhs mod rhs))     
    | Eq        -> Ok (VBool (lhs = rhs))
    | Neq       -> Ok (VBool (lhs <> rhs))
    | Lt        -> Ok (VBool (lhs < rhs))
    | Lte       -> Ok (VBool (lhs <= rhs))
    | Gt        -> Ok (VBool (lhs > rhs))
    | Gte       -> Ok (VBool (lhs >= rhs))
    | BitAnd    -> Ok (VInt (lhs land rhs))
    | BitOr     -> Ok (VInt (lhs lor rhs))
    | BitXor    -> Ok (VInt (lhs lxor rhs))
    | Shl       -> Ok (VInt (lhs lsl rhs))
    | Shr       -> Ok (VInt (lhs asr rhs))
    | _         -> Error ("error: invalid operation on an int value")

and eval_binary_float lhs op rhs =
    match op with
    | Add       -> Ok (VFloat (lhs +. rhs))
    | Sub       -> Ok (VFloat (lhs -. rhs))
    | Mul       -> Ok (VFloat (lhs *. rhs))
    | Div       -> if rhs = 0.0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VFloat (lhs /. rhs))
    | Mod       -> if rhs = 0.0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VFloat (mod_float lhs rhs))     
    | Eq        -> Ok (VBool (lhs = rhs))
    | Neq       -> Ok (VBool (lhs <> rhs))
    | Lt        -> Ok (VBool (lhs < rhs))
    | Lte       -> Ok (VBool (lhs <= rhs))
    | Gt        -> Ok (VBool (lhs > rhs))
    | Gte       -> Ok (VBool (lhs >= rhs))
    | _         -> Error ("error: invalid operation on a floating point value")

and eval_binary_char lhs op rhs =
    match op with    
    | Eq        -> Ok (VBool (lhs = rhs))
    | Neq       -> Ok (VBool (lhs <> rhs))
    | _         -> Error ("error: invalid operation on a char value")

and eval_binary_string lhs op rhs =
    match op with
    | Add       -> Ok (VString (lhs ^ rhs))
    | Eq        -> Ok (VBool (lhs = rhs))
    | Neq       -> Ok (VBool (lhs <> rhs))
    | _         -> Error ("error: invalid operation on a string value")

and eval_binary_bool lhs op rhs =
    match op with
    | Eq        -> Ok (VBool (lhs = rhs))
    | Neq       -> Ok (VBool (lhs <> rhs))
    | Lt        -> Ok (VBool (lhs < rhs))
    | Lte       -> Ok (VBool (lhs <= rhs))
    | Gt        -> Ok (VBool (lhs > rhs))
    | Gte       -> Ok (VBool (lhs >= rhs))
    | And       -> Ok (VBool (lhs && rhs))
    | Or        -> Ok (VBool (lhs || rhs))
    | _         -> Error ("error: invalid operation on a boolean value")


(* Assign Functions*)
and eval_assign lhs op rhs env =
    let l = env |> lvalue lhs in 
    let r = (match rhs with 
    | Identifier _ -> env |> lvalue rhs
    | _ -> env |> eval_expr rhs) in 
    match l, r with 
    | Ok (VInt a), Ok (VInt b)                          -> env |> eval_assign_unit lhs (eval_assign_int a op b) 
    | Ok (VFloat a), Ok (VFloat b)                      -> env |> eval_assign_unit lhs (eval_assign_float a op b) 
    | Ok (VString a), Ok (VString b)                    -> env |> eval_assign_unit lhs (eval_assign_string a op b) 
    | _                                                 -> Error ("error: expected int, float or string literal")

and eval_assign_int lhs op rhs =
    match op with
    | EqAssign      -> Ok(VInt(rhs)) (* reassign rhs value to lhs *)
    | AddAssign     -> Ok (VInt (lhs + rhs))
    | SubAssign     -> Ok (VInt (lhs - rhs))
    | MulAssign     -> Ok (VInt (lhs * rhs))
    | DivAssign     -> if rhs = 0 then Error ("error: division by zero is undefined")
                       else
                       Ok (VInt (lhs / rhs))
    | ModAssign     -> if rhs = 0 then Error ("error: division by zero is undefined")
                       else
                       Ok (VInt (lhs mod rhs))     

    | AndAssign    -> Ok (VInt (lhs land rhs))
    | OrAssign     -> Ok (VInt (lhs lor rhs))
    | XorAssign    -> Ok (VInt (lhs lxor rhs))
    | ShlAssign     -> Ok (VInt (lhs lsl rhs))
    | ShrAssign     -> Ok (VInt (lhs asr rhs))

and eval_assign_float lhs op rhs =
    match op with
    | EqAssign      -> Ok(VFloat(rhs)) (* reassign rhs value to lhs *)
    | AddAssign     -> Ok (VFloat (lhs +. rhs))
    | SubAssign     -> Ok (VFloat (lhs -. rhs))
    | MulAssign     -> Ok (VFloat (lhs *. rhs))
    | DivAssign     -> if rhs = 0.0 then Error ("error: division by zero is undefined")
                       else
                       Ok (VFloat (lhs /. rhs))
    | ModAssign     -> if rhs = 0.0 then Error ("error: division by zero is undefined")
                       else
                       Ok (VFloat (mod_float lhs rhs))     

    | _         -> Error ("error: invalid operation on a floating point value")

and eval_assign_string lhs op rhs =
    match op with
    | AddAssign  -> Ok (VString (lhs ^ rhs))
    | _         -> Error ("error: invalid operation on a string value")

and eval_assign_unit lhs res env =
    match get_identifier_name lhs with 
    | Error e -> Error e 
    | Ok name -> 
        begin
            match res with 
            | Error e -> Error e 
            | Ok value -> let env' = env |> assign name value
                in Ok (VUnit) (* value, env *)
            end

(* Unary Functions *)
and eval_unary op rhs env = 
    let r = env |> eval_expr rhs in 
    match r with
    | Ok (VInt n)       -> n |> eval_unary_int op
    | Ok (VFloat f)     -> f |> eval_unary_float op
    | Ok (VBool b)      -> b |> eval_unary_bool op
    | _ -> Error ("error: invalid unary operation")


and eval_unary_int op rhs =
    match op with 
    | Neg -> Ok (VInt (-rhs))
    | _ -> Error ("error: invalid unary operation on an int value")

and eval_unary_float op rhs =
    match op with 
    | Neg -> Ok (VFloat (-.rhs))
    | _ -> Error ("error: invalid unary operation on a floating point value")

and eval_unary_bool op rhs =
    match op with 
    | Not -> Ok (VBool (not rhs))
    | _ -> Error ("error: invalid unary operation on a floating point value")

and eval_identifier name env =
    match env |> find name with 
    | None -> Error ("error: variable not bound to a value")
    | Some value -> Ok (value)