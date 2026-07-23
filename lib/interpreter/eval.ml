open Frontend
open Value
open Env
open Lexer

(* Helper Functions *)

let get_identifier_name expr =
    match expr with 
    | Frontend.Ast.Identifier name -> Ok (name)
    | _ -> Error ("error: expected identifier")

(* Expression Functions *)

let rec eval_expr expr env =
    match expr with 
    | Frontend.Ast.Literal (LitInt n)                   -> Ok (VInt n, env) (* return type = value, env*)
    | Literal (LitFloat f)                              -> Ok (VFloat f, env)
    | Literal (LitChar c)                               -> Ok (VChar c, env)
    | Literal (LitString s)                             -> Ok (VString s, env)
    | Literal (LitBool b)                               -> Ok (VBool b, env)
    | Identifier name                                   -> env |> eval_identifier name
    | Grouping expr'                                    -> env |> eval_expr expr'
    | Call { callee; args }                             -> env |> eval_call callee args
    | Binary { lhs; op; rhs }                           -> env |> eval_binary lhs op rhs  
    | Assign { lhs; op; rhs }                           -> env |> eval_assign lhs op rhs       
    | Unary { op; rhs }                                 -> env |> eval_unary op rhs
    | Block { stmts; expr }                             -> env |> eval_block stmts expr
    | InfiniteLoop expr'                                -> env |> eval_infinite_loop expr
    | WhileLoop { cond; body }                          -> env |> eval_while_loop cond body
    | IfExpr { cond; then_branch; else_branch }         -> env |> eval_if cond then_branch else_branch
    | BreakExpr expr'                                   -> env |> eval_break expr'    
    | ReturnExpr expr'                                  -> env |> eval_return expr'                                                      

and eval_infinite_loop expr env = (* DOESN'T SUPPORT BREAK YET, DON'T WANNA HAVE TO DEAL WITH IT RN 
                                    -> will have to propagate break and eventually return from eval_expr to
                                    eval_stmt to eval_block_helper to eval_block to eval_if / eval_while / eval_for and eval_fn
                                    -> a lot of work that I don't want to deal with rn *)             
    match expr with 
    | Block { stmts; expr } -> 
        begin 
            match env |> eval_block stmts expr with
            | Error e -> Error e 
            | Ok (_, env') -> env' |> eval_infinite_loop expr
        end
    | _ -> Error ("error: expected block expression")

and eval_while_loop cond body env =
    match env |> eval_expr cond with 
    | Error e -> Error e
    | Ok (VBool false, _) -> Ok (VUnit, env)
    | Ok (VBool true, _) ->
        begin 
            match body with 
            | Block { stmts; expr } ->
                begin 
                    match env |> eval_block stmts expr with 
                    | Error e -> Error e
                    | Ok (_, env') -> env' |> eval_while_loop cond body
                    end
            | _ -> Error ("error: expected block expression")
        end
    | Ok (_, _) -> Error ("error: condition must evaluate to a boolean value")

and eval_if cond then_branch else_branch env = 
    match env |> eval_expr cond with 
    | Error e -> Error e
    | Ok (VBool true, _) ->
        begin 
            match then_branch with 
            | Block { stmts; expr } ->
                begin 
                    match env |> eval_block stmts expr with 
                    | Error e -> Error e
                    | Ok (value, env') -> Ok (value, env') 
                    end
            | _ -> Error ("error: expected block expression")
        end
    | Ok (VBool false, _) -> 
        begin 
            match else_branch with 
            | None -> Ok (VUnit, env)
            | Some body -> 
                begin
                    match env |> eval_else body with 
                    | Error e -> Error e
                    | Ok (value, env') -> Ok (value, env')
                end
        end
    | Ok (_, _) -> Error ("error: condition must evaluate to a boolean value")
              
and eval_else body env =
    match body with 
    | Block { stmts; expr } ->
        begin 
            match env |> eval_block stmts expr with 
            | Error e -> Error e
            | Ok (value, env') -> Ok (value, env') 
        end
    | _ -> Error ("error: expected block expression")

and eval_break expr env =
    match expr with 
    | None -> Ok (VUnit, env)
    | Some expr' ->
        begin 
            match env |> eval_expr expr' with 
            | Error e -> Error e
            | Ok (value, _) -> Ok (value, env)
        end

and eval_return expr env =
    match expr with 
    | None -> Ok (VUnit, env)
    | Some expr' ->
        begin 
            match env |> eval_expr expr' with 
            | Error e -> Error e
            | Ok (value, _) -> Ok (value, env)
        end

and eval_block stmts expr env =
    let env' = enter_scope env in 
    match env' |> eval_block_helper stmts with (* handles binding *)
    | Error e -> Error e
    | Ok (_, env'') -> 
        begin
            match env'' |> eval_expr expr with
            | Error e -> Error e
            | Ok (value, _) -> Ok(value, env) (* restores state to BEFORE the block expression *)
        end
   
and eval_block_helper stmts env =
    match stmts with
    | [] -> Ok (VUnit, env)
    | [ stmt ] -> env |> eval_stmt stmt
    | stmt :: rest -> 
            begin 
                match env |> eval_stmt stmt with 
                | Error e -> Error e 
                | Ok (_, env') -> env' |> eval_block_helper rest
            end

and lvalue expr env = (* for now, an lvalue is an identifier, but field access and eventually array access are lvalues *)
    match expr with 
    | Frontend.Ast.Identifier name -> env |> eval_identifier name
    | _ -> Error ("error: expected identifier as lvalue")

(* Statement Functions *)

and eval_stmt stmt env =
    match stmt with 
    | Frontend.Ast.LetStmt { name; typ; expr; } -> env |> eval_let name typ expr

and eval_let name typ expr env = (* for now, we are ignoring the typ annotation 
                                        because we haven't implemented a type checker,
                                        and it adds unecessary complexitiy *)
    match env |> eval_expr expr with
    | Error e -> Error e
    | Ok (value, _) -> 
        Ok (VUnit, env |> bind name value)

and eval_call callee args env = 
    match env |> eval_expr callee with 
    | Error e -> Error e
    | Ok (callee', _) ->
        begin 
            match callee' with 
            | VFunction (params, body, closure_env, Some name) -> 
                if List.length params <> List.length args then
                    Error ("error: arity mismatch")
                else
                    begin
                        match env |> eval_call_helper args [] with 
                        | Error e -> Error e
                        | Ok (values, _) ->
                            let fn_env = enter_scope closure_env in 
                            let fn_env = bind name callee' fn_env in
                            let fn_env' = fn_env |> bind_params params values in
                            begin
                                match body with 
                                | Block { stmts; expr } -> 
                                    begin 
                                        match fn_env' |> eval_block stmts expr with
                                        | Error e -> Error e 
                                        | Ok (value, _) -> Ok (value, env) (* return value can be a Unit *)
                                    end
                                | _ -> Error ("error: expected block expression")    
                            end
                    end
            | VFunction (params, body, closure_env, None) -> (* anonymous function -> don't have the syntax yet *)
                if List.length params <> List.length args then
                    Error ("error: arity mismatch")
                else
                    begin
                        match env |> eval_call_helper args [] with 
                        | Error e -> Error e
                        | Ok (values, _) ->
                            let fn_env = enter_scope closure_env in 
                            let fn_env' = fn_env |> bind_params params values in
                            begin
                                match body with 
                                | Block { stmts; expr } -> 
                                    begin 
                                        match fn_env' |> eval_block stmts expr with
                                        | Error e -> Error e 
                                        | Ok (value, _) -> Ok (value, env) (* return value can be a Unit *)
                                    end
                                | _ -> Error ("error: expected block expression")    
                            end
                    end
            | _ -> Error ("error: not a callable function")
            end
      
and bind_params params values env =
    match params, values with 
    | [], [] -> env
    | (name, _) :: rest, value :: rest' -> (* ignoring type annotations rn *)
        let env' = env |> bind name value 
        in env' |> bind_params rest rest'
    | _ -> failwith "both lists must be the same size and are checked beforehand"

and eval_call_helper args eval_args env =
    match args with 
    | [] -> Ok (List.rev eval_args, env)
    | h :: t -> 
        begin 
            match env |> eval_expr h with 
            | Error e -> Error e
            | Ok (value, _) -> env |> eval_call_helper t (value :: eval_args)
        end

(* Identifier Function *)

and eval_identifier name env =
    match env |> find name with 
    | None -> Error ("error: variable not bound to a value")
    | Some value -> Ok (value, env)

(* Binary Functions *)

and eval_binary lhs op rhs env =
    let l = env |> eval_expr lhs in 
    let r = env |> eval_expr rhs in 
    match l, r with
    | Ok (VInt a, _), Ok (VInt b, _)                          -> env |> eval_binary_int a op b
    | Ok (VFloat a, _), Ok (VFloat b, _)                      -> env |> eval_binary_float a op b
    | Ok (VChar a, _), Ok (VChar b, _)                        -> env |> eval_binary_char a op b
    | Ok (VString a, _), Ok (VString b, _)                    -> env |> eval_binary_string a op b
    | Ok (VBool a, _), Ok (VBool b, _)                        -> env |> eval_binary_bool a op b
    | _                                                 -> Error ("error: expected int, float, char, string or bool literal")

and eval_binary_int lhs op rhs env =
    match op with
    | Add       -> Ok (VInt (lhs + rhs), env)
    | Sub       -> Ok (VInt (lhs - rhs), env)
    | Mul       -> Ok (VInt (lhs * rhs), env)
    | Div       -> if rhs = 0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VInt (lhs / rhs), env)
    | Mod       -> if rhs = 0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VInt (lhs mod rhs), env)     
    | Eq        -> Ok (VBool (lhs = rhs), env)
    | Neq       -> Ok (VBool (lhs <> rhs), env)
    | Lt        -> Ok (VBool (lhs < rhs), env)
    | Lte       -> Ok (VBool (lhs <= rhs), env)
    | Gt        -> Ok (VBool (lhs > rhs), env)
    | Gte       -> Ok (VBool (lhs >= rhs), env)
    | BitAnd    -> Ok (VInt (lhs land rhs), env)
    | BitOr     -> Ok (VInt (lhs lor rhs), env)
    | BitXor    -> Ok (VInt (lhs lxor rhs), env)
    | Shl       -> Ok (VInt (lhs lsl rhs), env)
    | Shr       -> Ok (VInt (lhs asr rhs), env)
    | _         -> Error ("error: invalid operation on an int value")

and eval_binary_float lhs op rhs env =
    match op with
    | Add       -> Ok (VFloat (lhs +. rhs), env)
    | Sub       -> Ok (VFloat (lhs -. rhs), env)
    | Mul       -> Ok (VFloat (lhs *. rhs), env)
    | Div       -> if rhs = 0.0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VFloat (lhs /. rhs), env)
    | Mod       -> if rhs = 0.0 then Error ("error: division by zero is undefined")
                    else
                    Ok (VFloat (mod_float lhs rhs), env)     
    | Eq        -> Ok (VBool (lhs = rhs), env)
    | Neq       -> Ok (VBool (lhs <> rhs), env)
    | Lt        -> Ok (VBool (lhs < rhs), env)
    | Lte       -> Ok (VBool (lhs <= rhs), env)
    | Gt        -> Ok (VBool (lhs > rhs), env)
    | Gte       -> Ok (VBool (lhs >= rhs), env)
    | _         -> Error ("error: invalid operation on a floating point value")

and eval_binary_char lhs op rhs env =
    match op with    
    | Eq        -> Ok (VBool (lhs = rhs), env)
    | Neq       -> Ok (VBool (lhs <> rhs), env)
    | _         -> Error ("error: invalid operation on a char value")

and eval_binary_string lhs op rhs env =
    match op with
    | Add       -> Ok (VString (lhs ^ rhs), env)
    | Eq        -> Ok (VBool (lhs = rhs), env)
    | Neq       -> Ok (VBool (lhs <> rhs), env)
    | _         -> Error ("error: invalid operation on a string value")

and eval_binary_bool lhs op rhs env =
    match op with
    | Eq        -> Ok (VBool (lhs = rhs), env)
    | Neq       -> Ok (VBool (lhs <> rhs), env)
    | Lt        -> Ok (VBool (lhs < rhs), env)
    | Lte       -> Ok (VBool (lhs <= rhs), env)
    | Gt        -> Ok (VBool (lhs > rhs), env)
    | Gte       -> Ok (VBool (lhs >= rhs), env)
    | And       -> Ok (VBool (lhs && rhs), env)
    | Or        -> Ok (VBool (lhs || rhs), env)
    | _         -> Error ("error: invalid operation on a boolean value")


(* Assign Functions*)
and eval_assign lhs op rhs env =
    let l = env |> lvalue lhs in 
    let r = (match rhs with 
    | Identifier _ -> env |> lvalue rhs
    | _ -> env |> eval_expr rhs) in 
    match l, r with 
    | Ok (VInt a, _), Ok (VInt b, _)                          -> env |> eval_assign_unit lhs (eval_assign_int a op b) 
    | Ok (VFloat a, _), Ok (VFloat b, _)                      -> env |> eval_assign_unit lhs (eval_assign_float a op b) 
    | Ok (VString a, _), Ok (VString b, _)                    -> env |> eval_assign_unit lhs (eval_assign_string a op b) 
    | _                                                       -> Error ("error: expected int, float or string literal")

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
            | Ok value -> begin
                    match env |> assign name value with 
                    | None -> Error("error: undefined variable name") (* probably will need to return env WITH error *)
                    | Some env' -> Ok (VUnit, env') 
                    end
            end

(* Unary Functions *)
and eval_unary op rhs env = 
    let r = env |> eval_expr rhs in 
    match r with
    | Ok (VInt n, _)       -> env |> eval_unary_int op n
    | Ok (VFloat f, _)     -> env |> eval_unary_float op f
    | Ok (VBool b, _)      -> env |> eval_unary_bool op b
    | _ -> Error ("error: invalid unary operation")

and eval_unary_int op rhs env =
    match op with 
    | Neg -> Ok (VInt (-rhs), env)
    | _ -> Error ("error: invalid unary operation on an int value")

and eval_unary_float op rhs env =
    match op with 
    | Neg -> Ok (VFloat (-.rhs), env)
    | _ -> Error ("error: invalid unary operation on a floating point value")

and eval_unary_bool op rhs env =
    match op with 
    | Not -> Ok (VBool (not rhs), env)
    | _ -> Error ("error: invalid unary operation on a floating point value")

(* Item Functions *)

and eval_const name typ expr env = (* similar to let statement *)
    match env |> eval_expr expr with
    | Error e -> Error e
    | Ok (value, _) -> 
        Ok (VUnit, env |> bind name value)  

and eval_fn name params return_type body env = (* note return type is a TYPE ANNOTATION, will handle it later *)
    let value = VFunction (params, body, env, Some name) in
    Ok (value, env |> bind name value) (* currently Result type *)

and eval_item item env =
    match item with 
    | Frontend.Ast.ConstItem { name; typ; expr }         -> env |> eval_const name typ expr
    | FnItem { name; params; return_type; body }         -> env |> eval_fn name params return_type body
    | _ -> failwith "not implemented yet"

(* Eval Function *)

let rec eval items env = 
    match items with 
    | [] -> Ok (VUnit, env)
    | [ item ] -> 
            begin
                match env |> eval_item item with 
                | Error e -> Error e
                | Ok (value, env') -> Ok (value, env')
            end
    | item :: rest -> 
        begin
            match env |> eval_item item with 
            | Error e -> Error e
            | Ok (_, env') -> env' |> eval rest
        end