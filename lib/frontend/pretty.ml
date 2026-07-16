open Ast
open Parser

let connector is_last = (* the connector for the node -> if the last node, the connector has a curve *)
    if is_last then
        "└──"
    else 
        "├──"

let child_prefix prefix is_last =
    if is_last then
        prefix ^ "    "
    else
        prefix ^ "│   "

let format_binary_op op =
    match op with 
    | Add       -> "+"
    | Sub       -> "-"
    | Mul       -> "*"
    | Div       -> "/"
    | Mod       -> "%"
    | Eq        -> "=="
    | Neq       -> "!="
    | Lt        -> "<"
    | Lte       -> "<="
    | Gt        -> ">"
    | Gte       -> ">="
    | And       -> "&&"
    | Or        -> "||"
    | BitAnd    -> "&"
    | BitOr     -> "|"
    | BitXor    -> "^"
    | Shl       -> "<<"
    | Shr       -> ">>"

let format_unary_op op =
    match op with 
    | Neg   -> "-"
    | Not   -> "!"

let format_assign_op op =
    match op with 
    | EqAssign          -> "="
    | AddAssign         -> "+="
    | SubAssign         -> "-="
    | MulAssign         -> "*="
    | DivAssign         -> "/="
    | ModAssign         -> "%="
    | AndAssign         -> "&="
    | OrAssign          -> "|="
    | XorAssign         -> "^="
    | ShlAssign         -> "<<="
    | ShrAssign         -> ">>="

let format_literal prefix is_last value =
    [ Printf.sprintf "%s%s Literal %s" prefix (connector is_last) value ]

let format_identifier prefix is_last value = 
    [ Printf.sprintf "%s%s Identifier %s" prefix (connector is_last) value]

let rec format_expr prefix is_last expr = 
    match expr with
    | Literal (LitInt n)                                -> format_literal prefix is_last (string_of_int n)
    | Literal (LitFloat f)                              -> format_literal prefix is_last (string_of_float f)
    | Literal (LitChar c)                               -> format_literal prefix is_last (String.make 1 c)
    | Literal (LitString s)                             -> format_literal prefix is_last s
    | Literal (LitBool b)                               -> format_literal prefix is_last (Bool.to_string b)
    | Identifier s                                      -> format_identifier prefix is_last s
    | Grouping expr'                                    -> format_grouping expr' prefix is_last
    | Call { callee; args }                             -> format_call callee args prefix is_last
    | Binary { lhs; op; rhs }                           -> format_binary lhs op rhs prefix is_last
    | Assign { lhs; op; rhs }                           -> format_assign lhs op rhs prefix is_last
    | Unary { op; rhs }                                 -> format_unary op rhs prefix is_last
    | Block { stmts; expr; }                            -> format_block stmts expr prefix is_last
    | InfiniteLoop expr'                                -> format_infinite_loop expr' prefix is_last
    | WhileLoop { cond; body; }                         -> format_while_loop cond body prefix is_last
    | IfExpr { cond; then_branch; else_branch; }        -> format_if cond then_branch else_branch prefix is_last
    | BreakExpr expr'                                   -> format_break expr' prefix is_last
    | ReturnExpr expr'                                  -> format_return expr' prefix is_last                    

and format_grouping expr prefix is_last =
    let group_lst = [ Printf.sprintf "%s%s Group" prefix (connector is_last) ] in 
    let expr_lst = expr |> format_expr (child_prefix prefix is_last) true in
    group_lst @ expr_lst

and format_call callee args prefix is_last =
    let call_lst = [ Printf.sprintf "%s%s Call" prefix (connector is_last) ] in
    let callee_lst = callee |> format_expr (child_prefix prefix is_last) false in
    let args_lst = format_call_helper args prefix in
    call_lst @ callee_lst @ args_lst

and format_call_helper args prefix =
    match args with 
    | [] -> []
    | [ arg ] -> arg |> format_expr (child_prefix prefix true) true (* at the last arg *)
    | h :: t -> (h |> format_expr (child_prefix prefix false) false) @ format_call_helper t prefix (* concat each arg with the rest of the args *)

and format_binary lhs op rhs prefix is_last =
    let binary_lst = [ Printf.sprintf "%s%s Binary op = \"%s\"" prefix (connector is_last) (format_binary_op op) ] in 
    let lhs_lst = lhs |> format_expr (child_prefix prefix is_last) false in
    let rhs_lst = rhs |> format_expr (child_prefix prefix is_last) true in
    binary_lst @ lhs_lst @ rhs_lst

and format_assign lhs op rhs prefix is_last =
    let assign_lst = [ Printf.sprintf "%s%s Assign op = \"%s\"" prefix (connector is_last) (format_assign_op op) ] in 
    let lhs_lst = lhs |> format_expr (child_prefix prefix is_last) false in
    let rhs_lst = rhs |> format_expr (child_prefix prefix is_last) true in
    assign_lst @ lhs_lst @ rhs_lst

and format_unary op rhs prefix is_last = 
    let unary_lst = [ Printf.sprintf "%s%s Unary op = \"%s\"" prefix (connector is_last) (format_unary_op op) ] in
    let rhs_lst = rhs |> format_expr (child_prefix prefix is_last) true in
    unary_lst @ rhs_lst

and format_block stmts expr prefix is_last = 
    let block_lst = [ Printf.sprintf "%s%s Block" prefix (connector is_last) ] in
    let stmts_lst = format_block_helper stmts (child_prefix prefix is_last) false in
    let expr_lst = expr |> format_expr (child_prefix prefix is_last) true in
    block_lst @ stmts_lst @ expr_lst

and format_block_helper stmts prefix is_last = 
    match stmts with 
    | [] -> []
    | [ stmt ] -> stmt |> format_stmt prefix true
    | h :: t -> (h |> format_stmt prefix is_last) @ format_block_helper t prefix is_last

and format_infinite_loop expr prefix is_last =
    let infinite_loop_lst = [ Printf.sprintf "%s%s Loop" prefix (connector is_last) ] in 
    let expr_lst = expr |> format_expr (child_prefix prefix is_last) true in
    infinite_loop_lst @ expr_lst

and format_while_loop cond body prefix is_last =
    let while_loop_lst = [ Printf.sprintf "%s%s While" prefix (connector is_last) ] in
    let cond_lst = cond |> format_expr (child_prefix prefix is_last) false in 
    let body_lst = body |> format_expr (child_prefix prefix is_last) true in 
    while_loop_lst @ cond_lst @ body_lst

and format_if cond then_branch else_branch prefix is_last =
    let if_lst = [ Printf.sprintf "%s%s If" prefix (connector is_last) ] in
    let cond_lst = cond |> format_expr (child_prefix prefix is_last) false in
    match else_branch with 
    | Some else_expr -> let then_lst = then_branch |> format_expr (child_prefix prefix is_last) false in 
                        let else_lst = else_expr |> format_expr (child_prefix prefix is_last) true in 
                        if_lst @ cond_lst @ then_lst @ else_lst 
    | None -> let then_lst = then_branch |> format_expr (child_prefix prefix is_last) true in 
    if_lst @ cond_lst @ then_lst

and format_break expr_opt prefix is_last  = 
    let break_lst = [ Printf.sprintf "%s%s Break" prefix (connector is_last) ] in 
    match expr_opt with 
    | Some expr -> let expr_lst = expr |> format_expr (child_prefix prefix is_last) true in
                    break_lst @ expr_lst
    | None -> break_lst

and format_return expr_opt prefix is_last = 
    let return_lst = [ Printf.sprintf "%s%s Return" prefix (connector is_last) ] in 
    match expr_opt with 
    | Some expr -> let expr_lst = expr |> format_expr (child_prefix prefix is_last) true in
                    return_lst @ expr_lst
    | None -> return_lst

and format_stmt prefix is_last stmt  = 
    match stmt with 
    | LetStmt { name; typ; expr }           -> format_let name typ expr prefix is_last

and format_let name typ expr prefix is_last =
    let let_lst = [ Printf.sprintf "%s%s Let" prefix (connector is_last) ] in 
    let name_lst = [ Printf.sprintf "%s%s name = %s" (child_prefix prefix is_last) (connector false) name ] in (* concat name to lst *)
    let typ_lst = (match typ with 
                | Some typ' -> [ Printf.sprintf "%s%s type = %s" (child_prefix prefix is_last) (connector false) typ' ]
                | None -> [ Printf.sprintf "%s%s type = inferred" (child_prefix prefix is_last) (connector false) ]) in
    let expr_lst = expr |> format_expr (child_prefix prefix is_last) true in
    let_lst @ name_lst @ typ_lst @ expr_lst
    
and format_item prefix is_last item = 
    match item with 
    | FnItem { name; params; return_type; body }    -> format_fn name params return_type body prefix is_last
    | StructItem { name; fields }                   -> format_struct name fields prefix is_last
    | VariantItem { name; cases }                   -> format_variant name cases prefix is_last
    | ConstItem { name; typ; expr }                 -> format_const name typ expr prefix is_last

 and format_fn name params return_type body prefix is_last =
    let fn_lst = [ Printf.sprintf "%s%s FnItem" prefix (connector is_last) ] in
    let name_lst = [ Printf.sprintf "%s%s name = %s" (child_prefix prefix is_last) (connector false) name ] in (* concat name to lst *)
    let return_lst = match return_type with 
                    | Some return_type -> [ Printf.sprintf "%s%s return = %s" (child_prefix prefix is_last) (connector false) return_type ]
                    | None -> [ Printf.sprintf "%s%s return = inferred" (child_prefix prefix is_last) (connector false) ] in

    let (params_lst, param_lst) = match params with
                            | [] -> ([], [])
                            | _ -> ([ Printf.sprintf "%s%s Params" (child_prefix prefix is_last) (connector false) ], 
                            format_fn_helper params (child_prefix (child_prefix prefix is_last) false)) in
    let body_lst = body |> format_expr (child_prefix prefix is_last) true in
    fn_lst @ name_lst @ return_lst @ params_lst @ param_lst @ body_lst

and format_fn_helper params prefix = 
    match params with 
    | [] -> []        
    | [ ( name, typ ) ] -> 
        let typ' = (match typ with 
                    | Some typ' -> typ'
                    | None -> "inferred") in
                    [ Printf.sprintf "%s%s Param { name = \"%s\", type = \"%s\" }" prefix (connector true) name typ' ]
    | ( name, typ ) :: t -> 
        let typ' = (match typ with 
                    | Some typ' -> typ'
                    | None -> "inferred") in
                    [ Printf.sprintf "%s%s Param { name = \"%s\", type = \"%s\" }" prefix (connector false) name typ' ]
                    @ format_fn_helper t prefix

and format_struct name fields prefix is_last =
    let struct_lst = [ Printf.sprintf "%s%s StructItem" prefix (connector is_last) ] in 
    let name_lst = [ Printf.sprintf "%s%s name = %s" (child_prefix prefix is_last) (connector false) name ] in
    let fields_lst = [ Printf.sprintf "%s%s Fields" (child_prefix prefix is_last) (connector true) ] in 
    let field_lst = format_struct_helper fields (child_prefix (child_prefix prefix is_last) true) in 
    struct_lst @ name_lst @ fields_lst @ field_lst

and format_struct_helper fields prefix =
    match fields with 
    | [] -> []
    | [ (name, typ) ] -> [ Printf.sprintf "%s%s Field { name = \"%s\", type = \"%s\" }" prefix (connector true) name typ ]
    | (name, typ) :: t -> [ Printf.sprintf "%s%s Field { name = \"%s\", type = \"%s\" }" prefix (connector false) name typ ] 
                            @ format_struct_helper t prefix
and format_variant name cases prefix is_last =
    let variant_lst = [ Printf.sprintf "%s%s VariantItem" prefix (connector is_last) ] in 
    let name_lst = [ Printf.sprintf "%s%s name = %s" (child_prefix prefix is_last) (connector false) name ] in
    let cases_lst = [ Printf.sprintf "%s%s Cases" (child_prefix prefix is_last) (connector true) ] in 
    let case_lst = format_variant_helper cases (child_prefix (child_prefix prefix is_last) true) in 
    variant_lst @ name_lst @ cases_lst @ case_lst

and format_variant_helper cases prefix =
    match cases with 
    | [] -> []
    | [ (name, fields) ] -> 
            (match fields with
            | [] ->  [ Printf.sprintf "%s%s Case { name = \"%s\" }" prefix (connector true) name ]
            | fields -> [ Printf.sprintf "%s%s Case { name = \"%s\" }" prefix (connector true) name ] 
            @ format_variant_case_helper fields (child_prefix prefix true))
    | (name, fields) :: t -> 
            (match fields with
            | [] ->  [ Printf.sprintf "%s%s Case { name = \"%s\" }" prefix (connector false) name ] @ format_variant_helper t prefix
            | fields -> [ Printf.sprintf "%s%s Case { name = \"%s\" }" prefix (connector false) name ]
            @ format_variant_case_helper fields (child_prefix prefix false) @ format_variant_helper t prefix)
    
and format_variant_case_helper fields prefix =
    match fields with 
    | [] -> []
    | [ (name, typ) ] -> [ Printf.sprintf "%s%s Field { name = \"%s\", type = \"%s\" }" prefix (connector true) name typ ]
    | (name, typ) :: t -> [ Printf.sprintf "%s%s Field { name = \"%s\", type = \"%s\" }" prefix (connector false) name typ ] 
                            @ format_variant_case_helper t prefix

and format_const name typ expr prefix is_last =
    let const_lst = [ Printf.sprintf "%s%s Const" prefix (connector is_last) ] in 
    let name_lst = [ Printf.sprintf "%s%s name = %s" (child_prefix prefix is_last) (connector false) name ] in (* concat name to lst *)
    let typ_lst = (match typ with 
                | Some typ' -> [ Printf.sprintf "%s%s type = %s" (child_prefix prefix is_last) (connector false) typ' ]
                | None -> [ Printf.sprintf "%s%s type = inferred" (child_prefix prefix is_last) (connector false) ]) in
    let expr_lst = expr |> format_expr (child_prefix prefix is_last) true in
    const_lst @ name_lst @ typ_lst @ expr_lst
    
and format_program items = 
    let program_lst = [ "Program" ] in
    let items_lst = format_program_helper items "" in 
    program_lst @ items_lst

and format_program_helper items prefix =
    match items with 
    | [] -> []
    | [ item ] -> item |> format_item prefix true
    | h :: t -> (h |> format_item prefix false) @ format_program_helper t prefix

let print_expr expr =
    List.iter print_endline (expr |> format_expr "" false)

let print_stmt stmt =
    List.iter print_endline (stmt |> format_stmt "" false)

let print_item item =
    List.iter print_endline (item |> format_item "" false)

let print_ast program =
    let items_lst = format_program program.items in
    List.iter print_endline items_lst