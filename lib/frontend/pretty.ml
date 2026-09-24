open Ast 
open Parser
open Typ

(* -------------------- Helper Functions -------------------- *)

let escape_char c =
    match c with
    | '\n'   -> "\\n"
    | '\t'   -> "\\t"
    | '\r'   -> "\\r"
    | '\\'  -> "\\\\"
    | '\''  -> "\\'"
    | c     -> String.make 1 c

let escape_string s =
    let buf = Buffer.create (String.length s) in 
    String.iter (fun c ->
        match c with 
        | '\n'  -> Buffer.add_string buf "\\n"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\"' -> Buffer.add_string buf "\\\""
        | c     -> Buffer.add_char buf c
    ) s;
    Buffer.contents buf

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
    
let format_primitive_typ typ = 
    match typ with 
    | Int8  -> "int8"
    | Int16 -> "int16"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Uint8 -> "uint8"
    | Uint16 -> "uint16"
    | Uint32 -> "uint32"
    | Uint64 -> "uint64"
    | Float32 -> "float32"
    | Float64 -> "float64"
    | Char -> "char"
    | String -> "string"
    | Bool -> "bool"
    | Unit -> "unit"

(* -------------------- Walk Expr -------------------- *)

let rec walk_expr expr =
    match expr with 
    | Literal (IntLiteral n)                            -> [(Int64.to_string n)]
    | Literal (FloatLiteral f)                          -> [(string_of_float f)]
    | Literal (CharLiteral c)                           -> ["'" ^ escape_char c ^ "'"]
    | Literal (StringLiteral s)                         -> ["\"" ^  escape_string s ^ "\""]
    | Literal (BoolLiteral b)                           -> [(Bool.to_string b)]
    | Ident s                                           -> [s]
    | Grouping expr'                                    -> walk_grouping expr'
    | Index { target; index }                           -> walk_index target index
    | FieldAccess { target; field }                     -> walk_field_access target field
    | Call { callee; args }                             -> walk_call callee args
    | Binary { lhs; op; rhs }                           -> walk_binary lhs op rhs
    | Assign { lhs; op; rhs }                           -> walk_assign lhs op rhs
    | Unary { op; rhs }                                 -> walk_unary op rhs
    | Block { stmts; expr; }                            -> walk_block stmts expr
    | InfiniteLoop expr'                                -> walk_infinite_loop expr'
    | WhileLoop { cond; body; }                         -> walk_while_loop cond body
    | IfExpr { cond; then_branch; else_branch; }        -> walk_if cond then_branch else_branch
    | ArrayExpr expr_list                               -> walk_array_expr expr_list 
    | TupleExpr expr_list                               -> walk_tuple_expr expr_list 
    | ErrorExpr span                                    -> walk_error_expr span

and walk_grouping expr = ["("] @ walk_expr expr @ [")"]

and walk_index target index = 
    (walk_expr target) @  ["["] @ (walk_expr index) @ ["]"]

and walk_field_access target field = 
    (walk_expr target) @ ["."] @ [field]

and walk_call callee args = 
    (walk_expr callee) @ ["("] @ (walk_args args) @ [")"]

and walk_args args = 
    match args with 
    | []            -> []
    | [arg]         -> walk_arg arg
    | arg :: rest   -> (walk_arg arg) @ [","] @ walk_args rest

and walk_arg arg = 
    match arg with 
    | Positional expr       -> walk_expr expr 
    | Named { name; value } -> [name; ":"] @ (walk_expr value)

and walk_binary lhs op rhs = 
    (walk_expr lhs) @ [ format_binary_op op ] @ (walk_expr rhs)

and walk_assign lhs op rhs = 
    (walk_expr lhs) @ [ format_assign_op op ] @ (walk_expr rhs)

and walk_unary op rhs = 
    [ format_unary_op op ] @ (walk_expr rhs)

and walk_block stmts tail_expr = 
    let stmt_tokens = List.concat_map walk_stmt stmts in
    let tail_token = match tail_expr with 
    | Some e    -> walk_expr e 
    | None      -> []
    in
    ["{"] @ stmt_tokens @ tail_token @ ["}"]

and walk_infinite_loop body = 
    ["loop"] @ (walk_expr body)

and walk_while_loop cond body =
    ["while"] @ (walk_expr cond) @ (walk_expr body)

and walk_if cond then_branch else_branch =
    let base = ["if"] @ (walk_expr cond) @ (walk_expr then_branch) in 
    match else_branch with 
    | Some e        -> base @ ["else"] @ (walk_expr e)
    | None          -> base

and walk_array_expr exprs = 
    ["["] @ (walk_expr_list exprs) @ ["]"]

and walk_tuple_expr exprs = 
    match exprs with 
    | [e]   -> ["("] @ (walk_expr e) @ [",)"]
    | _     -> ["("] @ (walk_expr_list exprs) @ [")"]

and walk_expr_list exprs =
    match exprs with 
    | []            -> []
    | [e]           -> walk_expr e 
    | e :: rest   -> (walk_expr e) @ [","] @ walk_expr_list rest

and walk_error_expr span = ["<error_expr>"]

(* -------------------- Types -------------------- *)

and format_typ typ = 
    match typ with 
    | PrimitiveType p           -> format_primitive_typ p
    | UserType name             -> name 
    | ArrayType(t, size)        ->
        let size_str = match size with 
        | Known n       -> Int64.to_string n 
        | Unknown       -> "?"
        in
        "[" ^ format_typ t ^ ", " ^ size_str ^ "]"
    | TupleType types           -> "(" ^ String.concat ", " (List.map format_typ types) ^ ")"
    | ErrorType _               -> "<error_type>"

(* -------------------- Walk Stmt  -------------------- *)

and walk_stmt stmt =
    match stmt with 
    | Item item                     -> walk_item item 
    | LetStmt { name; typ; expr }   -> walk_binding "let" name typ expr
    | VarStmt { name; typ; expr }   -> walk_binding "var" name typ expr
    | ExprStmt e                    -> walk_expr e
    | ErrorStmt _                   -> ["<error_stmt>"]

and walk_binding keyword name typ expr = 
    let typ_token = match typ with 
    | Some t        -> [":"; format_typ t]
    | None          -> []
    in
    [keyword; name] @ typ_token @ ["="] @ walk_expr expr

(* -------------------- Walk Item -------------------- *)

and walk_item item =
    match item with 
    | FnItem { name; params; return_type; body }    -> walk_fn name params return_type body 
    | ClosureItem { params; body }                  -> walk_closure params body
    | StructItem { name; fields }                   -> walk_struct name fields
    | ConstItem { name; typ; expr }                 -> walk_const name typ expr
    | ErrorItem _                                   -> ["<error_item>"]

and walk_param param =
    match param.param_typ with 
    | Some t    -> [param.param_name; ":"; format_typ t]
    | None      -> [param.param_name]

and walk_params params = 
    match params with 
    | [] -> [] 
    | [p] -> walk_param p
    | p :: rest -> (walk_param p) @ [","] @ walk_params rest

and walk_fn name params return_type body = 
    let ret_token = match return_type with 
    | Some t    -> ["->"; format_typ t] 
    | None      -> []
    in
    ["fn"; name; "("] @ (walk_params params) @ [")"] @ ret_token @ (walk_expr body)


and walk_closure params body =
    ["fn"; "("] @ (walk_params params) @ [")"] @ ["=>"] @ (walk_expr body)

and walk_struct name fields =
    ["struct"; name; "{"] @ (List.concat_map walk_field fields) @ ["}"]

and walk_field field = 
    let default_token = match field.expr with 
    | Some e            -> ["="] @ (walk_expr e)
    | None              ->  []
    in
    [field.field_name; ":"; format_typ field.field_typ] @ default_token

and walk_const name typ expr =
    let typ_token = match typ with 
    | Some t            -> [":"; format_typ t]
    | None              -> []
    in
    ["const"; name] @ typ_token @ ["="] @ (walk_expr expr)

(* -------------------- Full Walk -------------------- *)

let walk_program items = List.map walk_item items

let to_source items =
    items 
    |> walk_program
    |> List.map (String.concat " ")
    |> String.concat "\n"