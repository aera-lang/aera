open Token

(* Basic Operators *)

type binary_op =
(* Arithmetic *)
| Add | Sub | Mul | Div | Mod
(* Comparison *)
| Eq | Neq | Lt | Lte | Gt | Gte
(* Logical *)
| And | Or
(* Bitwise *)
| BitAnd | BitOr | BitXor | Shl | Shr

type unary_op =
| Neg | Not (* - / ! *)

type assign_op =
(* Reassignment *)
| EqAssign
(* Arithmetic *)
| AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
(* Bitwise *)
| AndAssign | OrAssign | XorAssign | ShlAssign | ShrAssign

(* Literals *)

type literal =
| LitInt of int (* resolve suffix and type later *)
| LitFloat of float (* resolve suffix and type later *)
| LitChar of char
| LitString of string
| LitBool of bool

(* Program *)

type program = {
    items: item list;
}

and item =
| FnItem of fn_item
| StructItem of struct_item
| VariantItem of variant_item

and fn_item = {
    name: string;
    params: (string * string option) list;
    return_type: string option; (* if omitted, return unit type *)
    body: expr;
}

and struct_item = {
    name: string;
    fields: (string * string) list; (* format = name : type *)
}

and variant_case = string * (string * string) list (* if the list is empty, the variant carries nothing *)
                                                   (* note -> string * string will be changed to string * typ once
                                                    type system has been implemented *)
and variant_item = {
    name: string;
    cases: variant_case list;
}

(* Expressions *)

and block = {
    stmts: stmt list;
    expr: expr; 
}

and expr =
| Literal           of literal
| Identifier        of string
| Grouping          of expr
| Call              of { callee: expr; args: expr list }
| Binary            of { lhs: expr; op: binary_op; rhs: expr }
| Assign            of { lhs: expr; op: assign_op; rhs: expr }
| Unary             of { op: unary_op; rhs: expr }
| Block             of block
| InfiniteLoop      of expr
| WhileLoop         of { cond: expr; body: expr }
| IfExpr            of { cond: expr; then_branch: expr; else_branch: expr option }
| BreakExpr         of expr option
| ReturnExpr        of expr option

(* Statements *)

and stmt = 
| LetStmt           of let_stmt
| ConstStmt         of const_stmt

and let_stmt = { 
    name: string;
    typ: string option;
    expr: expr option;
}

and const_stmt = {
    name: string;
    typ: string option;
    expr: expr;
}

(* REPL nodes *)

type repl =
| ReplItem of item
| ReplExpr of expr
| ReplStmt of stmt

