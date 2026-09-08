open Token
open Typ

(* Type *)

type typ = Typ.t 

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
| IntLiteral of int (* CHANGE TO UNSIGNED INT 64 *)
| FloatLiteral of float
| CharLiteral of char
| StringLiteral of string
| BoolLiteral of bool

(* Identifiers *)

type identifier = string

(* Program *)

type program = item list (* no need to be a record *)

and item =
| FnItem of fn_item
| ClosureItem of closure_item
| StructItem of struct_item
| VariantItem of variant_item
| ConstItem of const_item

and fn_item = {
    name: identifier;
    params: (identifier * typ option) list;
    return_type: typ option; (* if omitted, return unit type *)
    body: expr;
}

and closure_item = {
    params: (identifier * typ option) list;
    return_type: typ option; (* if omitted, return unit type *)
    body: expr;
}

and struct_item = {
    name: identifier;
    fields: (identifier * typ) list;
}

and variant_case = identifier * (identifier * typ) list (* if the list is empty, the variant carries nothing *)
                                                   
and variant_item = {
    name: identifier;
    cases: variant_case list;
}

and const_item = {
    name: identifier;
    typ: typ option;
    expr: expr;
}

(* Expressions *)

and expr =
| Literal           of literal
| Identifier        of identifier
| Grouping          of expr
| Call              of { callee: expr; args: expr list }
| Binary            of { lhs: expr; op: binary_op; rhs: expr }
| Assign            of { lhs: expr; op: assign_op; rhs: expr }
| Unary             of { op: unary_op; rhs: expr }
| Block             of block
| InfiniteLoop      of expr
| WhileLoop         of { cond: expr; body: expr }
| IfExpr            of { cond: expr; then_branch: expr; else_branch: expr option }
| MatchExpr         of match_
| BreakExpr         of expr option
| ReturnExpr        of expr option
| ArrayExpr         of expr list 
| TupleExpr         of expr list
| StructExpr        of { name: identifier; fields: (identifier * expr) list }

and block = {
    stmts: stmt list;
    expr: expr; 
}

and pattern = 
| LiteralPattern       of literal 
| IdentifierPattern    of identifier
| WildcardPattern
| ArrayPattern         of pattern list
| TuplePattern         of pattern list
| StructPattern        of identifier * field_pattern list
| VariantPattern       of identifier * pattern list     

and field_pattern = {
    field_name: identifier;
    field_pattern: pattern option;
}

and match_case = {
    pattern: pattern;
    expr: expr;
}

and match_ = {
    cond: expr;
    cases: match_case list;
}

(* Statements *)

and stmt = 
| Item              of item
| LetStmt           of let_stmt
| VarStmt           of var_stmt

and let_stmt = { 
    name: identifier;
    typ: typ option;
    expr: expr;
}

and var_stmt = { 
    name: identifier;
    typ: typ option;
    expr: expr;
}

(* REPL nodes *)

type repl =
| ReplItem of item
| ReplExpr of expr
| ReplStmt of stmt

