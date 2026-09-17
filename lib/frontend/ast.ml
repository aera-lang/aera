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

type unary_op = (* = prefix_op *)
| Neg | Not (* - / ! *)

type postfix_op =
| Subscript | FieldAccess

type assign_op =
(* Reassignment *)
| EqAssign
(* Arithmetic *)
| AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
(* Bitwise *)
| AndAssign | OrAssign | XorAssign | ShlAssign | ShrAssign

(* Literals *)

type literal =
| IntLiteral of Int64.t
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
| ConstItem of const_item
| ErrorItem of Span.t


and fn_item = {
    name: identifier;
    params: param list;
    return_type: typ option; (* if omitted, infer *)
    body: expr;
}

and closure_item = {
    params: param list;
    body: expr;
}

and param = {
    param_name: identifier;
    param_typ: typ option;
}

and struct_item = {
    name: identifier;
    fields: field list;
}

and field = {
    field_name: identifier;
    field_typ: typ;
    expr: expr option; 
}

and const_item = {
    name: identifier;
    typ: typ option;
    expr: expr;
}

(* Expressions *)

and expr =
| Literal           of literal
| Ident             of identifier
| Grouping          of expr
| Index             of { target: expr; index: expr; }
| FieldAccess       of { target: expr; field: identifier; }
| Call              of { callee: expr; args: argument list }
| Binary            of { lhs: expr; op: binary_op; rhs: expr }
| Assign            of { lhs: expr; op: assign_op; rhs: expr }
| Unary             of { op: unary_op; rhs: expr }
| Block             of block
| InfiniteLoop      of expr
| WhileLoop         of { cond: expr; body: expr }
| IfExpr            of { cond: expr; then_branch: expr; else_branch: expr option }
| ArrayExpr         of expr list 
| TupleExpr         of expr list
| ErrorExpr         of Span.t

and argument = 
| Positional of expr 
| Named of { name: identifier; value: expr; }

and block = {
    stmts: stmt list;
    expr: expr option; 
}

(* Statements *)

and stmt = 
| Item              of item
| LetStmt           of let_stmt
| VarStmt           of var_stmt
| ExprStmt          of expr
| ErrorStmt         of Span.t

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