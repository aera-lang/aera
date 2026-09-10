open Ast
open Token

let binary_bp op =
    match op with
    | Or                                -> Some (3, 4)
    | And                               -> Some (5, 6)
    | BitOr                             -> Some (7, 8)
    | BitXor                            -> Some (9, 10)
    | BitAnd                            -> Some (11, 12)
    | Eq | Neq | Lt | Lte | Gt | Gte    -> Some (13, 14)
    | Shl | Shr                         -> Some (15, 16)
    | Add | Sub                         -> Some (17, 18)
    | Mul | Div | Mod                   -> Some (19, 20)

let to_binary_op tok_kind =
    match tok_kind with
    (* Arithmetic *)
    | Plus              -> Some Add
    | Minus             -> Some Sub
    | Star              -> Some Mul
    | Slash             -> Some Div
    | Percent           -> Some Mod
    (* Comparison *)
    | EqualEqual        -> Some Eq
    | ExclaimEqual      -> Some Neq
    | Less              -> Some Lt
    | LessEqual         -> Some Lte
    | Greater           -> Some Gt
    | GreaterEqual      -> Some Gte
    (* Logical *)
    | AmpAmp            -> Some And
    | PipePipe          -> Some Or
    (* Bitwise *)
    | Amp               -> Some BitAnd
    | Pipe              -> Some BitOr
    | Caret             -> Some BitXor
    | LessLess          -> Some Shl (* Shift left *)
    | GreaterGreater    -> Some Shr (* Shift right *)
    | _                 -> None

let assign_bp op = 
    match op with
    | EqAssign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | AndAssign | OrAssign | XorAssign | ShlAssign | ShrAssign 
    -> Some (2, 1) (* format = (left binding power, right binding power) *)

let to_assign_op tok_kind =
    match tok_kind with
    | Equal                 -> Some EqAssign
    | PlusEqual             -> Some AddAssign
    | MinusEqual            -> Some SubAssign
    | StarEqual             -> Some MulAssign
    | SlashEqual            -> Some DivAssign
    | PercentEqual          -> Some ModAssign
    | AmpEqual              -> Some AndAssign
    | PipeEqual             -> Some OrAssign
    | CaretEqual            -> Some XorAssign
    | LessLessEqual         -> Some ShlAssign
    | GreaterGreaterEqual   -> Some ShrAssign
    | _                     -> None

let prefix_bp op =
    match op with 
    | Neg | Not -> 30

let to_prefix_op tok_kind =
    match tok_kind with 
    | Minus                 -> Some Not
    | Exclaim               -> Some Neg 
    | _                     -> None