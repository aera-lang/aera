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

let is_binary_op tok_kind =
    match tok_kind with
    | Plus | Minus | Star | Slash | Percent | EqualEqual | ExclaimEqual | Less | LessEqual | Greater | GreaterEqual | AmpAmp | PipePipe | Amp | Pipe | Caret | LessLess | GreaterGreater 
        -> true
    | _ -> false

let to_binary_op tok_kind =
    match tok_kind with
    (* Arithmetic *)
    | Plus              -> Add
    | Minus             -> Sub
    | Star              -> Mul
    | Slash             -> Div
    | Percent           -> Mod
    (* Comparison *)
    | EqualEqual        -> Eq
    | ExclaimEqual      -> Neq
    | Less              -> Lt
    | LessEqual         -> Lte
    | Greater           -> Gt
    | GreaterEqual      -> Gte
    (* Logical *)
    | AmpAmp            -> And
    | PipePipe          -> Or
    (* Bitwise *)
    | Amp               -> BitAnd
    | Pipe              -> BitOr
    | Caret             -> BitXor
    | LessLess          -> Shl (* Shift left *)
    | GreaterGreater    -> Shr (* Shift right *)
    | _                 -> failwith "should not reach this place if used in conjunction with is_binary_op"

let assign_bp op = 
    match op with
    | EqAssign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | AndAssign | OrAssign | XorAssign | ShlAssign | ShrAssign 
    -> Some (2, 1) (* format = (left binding power, right binding power) *)

let is_assign_op tok_kind =
    match tok_kind with
    | Equal | PlusEqual | MinusEqual | StarEqual | SlashEqual | PercentEqual | AmpEqual | PipeEqual | CaretEqual | LessLessEqual | GreaterGreaterEqual
        -> true
    | _ -> false

let to_assign_op tok_kind =
    match tok_kind with
    | Equal                 -> EqAssign
    | PlusEqual             -> AddAssign
    | MinusEqual            -> SubAssign
    | StarEqual             -> MulAssign
    | SlashEqual            -> DivAssign
    | PercentEqual          -> ModAssign
    | AmpEqual              -> AndAssign
    | PipeEqual             -> OrAssign
    | CaretEqual            -> XorAssign
    | LessLessEqual         -> ShlAssign
    | GreaterGreaterEqual   -> ShrAssign
    | _                     -> failwith "should not reach this place if used in conjunction with is_assign_op"

let prefix_bp op =
    match op with 
    | Neg | Not -> 30

let to_prefix_op tok_kind =
    match tok_kind with 
    | Minus                 -> Some Not
    | Exclaim               -> Some Neg 
    | _                     -> None