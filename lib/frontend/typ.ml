open Int64
open Span

type identifier = string

type t = 
| PrimitiveType of primitive_type
| ArrayType of t * array_size
| TupleType of t list
| UserType of identifier (* covers primitive types alongside user-defined types *)
| ErrorType of Span.t 

and primitive_type = 
| Int8 | Int16 | Int32 | Int64 
| Uint8 | Uint16 | Uint32 | Uint64 
| Float32 | Float64
| Char | String | Bool | Unit

and array_size =
| Known of Int64.t
| Unknown

let get_primitive_typ s = 
    match s with 
    | "int8" -> Some Int8
    | "int16" -> Some Int16
    | "int32" -> Some Int32
    | "int64" -> Some Int64
    | "uint8" -> Some Uint8
    | "uint16" -> Some Uint16
    | "uint32" -> Some Uint32
    | "uint64" -> Some Uint64
    | "float32" -> Some Float32
    | "float64" -> Some Float64
    | "char" -> Some Char
    | "string" -> Some String 
    | "bool" -> Some Bool
    | "unit" -> Some Unit
    | _ -> None



