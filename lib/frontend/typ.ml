open Int64
type identifier = string

type t = 
| PrimitiveType of primitive_type
| ArrayType of t * Int64.t
| TupleType of t list
| UserType of identifier (* covers primitive types alongside user-defined types *)

and primitive_type = 
| Int8 | Int16 | Int32 | Int64 
| Uint8 | Uint16 | Uint32 | Uint64 
| Float32 | Float64
| Char | String | Bool | Unit

let is_primitive s =
    match s with 
    (* Integers *)
    | "int8" | "int16" | "int32" | "int64" -> true
    | "uint8" | "uint16" | "uint32" | "uint64" -> true
    (* Floats *)
    | "float32" | "float64" -> true
    (* Char *)
    | "char" -> true
    (* String *)
    | "string" -> true
    (* Bool *)
    | "bool" -> true 
    (* Unit *)
    | "unit" -> true
    | _ -> false

let to_primitive s = 
    match s with 
    | "int8" -> Int8
    | "int16" -> Int16
    | "int32" -> Int32
    | "int64" -> Int64
    | "uint8" -> Uint8
    | "uint16" -> Uint16
    | "uint32" -> Uint32
    | "uint64" -> Uint64
    | "float32" -> Float32
    | "float64" -> Float64
    | "char" -> Char
    | "string" -> String 
    | "bool" -> Bool
    | "unit" -> Unit
    | _ -> failwith "not a primitive type" (* when used in conjunction with is_primitive, this match case is never reached *)
