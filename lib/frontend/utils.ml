let decode_escape c =
    match c with
    | 'n' -> Ok '\n'
    | 't' -> Ok '\t'
    | 'r' -> Ok '\r'
    | '\\' -> Ok '\\'
    | '\'' -> Ok '\''
    | '"' -> Ok '"'
    | '/' -> Ok '/'
    | _ -> Error ("invalid escape sequence: \\" ^ String.make 1 c)

let decode_char raw =
    let len = String.length raw in
    if len = 1 then
        Ok raw.[0]
    else if len = 2 && raw.[0] = '\\' then
        decode_escape raw.[1]
    else
        Error ("invalid character literal: " ^ raw)

let decode_string raw =
    let buf = Buffer.create (String.length raw) in
    let len = String.length raw in
    let rec loop i =
        if i >= len then Ok (Buffer.contents buf)
        else if raw.[i] = '\\' && i + 1 < len then
            match decode_escape raw.[i + 1] with
            | Ok c -> Buffer.add_char buf c; loop (i + 2)
            | Error e -> Error e
        else
            (Buffer.add_char buf raw.[i]; loop (i + 1))
    in
    loop 0


