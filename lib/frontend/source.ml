open Span
open Position

type source = {
    contents: string;
    filename: string;
    line_spans: span array;
}

let rec line_spans src start offsets acc =
    match offsets with
    | [] -> ({ start_ = start; end_ = String.length src.contents } :: acc)
                |> List.rev
                |> Array.of_list
    | offset :: rest -> { start_ = start; end_ = offset } :: acc |> line_spans src (offset + 1) rest

let line_from_offset offset src =
    let rec loop left right =
        if left >= right then
            if left = 0 then
                Error "offset is before the start of the source"
            else
                let span = src.line_spans.(left - 1) in
                if offset <= span.end_ then
                    Ok left (* return the index instead of the span -> can find span by doing spans.(index - 1) since it's 1-indexed*)
                else
                    Error "offset is beyond the end of the source"
        else
            let mid = left + (right - left) / 2 in
            if src.line_spans.(mid).start_ <= offset then
                loop (mid + 1) right
            else
                loop left mid
    in
    loop 0 (Array.length src.line_spans)


let offset_to_pos offset src =
    match src |> line_from_offset offset with 
    | Error e -> Error e
    | Ok index -> 
        Ok ({ line = index; col = offset - src.line_spans.(index - 1).start_ + 1 }) (* column is 1-indexed *)

let span_to_pos span src = (* from line X col Y to line X' col Y *)
    match src |> offset_to_pos span.start_ with 
    | Error e -> Error e 
    | Ok start_pos ->
        begin 
            match src |> offset_to_pos span.end_ with 
            | Error e -> Error e 
            | Ok end_pos -> Ok (start_pos, end_pos)
        end

let extract span src =
    String.sub src.contents span.start_ (span.end_ - span.start_)

let get_line line src = 
    if line < 1 || line > Array.length src.line_spans then 
        Error ("line index out of range")
    else 
        Ok (extract src.line_spans.(line - 1) src)
    
let line_containing offset src = 
    match src |> line_from_offset offset with 
    | Error e -> Error e 
    | Ok index -> Ok (src.line_spans.(index - 1))