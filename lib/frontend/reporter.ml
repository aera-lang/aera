open Diagnostic
open Source 

type t = Diagnostic.t list (* the reporter contains a list of diagnostics *)

(* Helper Functions *)

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let all_whitespace msg =
  String.for_all is_whitespace msg

(* Reporter Functions *)

let add severity module_name span msg note rep = 
    if all_whitespace msg then 
        invalid_arg "diagnostic message cannot be empty" 
    else
    { 
        severity = severity;
        module_name = module_name;
        span = span;
        msg = msg;
        note = note; 
    } :: rep
   
let add_error module_name span msg note rep = 
    rep |> add Diagnostic.Error module_name span msg note

let add_warning module_name span msg note rep = 
    rep |> add Diagnostic.Warning module_name span msg note

let add_note module_name span msg note rep = 
    rep |> add Diagnostic.Note module_name span msg note

let has_errors rep = 
    rep |> List.exists (fun d -> d.severity = Diagnostic.Error)

let has_warnings rep = 
    rep |> List.exists (fun d -> d.severity = Diagnostic.Warning)

let count_errors rep =
    rep |> List.fold_left (fun count d -> if d.severity = Diagnostic.Error then count + 1 else count) 0
        
let count_warnings rep =
    rep |> List.fold_left (fun count d -> if d.severity = Diagnostic.Warning then count + 1 else count) 0

let format_header module_name severity line col msg =
    Printf.sprintf "%s:%d:%d: %s: %s" module_name line col (severity_to_string severity) msg

let print_header module_name severity line col msg =
    prerr_endline (format_header module_name severity line col msg)

let format_line line = 
    Printf.sprintf "    %s" line

let format_caret_line col =
    Printf.sprintf "    %s^" (String.make (col - 1) ' ')

let print_contents source_line col d =
    prerr_endline (format_line source_line);
    prerr_string (format_caret_line col);
    let token_length = d.span.end_ - d.span.start_ in
    if token_length > 1 then
        prerr_string (String.make (token_length - 1) '~');
    prerr_newline ()

let print_note note =
    prerr_endline (Printf.sprintf "    note: %s" note)
   
let print_diagnostic src d =
    match offset_to_pos d.span.start_ src with
    | Error e -> prerr_endline ("internal error: " ^ e)
    | Ok pos ->
        print_header d.module_name d.severity pos.line pos.col d.msg;
        begin
            match get_line pos.line src with
            | Error e -> prerr_endline ("internal error: " ^ e)
            | Ok source_line ->
                if source_line <> "" then
                    print_contents source_line pos.col d;
                (match d.note with
                | None -> ()
                | Some note -> print_note note);
                ()
        end

    

let print_errors src rep =
    List.iter (fun d ->
    match d.severity with
    | Diagnostic.Error -> d |> print_diagnostic src
    | _ -> ()
  ) rep
        
let print_warnings src rep =
    List.iter (fun d ->
    match d.severity with
    | Diagnostic.Warning -> d |> print_diagnostic src
    | _ -> ()
  ) rep
   
let print_notes src rep =
    List.iter (fun d ->
    match d.severity with
    | Diagnostic.Note -> d |> print_diagnostic src
    | _ -> ()
  ) rep

let print src rep =
    List.iter (fun d -> 
        d |> print_diagnostic src
    ) rep