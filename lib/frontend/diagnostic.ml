open Span

type severity = 
| Error 
| Warning
| Note

type t = { 
    severity: severity;
    span: Span.t;
    msg: string;
    note: string option;
    module_name: string;
}

let severity_to_string severity = 
match severity with 
| Error -> "error"
| Warning -> "warning"
| Note -> "note"