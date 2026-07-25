open Span

type severity = 
| Error 
| Warning
| Note

type diagnostic = { 
    severity: severity;
    span: span;
    msg: string;
    note: string option;
    module_name: string;
}