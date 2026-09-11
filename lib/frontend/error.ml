open Position

type error = { 
    pos: position;
    msg: string;
}

type reporter = {
    errors: error list;
}

let add_error pos msg rep =
    let diag = { pos = pos; msg = msg } in
    { errors = diag :: rep.errors } (* preprend error to the list *)

let has_errors rep =
    rep.errors <> [] (* checks if reporter has errors -> errors <> [] == errors != [] *)

let format_error pos msg =
    Printf.sprintf "error at %d:%d: %s" pos.line pos.col msg

let print_errors rep =
    List.iter (fun x -> 
        print_endline (format_error x.pos x.msg) (* goes through list and prints each error to the terminal *)
        ) rep.errors
