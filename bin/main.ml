open Frontend
open Interpreter

let has_valid_ext path =
  Filename.check_suffix path ".aera"
    
let () =
    match Sys.argv with
    | [| _; path |] -> (* expects an ABSOLUTE PATH right now, will resolve paths later *)
        if not (has_valid_ext path) then 
            print_endline "error: expected file name with .aera extension"
        else
            Interpreter.Run.interpret path
    | [| _ |] -> 
        print_endline "Welcome to Aera 0.0.1\nType 'quit' to exit.\n";
        Interpreter.Repl.repl ()
    | _ -> print_endline "error: expected file name with .aera extension. usage: aera <file>"

