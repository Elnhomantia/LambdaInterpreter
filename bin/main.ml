open InterpreterLambdaBruijn

let console_to_string (c : Ast.console) : string =
  match c with
  | U _ -> "<unit>"
  | T t -> Ast.string_of_terme t

  
let run_console () =
  let open Sys in
  let input_channel =
    if Array.length argv > 1 then
      let path = Filename.concat (getcwd ()) argv.(1) in
      open_in path
    else
      stdin
  in
  try
    let lexbuf = Lexing.from_channel input_channel in
    let commands = Parser.main Lexer.token lexbuf in
    let output = List.map console_to_string (List.rev commands) in
    List.iter print_endline output;
    if input_channel != stdin then close_in input_channel
  with
  | End_of_file ->
      if input_channel != stdin then close_in input_channel
  | Parser.Error ->
      prerr_endline "Erreur de parsing."
  | exn ->
      prerr_endline ("Erreur inattendue : " ^ Printexc.to_string exn)

let () = run_console()