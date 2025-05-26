open InterpreterLambdaBruijn

let console_to_string (cmd:Ast.console)=
match cmd with
|T t -> Ast.string_of_terme t
|U _ -> ""

  
let rec prompt() =
  let lexbuf =
    if Array.length Sys.argv > 1 then
      let file = Filename.concat (Sys.getcwd ()) Sys.argv.(1) in
      let channel = open_in file in
      Lexing.from_channel channel
    else
      Lexing.from_channel stdin
  in
  try
    let cmd = Parser.main Lexer.token lexbuf in
    Printf.printf "%s\n" (console_to_string cmd); prompt()
  with
  | Parser.Error ->
      Printf.eprintf "Parser error\n"
  | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn)

let () = prompt()