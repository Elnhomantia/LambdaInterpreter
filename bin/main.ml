open InterpreterLambdaBruijn

let rec printlist (cmdl:Ast.termList)=
  match cmdl with
  | Ast.Node (h, t) -> Printf.printf "%s\n" (Ast.string_of_terme h) ; printlist t
  | Ast.Empty -> ()

let () =
  let lexbuf =
    if Array.length Sys.argv > 1 then
      let file = Filename.concat (Sys.getcwd ()) Sys.argv.(1) in
      let channel = open_in file in
      Lexing.from_channel channel
    else
      Lexing.from_channel stdin
  in
  try
    let cmdl = Parser.main Lexer.token lexbuf in
    printlist cmdl
  with
  | Parser.Error ->
      Printf.eprintf "Parser error\n"
  | exn ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn)