{
  open Parser
}

let _string = ['a'-'z''A'-'Z']+
let _int = ['0'-'9']+

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | "next" { NEXT }
  | "all-next" { ALL_NEXT }
  | "normalise" { NORMALISE }
  | "lo" { LO_ALL }
  | "ri" { RI_ALL }
  | "lo-all" { LO_ALL }
  | "ri-all" { RI_ALL }
  | "lo-excl" { LO_EXCL }
  | "ri-excl" { RI_EXCL }
  | "let" { LET }
  | "L" { LAMBDA }
  | '(' { OPAR }
  | ')' { CPAR }
  | '.' { DOT }
  | _int { UNVAR (int_of_string(Lexing.lexeme lexbuf)) }
  | _string { NVAR (Lexing.lexeme lexbuf) }
  | ';' { ENDCOM }
  | eof { EOF }
  | _ { failwith ("Unexpected token: " ^ Lexing.lexeme lexbuf) }