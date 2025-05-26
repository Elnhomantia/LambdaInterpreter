%{
  open Ast
%}

%token <int> UNVAR (*Un-Named VARiable*)
%token <string> NVAR (*Named VARiable*)
%token LAMBDA LET NEXT ALL_NEXT NORMALISE
%token LO_ALL RI_ALL LO_EXCL RI_EXCL
%token OPAR CPAR DOT
%token ENDCOM EOF (*ENDCOM = '\n' or ';'*)

%start <termList> main
%%

main:
| stat ENDCOM? EOF? { Node($1, Empty) }
| stat ENDCOM main { Node($1, $3) }

stat:
| command { $1 }
| naming { $1 }

(*We can write normalise next ri L.( ... ); for example.*)
command:
| NEXT reduction_strategy command { betaReduction $2 $3 }
| ALL_NEXT reduction_strategy command { betaReductionAll $2 $3 }
| NORMALISE reduction_strategy command { normalise $2 $3 }
| terme { $1 }

naming:
| LET NVAR terme { failwith "let not yet implemented" }

reduction_strategy:
| LO_ALL { LoAll }
| RI_ALL { RiAll }
| LO_EXCL { LoExcl }
| RI_EXCL { RiExcl }

terme:
| terme terme { App($1, $2) }
| LAMBDA DOT OPAR terme CPAR { Lambda($4) } (*L.( ... )*)
| LAMBDA DOT var { Lambda($3) } (*So we can write L.1*)
| var { $1 }
| OPAR terme CPAR { $2 } (*Optional parenthesis*)

var:
| UNVAR { Var($1) } (*un-named variable, eg. 1 in L.(1)*)
| NVAR { failwith "let not yet implemented" } (*named variable, eg. x in let x L.(1 2)*)