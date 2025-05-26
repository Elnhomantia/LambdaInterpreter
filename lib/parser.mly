%{
  open Ast
  open Hcons.Hcons
%}

%token <int> UNVAR (*Un-Named VARiable*)
%token <string> NVAR (*Named VARiable*)
%token LAMBDA LET NEXT ALL_NEXT NORMALISE
%token LO_ALL RI_ALL LO_EXCL RI_EXCL
%token OPAR CPAR DOT
%token ENDCOM EOF (*ENDCOM = '\n' or ';'*)

%start <console> main
%%

main:
| stat ENDCOM EOF? { $1 }

stat:
| command { T($1) }
| naming { U($1) }

(*We can write normalise next ri L.( ... ); for example.*)
command:
| NEXT reduction_strategy command { betaReduction $2 $3 }
| ALL_NEXT reduction_strategy command { betaReductionAll $2 $3 }
| NORMALISE reduction_strategy command { normalise $2 $3 }
| terme { $1 }

naming:
| LET NVAR terme { addNvar $2 $3 }

reduction_strategy:
| LO_ALL { LoAll }
| RI_ALL { RiAll }
| LO_EXCL { LoExcl }
| RI_EXCL { RiExcl }

terme:
| terme terme { sApp $1 $2 }
| LAMBDA DOT OPAR terme CPAR { sLambda $4 } (*L.( ... )*)
| LAMBDA DOT var { sLambda $3 } (*So we can write L.1*)
| var { $1 }
| OPAR terme CPAR { $2 } (*Optional parenthesis*)

var:
| UNVAR { sVar $1 } (*un-named variable, eg. 1 in L.(1)*)
| NVAR { getNvar $1 } (*named variable, eg. x in let x L.(1 2)*)