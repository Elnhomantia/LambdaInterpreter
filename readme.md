# De Bruijn lambda interpret

This is a lambda interpret in De Bruijn indexs.

You can test it by writting a test. See in /test for examples.

## Features :
- Interactive prompt with : `dune exec interpreterLambdaBruijn`
- Read from a file with : `dune exec interpreterLambdaBruijn <file_path_from_pwd>`

## REQUIREMENTS :
- Dune
- ocamllex
- menhir

All can be installed from opam

## How to build :
`dune build`

## How to test :
`dune test`
all test are in /test.
Also add your tests to the dune file instead for now.

## Syntax :
See lexer and parser file.

## Known bugs :
Interactive prompt don't work for now.

### Author :
Ithier Débauché

### Context :
This is a small project for a class in first year of master degree.
See : https://perso.liris.cnrs.fr/xavier.urbain/ens/PFA/projpfa.html
from https://perso.liris.cnrs.fr/xavier.urbain/
26/05/2025

### License :
IDK
