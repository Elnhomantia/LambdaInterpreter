(lang dune 3.17)
(using menhir 3.0)

(name interpreterLambdaBruijn)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Débauché Ithier")

(maintainers "Débauché Ithier")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name interpreterLambdaBruijn)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project