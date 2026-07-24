(lang dune 3.22)

(name aera)

(generate_opam_files true)

(source
 (github aera-lang/aera))

(authors "Anjola Aina <anjola.aina@gmail.com>")

(maintainers "Anjola Aina <anjola.aina@gmail.com.com>")

(license LICENSE)

(documentation https://github.com/aera-lang/aera/tree/master/docs/spec)

(package
 (name aera)
 (synopsis "An expression-oriented programming language centered around clarity and simplicity.")
 (description "Aera is an expression-oriented language with one goal: code should just feel right. No hidden state, no guessing. Clean syntax, immutable state, static typing. Everything made explicit.")
 (depends ocaml)
 (tags
  ("language" "interpreter" "compiler" "functional" "ocaml" "repl")))