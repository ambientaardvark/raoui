(lang dune 3.20)

(name raoui)

(generate_opam_files true)

(source
 (github ambientaardvark/raoui))

(authors "Alan Lee")

(maintainers "Alan Lee")

(license LICENSE)

(documentation https://github.com/ambientaardvark/raoui)

(package
 (name raoui)
 (synopsis "A modern terminal console for R built in OCaml")
 (description "Raoui is an interactive terminal R console with syntax highlighting, completion, inline plotting, and an asynchronous UI/interpreter architecture.")
 (depends
  ocaml
  eio
  eio_main
  logs
  sqlite3
  terminal_size
  uutf
  uuseg
  uucp
  ptime
  sedlex
  cmarkit
  cohttp-eio
  yojson
  otoml
  (alcotest :with-test))
 (tags
  ("r" "repl" "terminal" "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
