(lang dune 3.20)

(name raoui)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name raoui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  eio
  eio_main
  logs
  terminal_size
  uutf
  uuseg
  uucp
  ptime
  sqlite3
  sedlex
  (alcotest :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
