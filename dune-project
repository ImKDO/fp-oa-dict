(lang dune 3.20)

(name Lab2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name Lab2)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (dune (>= 3.20))
  ocaml
  (ocamlformat)
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (odoc :with-doc)
 )
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
