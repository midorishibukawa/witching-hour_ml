(lang dune 3.8)

(name witching_hour)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name witching_hour)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune dream caqti-driver-postgresql js_of_ocaml-compiler js_of_ocaml js_of_ocaml-ppx crunch)
 (tags
  (topics "to describe" your project)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
