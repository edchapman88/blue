(lang dune 3.16)

(name blue)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Ed Chapman")

(maintainers "Ed Chapman")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name blue)
 (synopsis "POMDP interface")
 (description "Interface between partially observable, concurrent systems and POMDPs.")
 (depends ocaml dune odoc core)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
