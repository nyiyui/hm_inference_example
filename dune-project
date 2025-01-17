(lang dune 3.16)

(name hm_inference_example)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hm_inference_example)
 (synopsis "Example implementation of Hindley-Milner's type system with Algorithm W")
 (depends ocaml dune menhir ocamlformat)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
