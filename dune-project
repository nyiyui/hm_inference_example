(lang dune 3.16)

(name hm_inference_example)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Ken Shibata")

(maintainers "Ken Shibata")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hm_inference_example)
 (synopsis "Example implementation of Hindley-Milner's type system with Algorithm W")
 (depends ocaml dune menhir ocamlformat))
