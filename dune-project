(lang dune 3.19)

(name soluna)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Thomas (L0Wigh) Mathiot <mathiotthomas@gmail.com>")

(maintainers "Thomas (L0Wigh) Mathiot <mathiotthomas@gmail.com>")

(license LICENSE)

(documentation https://github.com/l0wigh/Soluna/blob/master/DOCUMENTATION.md)

(package
 (name soluna)
 (synopsis "Lisp dialect for Divine Intellect")
 (description "Little Alchemy lab to invoke Gods and Space Creatures")
 (depends ocaml))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
