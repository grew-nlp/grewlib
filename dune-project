(lang dune 3.5)

(name grewlib)

(version 1.12.0)

(generate_opam_files true)

(source
 (github grew-nlp/grewlib))

(authors "Bruno Guillaume")

(maintainers "Bruno.Guillaume@inria.fr")

(license CECILL-2.1)

(documentation https://grew.fr)

(package
 (name grewlib)
 (synopsis "The main library for the Grew system")
 (description "Grew implements a Grapg Rewriting system to be used in NLP")
 (depends ocaml dune amr conll)
 (tags
  (nlp "graph rewriting")
 )
)
