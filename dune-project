(lang dune 3.5)

(name grewlib)

(version 1.15.4)

(using menhir 2.0)

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
 (description "Grew implements a Graph Rewriting system to be used in NLP")
 (depends 
   (ocaml 
      ( >= "4.13.0" )
    )
   dune 
   menhir
   yojson
   ANSITerminal
   (containers ( >= "3.6" ))
   (amr ( >= "0.6.0" )) 
   (conll ( >= "1.17" ))
 )
 (tags
  (nlp "graph rewriting")
 )
 (conflicts libcaml-grew)
)
