(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

module String_set : Set.S with type elt = string
module String_map : Map.S with type key = string
module String_opt_map : Map.S with type key = string option

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int


type cmp = Eq | Neq
val string_of_cmp: cmp -> string
val cmp_fct: cmp -> ('a -> 'a -> bool)

