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



(* ================================================================================ *)
module Clustered : sig
  type 'a t

  val empty: 'a t

  val size: 'a t -> int

  val update: ('a -> 'a) -> string option list -> 'a -> 'a t -> 'a t

  val prune_unambiguous: int -> 'a t -> 'a t

  val fold: ('b -> string option list -> 'a -> 'b) -> 'a t -> 'b -> 'b

  val insert: string option list -> int t -> int t

  val cardinal: int t -> int

  val to_json: string list -> int t -> Yojson.Basic.t

end (* module Clustered *)
