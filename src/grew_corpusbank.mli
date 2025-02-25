(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_corpus

module Corpusbank : sig
  type t

  val iter:
    ?filter: (string -> bool) ->
    (string -> Corpus_desc.t -> unit) ->
      t -> unit

  val fold:
    ?filter: (string -> bool) ->
    (string -> Corpus_desc.t -> 'a -> 'a) ->
      t -> 'a -> 'a


  val read_files: string list -> t 

  val load: string -> t

  val build_filter : string list -> (string -> bool)

  val get_corpus_desc_opt : t -> string -> Corpus_desc.t option

  val print_status : ?verbose:bool -> ?filter:(string -> bool) -> t  -> unit

  val build_derived: t -> Corpus_desc.t -> unit

  val build: ?filter: (string -> bool) -> t -> unit

  val compile: ?filter: (string -> bool) -> t -> unit

end

