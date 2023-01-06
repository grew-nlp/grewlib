(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

open Grew_types
open Grew_graph
open Grew_rule


module Corpus : sig
  type t

  val merge: t list -> t

  val graph_of_sent_id: string -> t -> G_graph.t option

  val size: t -> int
  val get_graph: int -> t -> G_graph.t
  val is_conll: t -> bool
  val get_columns_opt: t -> Conll_columns.t option

  val update_graph: string -> G_graph.t -> t -> unit

  val get_sent_id: int -> t -> string
  val get_text: int -> t -> string

  val fold_left: ('a -> string -> G_graph.t -> 'a) -> 'a -> t -> 'a
  val fold_right: (string -> G_graph.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iteri: (int -> string -> G_graph.t -> unit) -> t -> unit

  val permut_length: t -> int array

  val of_conllx_corpus: Conll_corpus.t -> t
  val from_stdin: ?ext:string -> ?log_file: string -> ?config:Conll_config.t -> unit -> t
  val from_string: ?ext:string -> ?log_file: string -> ?config:Conll_config.t -> string -> t
  val from_file: ?ext:string -> ?log_file: string -> ?config:Conll_config.t -> string -> t
  val from_dir: ?log_file: string -> ?config:Conll_config.t -> string -> t
  val from_assoc_list: (string * G_graph.t) list -> t

  val search: ?json_label:bool -> config:Conll_config.t -> 'a -> (string -> G_graph.t -> Matching.t -> 'a -> 'a) -> Request.t -> cluster_item list -> t -> 'a Clustered.t

  val bounded_search: 
    config:Conll_config.t ->
    ?ordering: string option ->  (* if value is "length", graph are considered by size, if value is "shuffle", graph order is randomiez, else a default order is used  *)
    int option ->                (* bound on the number of matching *)
    float option ->              (* Timeout in seconds *)  
    'a ->                        (* The null value to build clusters *)
    (* The update function to build clusters. Parameters ares: *)
    (*  * int    --> graph_index in the corpus *)
    (*  * string --> sent_id *)
    (*  * int    --> position of the matching in the ≠ matchings for the same graph *)
    (*  * int    --> number of matching in the current graph  *)
    (int -> string -> int -> int -> Matching.t -> 'a -> 'a) ->
    Request.t ->
    cluster_item list ->         (* The list of element used for clustering *)
    t -> 
      ('a Clustered.t * string * float)  (* (output, statut, ratio) status is "ok", "timeout" or "over" *)
end

module Corpus_desc : sig

  type t

  val build: string -> string -> t
  val build_corpus: t -> Corpus.t
  val load_corpus_opt: t -> Corpus.t option

  val get_config: t -> Conll_config.t

  val is_rtl: t -> bool
  val is_audio: t -> bool
  val get_id: t -> string
  val get_lang_opt: t -> string option
  val get_display: t -> int option
  val get_directory: t -> string
  val load_json: string -> t list

  val compile: ?force:bool -> ?grew_match: string -> t -> unit

  val clean: t -> unit
end
