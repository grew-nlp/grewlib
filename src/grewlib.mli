(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

(* ================================================================================ *)
include module type of struct include Grew_types end

(* ==================================================================================================== *)
(** {2 General definitions} *)
(* ==================================================================================================== *)
module Grewlib : sig
  val set_debug_mode: bool -> unit
  val set_safe_commands: bool -> unit
  val set_track_rules: bool -> unit
  val set_track_history: bool -> unit
  val set_track_impact: bool -> unit

  exception Error of string
  exception Bug of string
end

(* ==================================================================================================== *)
(** {2 Deco} *)
(* ==================================================================================================== *)
module Deco: sig
  type t
end


(* ==================================================================================================== *)
(** {2 Graph} *)
(* ==================================================================================================== *)
module Graph : sig

  type t

  (** number of nodes *)
  val size: t -> int

  (** get a graph from a file either in 'gr' or 'conll' format.
      File extension should be '.gr' or '.conll'.
      @raise Parsing_err if grewlib can't parse the file
      @raise File_not_found if the file doesn't exists. *)
  val load: config:Conll_config.t -> string -> t

  val of_json: Yojson.Basic.t -> t
  val to_json: t -> Yojson.Basic.t

  val of_pst: string -> t

  val sentence_of_pst: string -> string

  val to_sentence: ?pivot: string -> ?deco:Deco.t -> t -> string

  (** [to_sentence_audio ~deco graph] computes the needed information for Grew-match with audio.
      The output contains:
      - the HTML string with sound offset
      - the first/last offsets of the sentence
      - the sound_url metadata
  *)
  val to_sentence_audio: ?deco:Deco.t -> t -> string * (float * float) option * string option

  val to_dot : ?main_feat:string -> config:Conll_config.t -> ?deco:Deco.t -> t -> string

  val to_dep : ?filter: (string -> bool) -> ?no_root:bool -> ?pid:bool -> ?main_feat:string -> ?deco:Deco.t -> config:Conll_config.t -> t -> string

  val get_meta_opt: string -> t -> string option

  val get_meta_list: t -> (string * string) list

  val set_meta: string -> string -> t -> t

  val append_in_ag_lex: string list -> t -> int Clustered.t -> int Clustered.t
  (** [append_in_ag_lex feature_name_list graph ag_lex] appends each node
      of the input [graph] into the [ag_lex] built following features in [feature_name_list].
      The type [int Clustered.t] is used in ArboratogGrew for the storage of the lexicon extracted from a corpus.
  *)

  val get_feature_values: string -> t -> String_set.t
  val get_relations: config:Conll_config.t -> t -> String_set.t
  val get_features: t -> String_set.t

  val get_history: t -> (Deco.t * (string * int) * Deco.t * t) list
  val trace_depth: t -> int
end

(* ==================================================================================================== *)
(** {2 Requests} *)
(* ==================================================================================================== *)
module Request : sig
  type t

  type basic

  (** [load filename] returns the request described in the file *)
  val load: config:Conll_config.t -> string -> t

  (** [parse description] returns the request described in the [descriprion] string *)
  val parse: config:Conll_config.t -> string -> t

  (** [json_bound_names request] returns a JSON object with two items: "nodes" and "edges", 
      each associated to a list of string for bound nodes and edges *)
  val json_bound_names: t -> Yojson.Basic.t

  val of_json: config:Conll_config.t -> Yojson.Basic.t -> t

  type cluster_item

  (** [parse_cluster_item s] returns a whether if the input string is  *)
  val parse_cluster_item: config:Conll_config.t -> t -> string -> cluster_item

end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching: sig
  type t

  val to_json: ?all_edges:bool -> Request.t -> Graph.t -> t -> Yojson.Basic.t

  val nodes: Request.t -> Graph.t -> t -> (string * string) list

  val subgraph: Graph.t -> t -> int -> Graph.t

  (** [search_request_in_graph request graph] returns the list of the possible matching of [request] in [graph] *)
  val search_request_in_graph: config:Conll_config.t -> Request.t -> Graph.t -> t list

  (** [build_deco request matching] returns the deco to be used in the graphical representation.
      WARNING: the function supposes that [matching] was found with the given [request]! *)
  val build_deco: Request.t -> t -> Deco.t

  val get_clust_value_opt: ?json_label:bool -> config:Conll_config.t -> Request.cluster_item ->  Request.t -> Graph.t -> t -> string option
end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs : sig
  type t

  val empty: t

  val load: config:Conll_config.t -> string -> t

  val parse: config:Conll_config.t -> string -> t

  val to_json: config:Conll_config.t -> t -> Yojson.Basic.t

  val get_strat_list: t -> string list

  (** [get_strat_lists grs] returns two lists:
      * the full list of strategies defined
      * the sublist of "top" strategies: the ones not used as a substrat elsewhere *)
  val get_strat_lists: t -> (string list * string list)

  val get_package_list: t -> string list

  val get_rule_list: t -> string list

  val get_timestamp_opt: t -> float option

  val of_json: config:Conll_config.t -> Yojson.Basic.t -> t
end

(* ==================================================================================================== *)
(** {2 Rewrite} *)
(* ==================================================================================================== *)
module Rewrite: sig
  val set_max_rules: int -> unit

  val set_timeout: float option -> unit

  (** [simple_rewrite config gr grs strat] builds the set of graphs given by
      the rewriting of graph [gr] with the strategy [strat] of [grs].
      @param gr the graph to rewrite
      @param grs the graph rewriting system
      @param strat the name of the strategy to apply
  *)
  val simple_rewrite: config:Conll_config.t -> Graph.t -> Grs.t -> string -> Graph.t list


  val onf_rewrite_opt: config:Conll_config.t -> Graph.t -> Grs.t -> string -> Graph.t option

  (** [log_rewrite ())] outputs a JSON describing the number of rules applies and the time for the last rewrite call *)
  val log_rewrite: unit -> Yojson.Basic.t
end



(* ==================================================================================================== *)
(** {2 Corpus} *)
(* ==================================================================================================== *)
module Corpus: sig
  type t

  (* [size t] returns the number of graphs in the corpus *)
  val size: t -> int

  val graph_of_sent_id: string -> t -> Graph.t option

  val get_graph: int -> t -> Graph.t
  val get_sent_id: int -> t -> string
  val is_conll: t -> bool
  val get_text: int -> t -> string

  val update_graph: string -> Graph.t -> t -> unit

  val fold_left: ('a -> string -> Graph.t -> 'a) -> 'a -> t -> 'a
  val fold_right: (string -> Graph.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iteri: (int -> string -> Graph.t -> unit) -> t -> unit

  val permut_length: t -> int array

  val of_conllx_corpus: Conll_corpus.t -> t
  val from_stdin: ?ext:string -> ?log_file:string -> ?config:Conll_config.t -> unit -> t
  val from_string: ?ext:string -> ?log_file:string -> ?config:Conll_config.t -> string -> t
  val from_file: ?ext:string -> ?log_file:string -> ?config:Conll_config.t -> string -> t
  val from_dir: ?config:Conll_config.t -> string -> t

  val from_assoc_list: (string * Graph.t) list -> t

  val merge: t list -> t
  val get_columns_opt: t -> Conll_columns.t option

  val search:
    ?json_label: bool ->
    config:Conll_config.t ->
    'a ->                   (* null value to build clusters *)
    (string -> Graph.t -> Matching.t -> 'a -> 'a) ->
    (* update function to build clusters. Parameters ares:
        * string  --> sent_id
        * Graph.t --> the graph in which the pattern were found
        * int     --> position of the matching in the ≠ matchings for the same graph
        * int     --> number of matching in the current graph  *)
    Request.t ->
    Request.cluster_item list ->    (* list of element used for clustering *)
    t ->
      'a Clustered.t
  (** [search config null update request cluster_item_list corpus] returns a clustered structure
      representing the multilayer clustering following [cluster_item_list];
      the computing of each cluster contents (of type ['a]) is controlled by the value [null] and the function [update].
      Examples of usage:
      * null=0, update=(fun _ _ _ x -> x+1)    for counting the number of occurrences
      * null=[], update=(fun _ _ m x -> m::x)  for recording the matchings
   *)

  val bounded_search: 
    ?json_label: bool ->
    config:Conll_config.t ->
    ?ordering: string option ->  (* if value is "length", graph are considered by size, if value is "shuffle", graph order is randomiez, else a default order is used  *)
    int option ->                (* bound on the number of matching *)
    float option ->              (* Timeout in seconds *)  
    'a ->                        (* null value to build clusters *)
    (int -> string -> Graph.t -> int -> int -> Matching.t -> 'a -> 'a) ->
    (* update function to build clusters. Parameters are:
       * int    --> graph_index in the corpus
       * string --> sent_id
       * G_graph.t --> the graph
       * int    --> position of the matching in the ≠ matchings for the same graph
       * int    --> number of matching in the current graph  *)
    Request.t ->
    Request.cluster_item list ->         (* The list of element used for clustering *)
    t -> 
      ('a Clustered.t * string * float)  (* (output, statut, ratio) status is "ok", "timeout" or "over" *)
  (** search for a request in a corpus , with timeout and a bounded number of solutions *)

  val count_feature_values: 
    ?filter: (string -> bool) ->
    t -> int String_map.t String_map.t

end

(* ================================================================================ *)
module Corpus_desc : sig

  (** This module encodes the notion of corpus description which is mainly used in Grew-match *)
  type t

  val to_json: t -> Yojson.Basic.t
  val of_json: Yojson.Basic.t -> t

  (** [load_json filename] returns the list of corpus_desc described in the json file [filename] *)
  val load_json: ?env:(string * string) list -> string -> t list

  (** [get_config t] returns config defined in the corpus description *)
  val get_config: t -> Conll_config.t

  val get_id: t -> string

  val get_directory: t -> string

  val get_field_opt: string -> t -> string option

  val get_display: t -> int option

  val get_flag: string -> t -> bool

  (** [build_corpus t] returns the corpus described *)
  val build_corpus: t -> Corpus.t

  (** [load_corpus_opt t] returns the corpus if it is compiled *)
  val load_corpus_opt: t -> Corpus.t option

  (** [get_files t] returns the list of full path of files considered in the corpus *)
  val get_files: t -> string list

  (** [compile t] compiles the corpus
       - if the compiled file does not exist or is older than one if the corpora files
       - or if [force] is `true` (default is `false`) *)
  val compile: ?force:bool -> t -> unit

  (** [clean t] remove the compiled file, if any *)
  val clean: t -> unit

  (** [show t] print the corpus config *)
  val show: t -> unit

  val validate: ?verbose:bool -> ?env: (string * string) list -> t -> unit
end (* module Corpus_desc *)

(* ================================================================================ *)
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

  val build: ?filter: (string -> bool) -> t -> unit

  val compile: ?filter: (string -> bool) -> t -> unit

end

(* ================================================================================ *)
module Sbn: sig
  val to_json: string -> Yojson.Basic.t
end