(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conllx

open Grew_types

(* ==================================================================================================== *)
(** {2 General definitions} *)
(* ==================================================================================================== *)
module Libgrew : sig
  val get_version: unit -> string
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
      @raise Parsing_err if libgrew can't parse the file
      @raise File_not_found if the file doesn't exists. *)
  val load: config:Conllx_config.t -> string -> t

  val of_json: Yojson.Basic.t -> t
  val to_json: t -> Yojson.Basic.t

  val of_pst: string -> t

  val sentence_of_pst: string -> string

  val to_sentence: ?pivot: string -> ?deco:Deco.t -> t -> string

  val to_sentence_audio: ?deco:Deco.t -> t -> string * (float * float) option

  val to_dot : ?main_feat:string -> config:Conllx_config.t -> ?deco:Deco.t -> t -> string

  val to_dep : ?filter: (string -> bool) -> ?no_root:bool -> ?main_feat:string -> ?deco:Deco.t -> config:Conllx_config.t -> t -> string

  val get_meta_opt: string -> t -> string option

  val get_meta_list: t -> (string * string) list

  val set_meta: string -> string -> t -> t

  val append_in_ag_lex: string list -> t -> int Clustered.t -> int Clustered.t
  (** [append_in_ag_lex feature_name_list graph ag_lex] appends each node
      of the input [graph] into the [ag_lex] built following features in [feature_name_list].
      The type [int Clustered.t] is used in ArboratogGrew for the storage of the lexicon extracted from a corpus.
  *)

  val get_feature_values: string -> t -> String_set.t
  val get_relations: config:Conllx_config.t -> t -> String_set.t
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

  (** [load filename] returns the pattern described in the file *)
  val load: config:Conllx_config.t -> string -> t

  (** [parse description] returns the pattern described in the [descriprion] string *)
  val parse: config:Conllx_config.t -> string -> t

  (** [parse_basic description] returns the basic described in the [descriprion] string *)
  val parse_basic: config:Conllx_config.t -> t -> string -> basic

  val pid_name_list: t -> string list
end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching: sig
  type t

  val to_json: ?all_edges:bool -> Request.t -> Graph.t -> t -> Yojson.Basic.t

  val nodes: Request.t -> Graph.t -> t -> (string * string) list

  (* [get_value_opt cluster_key pattern graph matching] returns the value corresponding to the cluster_key in the result of a previous result of match
      [cluster_key] can be:
      * the name of a feature value [N.feat] where [N] is a node declared in the kernel part of the pattern
      * the name of an edge featue [e.feat] where [e] is a edge declared in the kernel part of the pattern
  *)
  (* TODO: do not export: generalized by get_clust_value_opt *)
  val get_value_opt: config:Conllx_config.t -> string -> Request.t -> Graph.t -> t -> string option

  (* TODO: do not export: generalized by get_clust_value_opt *)
  val whether: config:Conllx_config.t -> Request.basic -> Request.t -> Graph.t -> t -> bool
  
  val subgraph: Graph.t -> t -> int -> Graph.t

  (** [search_pattern_in_graph pattern graph] returns the list of the possible matching of [pattern] in [graph] *)
  val search_pattern_in_graph: config:Conllx_config.t -> Request.t -> Graph.t -> t list

  (** [build_deco pattern matching] returns the deco to be used in the graphical representation.
      WARNING: the function supposes that [matching] was find with the given [pattern]! *)
  val build_deco: Request.t -> t -> Deco.t

  val get_clust_value_opt: config:Conllx_config.t -> cluster_item ->  Request.t -> Graph.t -> t -> string option
end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs : sig
  type t

  val empty: t

  val load: config:Conllx_config.t -> string -> t

  val parse: config:Conllx_config.t -> string -> t

  val to_json: config:Conllx_config.t -> t -> Yojson.Basic.t

  val dump: t -> unit

  val get_strat_list: t -> string list

  (** [get_strat_lists grs] returns two lists:
      * the full list of strategies defined
      * the sublist of "top" strategies: the ones not used as a substrat elsewhere *)
  val get_strat_lists: t -> (string list * string list)

  val get_package_list: t -> string list

  val get_rule_list: t -> string list

  val of_json: config:Conllx_config.t -> Yojson.Basic.t -> t

  val request_of_json: config:Conllx_config.t -> Yojson.Basic.t -> Request.t

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
  val simple_rewrite: config:Conllx_config.t -> Graph.t -> Grs.t -> string -> Graph.t list


  val onf_rewrite_opt: config:Conllx_config.t -> Graph.t -> Grs.t -> string -> Graph.t option

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

  val fold_left: ('a -> string -> Graph.t -> 'a) -> 'a -> t -> 'a
  val fold_right: (string -> Graph.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iteri: (int -> string -> Graph.t -> unit) -> t -> unit

  val permut_length: t -> int array

  val from_stdin: ?ext:string -> ?log_file:string -> ?config:Conllx_config.t -> unit -> t
  val from_string: ?ext:string -> ?log_file:string -> ?config:Conllx_config.t -> string -> t
  val from_file: ?ext:string -> ?log_file:string -> ?config:Conllx_config.t -> string -> t
  val from_dir: ?config:Conllx_config.t -> string -> t

  val merge: t list -> t
  val get_columns_opt: t -> Conllx_columns.t option

  val search: config:Conllx_config.t -> 'a -> (Matching.t -> 'a -> 'a) -> Request.t -> cluster_item list -> t -> 'a Clustered.t
  (** [search config null update pattern cluster_item_list corpus] returns a clustered structure
      representing the multilayer clustering following [cluster_item_list];
      The computing of each cluster contents (of type ['a]) is controlled by the value [null] and the function [update].
      Examples of usage:
      * null=0, update=(fun _ x -> x+1)    for counting the number of occurrences
      * null=[], update=(fun m x -> m::x)  for recording the matchings
   *)

   val bounded_search: 
   config:Conllx_config.t ->
   ?ordering: string option ->  (* if value is "length", graph are considered by size, if value is "shuffle", graph order is randomiez, else a default order is used  *)
   int option ->                (* bound on the number of matching *)
   float option ->              (* Timeunt in seconds *)  
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

(* ==================================================================================================== *)
(** {2 Corpus_desc} *)
(* ==================================================================================================== *)
module Corpus_desc: sig

  type t

  val build_corpus: t -> Corpus.t
  val load_corpus_opt: t -> Corpus.t option

  val get_config: t -> Conllx_config.t
  val is_rtl: t -> bool
  val is_audio: t -> bool
  val get_id: t -> string
  
  (** return the displat mode of the given corpus: None --> dep, Some 0 --> graph, Some i --> subgraph at depth i *)
  val get_display: t -> int option

  val get_lang_opt: t -> string option
  val get_directory: t -> string

  val load_json: string -> t list

  val compile: ?force: bool -> ?grew_match: string ->  t -> unit

  val clean: t -> unit
end


