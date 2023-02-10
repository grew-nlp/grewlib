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


module String_set : Set.S with type elt = string
module String_map : Map.S with type key = string
module String_opt_map : CCMap.S with type key = string option

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int

type cluster_item = Key of string | Whether of string

(* ================================================================================ *)
module Clustered : sig
  type 'a t
  (** the type Clustered.t stores the output of a (mutli) clustering operation.
      It is a recusive dict of homogeneous depth, dict keys are [string option].
      NB: the handling of the homogeneity is left to the user: in each function using a list,
      the length of the list must correspond to the depth of the structure.
  *)

  val depth: _ t -> int

  val empty: 'a -> 'a t
  (** The [empty] structure (the null value should be given) *)

  val get_opt: 'a -> string option list -> 'a t -> 'a
  (** [get_opt null key_list t] returns  the corresponding 'a value or [null] if it can not be found *)

  val nb_clusters: 'a t -> int
  (** Returns the number of element of type 'a stored in the structure *)

  val cardinal: ('a -> int) -> 'a t -> int
  (** Returns the number of element of type 'a stored in the structure *)

  val sizes: ('a -> int) -> 'a t -> int String_opt_map.t list
  (** [sizes f t] returns a list of map (the length of the list is the depth of [t]).
      each element of the list sums the usage of the key in all sub structs *)

  val build_layer: ('b -> 'a t) -> ('b -> string option) -> 'a -> 'b list -> 'a t
  (** [build_layer sub_fct key_fct null item_list] builds a structure which first layer
      associates from each [item] in [item_list], [key_fct item] to [sub_fct item] *)

  val update: ('a -> 'a) -> string option list -> 'a -> 'a t -> 'a t
  (** [update fct string_option_list null t] update the structure: 
      - if there is already an 'a value [x] at address [string_option_list],
        this value is updated to [fct x]
      - else a new entry is added with value [fct null]
  *)

  val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map fct t] apply the function [fct] to all leaves *)

  val fold: (string option list -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold fct t init] apply the function [fct] to all leaves *)

  val iter: (string option list -> 'a -> unit) -> 'a t -> unit
  (** [iter fct t] apply the function [fct] to all leaves *)

  val fold_layer: 
    ('a -> 'b) ->                        (* fct on leaves *)
    'c ->                                (* init value for node iteration *)
    (string option -> 'b -> 'c -> 'c) -> (* iteration of element of the node *)
    ('c -> 'b) ->                        (* closure *)
    'a t ->
    'b
  (** [fold fct_leaf null fct_node closure] 
      folds in all the structures (nodes and leaves).
      [fct_leaf] is applied on each leaf;
      on each node, [fct_node] is folded, starting from [null] and finally [closure] is applied to it
  *)

  val merge_keys: string option -> ('a -> 'a -> 'a) -> 'a -> (string option -> bool) list -> 'a t -> 'a t
  (** [merge_keys new_key merge_cell_fct null filter_functions input] 
      builds a new Clustered.t with the same data but where some keys are merged.
      [new_key] is the name of the new key used for merged keys (__*__ in grew-match grids)
      [merge_cell_fct] on merged cells;
      [filter_functions] is a list of boolean functions [f](one for each depth):
        * if [f key] is true, the key is kept
        * if [f key] is false, the corresponding data is merge into the [new_key] data 
  *)

  val prune_unambiguous: int -> 'a t -> 'a t
  (** [prune_unambiguous n t] prunes in input [t] with unambiguous structure at depth [n].
      For instance, if the keys are values for features [form, lemma, upos, Gender, Number],
      [prune_unambiguous 3 t] will keep only clusters entries where there is 
      more than one couple of value for Gender and Number with the same triple (form, lemma, upos).
  *)

  val dump: ('a -> string) -> 'a t -> unit
  (** outputs a raw display of the structure (to be used only for debug) *)

  val get_all_keys: int -> _ t -> string option list
  
end (* module Clustered *)



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

  val to_sentence_audio: ?deco:Deco.t -> t -> string * (float * float) option

  val to_dot : ?main_feat:string -> config:Conll_config.t -> ?deco:Deco.t -> t -> string

  val to_dep : ?filter: (string -> bool) -> ?no_root:bool -> ?main_feat:string -> ?deco:Deco.t -> config:Conll_config.t -> t -> string

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

  (** [parse_basic description] returns the basic described in the [descriprion] string *)
  val parse_basic: config:Conll_config.t -> t -> string -> basic

  val pid_name_list: t -> string list

  val of_json: config:Conll_config.t -> Yojson.Basic.t -> t
end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching: sig
  type t

  val to_json: ?all_edges:bool -> Request.t -> Graph.t -> t -> Yojson.Basic.t

  val nodes: Request.t -> Graph.t -> t -> (string * string) list

  (* [get_value_opt cluster_key request graph matching] returns the value corresponding to the cluster_key in the result of a previous result of match
      [cluster_key] can be:
      * the name of a feature value [N.feat] where [N] is a node declared in the kernel part of the request
      * the name of an edge featue [e.feat] where [e] is a edge declared in the kernel part of the request
  *)
  (* TODO: do not export: generalized by get_clust_value_opt *)
  val get_value_opt: config:Conll_config.t -> string -> Request.t -> Graph.t -> t -> string option

  (* TODO: do not export: generalized by get_clust_value_opt *)
  val whether: config:Conll_config.t -> Request.basic -> Request.t -> Graph.t -> t -> bool
  
  val subgraph: Graph.t -> t -> int -> Graph.t

  (** [search_request_in_graph request graph] returns the list of the possible matching of [request] in [graph] *)
  val search_request_in_graph: config:Conll_config.t -> Request.t -> Graph.t -> t list

  (** [build_deco request matching] returns the deco to be used in the graphical representation.
      WARNING: the function supposes that [matching] was find with the given [request]! *)
  val build_deco: Request.t -> t -> Deco.t

  val get_clust_value_opt:  ?json_label:bool -> config:Conll_config.t -> cluster_item ->  Request.t -> Graph.t -> t -> string option
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

  val dump: t -> unit

  val get_strat_list: t -> string list

  (** [get_strat_lists grs] returns two lists:
      * the full list of strategies defined
      * the sublist of "top" strategies: the ones not used as a substrat elsewhere *)
  val get_strat_lists: t -> (string list * string list)

  val get_package_list: t -> string list

  val get_rule_list: t -> string list

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
    cluster_item list ->    (* list of element used for clustering *)
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
    config:Conll_config.t ->
    ?ordering: string option ->  (* if value is "length", graph are considered by size, if value is "shuffle", graph order is randomiez, else a default order is used  *)
    int option ->                (* bound on the number of matching *)
    float option ->              (* Timeunt in seconds *)  
    'a ->                        (* null value to build clusters *)
    (int -> string -> int -> int -> Matching.t -> 'a -> 'a) ->
    (* update function to build clusters. Parameters ares:
       * int    --> graph_index in the corpus
       * string --> sent_id
       * int    --> position of the matching in the ≠ matchings for the same graph
       * int    --> number of matching in the current graph  *)
    Request.t ->
    cluster_item list ->         (* The list of element used for clustering *)
    t -> 
      ('a Clustered.t * string * float)  (* (output, statut, ratio) status is "ok", "timeout" or "over" *)
  (** search for a request in a corpus , with timeout and a bounded number of solutions *)

end

(* ==================================================================================================== *)
(** {2 Corpus_desc} *)
(* ==================================================================================================== *)
module Corpus_desc: sig

  type t

  val build: string -> string -> t

  val build_corpus: t -> Corpus.t
  val load_corpus_opt: t -> Corpus.t option

  val get_config: t -> Conll_config.t
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


(* ================================================================================ *)
module Sbn: sig
  val to_json: string -> Yojson.Basic.t
end