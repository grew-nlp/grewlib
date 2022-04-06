(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conllx

module String_set : Set.S with type elt = string

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
(** {2 Projection} *)
(* ==================================================================================================== *)
module Projection : sig

  (* the type Projection.t encodes the projection of a corpus as a lexicon (used in ArboratogGrew). See [Graph.insert_proj] for usage.
     It a recusive map of homogeneous depth, key are string otion for feature values.
     [Graph.insert_proj] is supposed to used always the same key list when interacting with a projection.
  *)
  type t

  (* empty projection *)
  val empty: t

  (* [prune_unambiguous n proj] prune in input proj with unambiguous structure at depth n
    For instance, if the keys are values for features [form, lemma, upos, Gender, Number],
    prune_unambiguous 3 proj will keep only lexicon entries where there is 
    more than one couple of value for Gender and Number with the same triple (form, lemma, upos).
   *)
  val prune_unambiguous: int -> t -> t

  (* export the projection as a json data. The output is a list of object;
     each object containts: 
     * a key for each feature key used to build it (value are string of null)
     * a numeric key "freq" with the frequency of the corresponding values 
    Ex:
    [ {"freq": 3, "Gender": "Masc", "upos": "NOUN", "lemma": "État", "form": "États" },
      {"freq": 1, "Gender": null, "upos": "NOUN", "lemma": "État", "form": "États" } ]
     *)
  val to_json: string list -> t -> Yojson.Basic.t
end 



(* ==================================================================================================== *)
(** {2 Patterns} *)
(* ==================================================================================================== *)
module Pattern : sig
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

  val to_json: ?all_edges:bool -> Pattern.t -> Grew_graph.G_graph.t -> t -> Yojson.Basic.t

  val nodes: Pattern.t -> Grew_graph.G_graph.t -> t -> (string * string) list

  (* [get_value_opt request pattern graph matching] returns the value corresponding to the request in the result of a previou result of match
      [request] can be:
      * the name of a feature value [N.feat] where [N] is a node declared in the kernel part of the pattern
      * the name of an edge featue [e.feat] where [e] is a edge declared in the kernel part of the pattern
  *)
  val get_value_opt: config:Conllx_config.t -> string -> Pattern.t -> Grew_graph.G_graph.t -> t -> string option

  val whether: config:Conllx_config.t -> Pattern.basic -> Pattern.t -> Grew_graph.G_graph.t -> t -> bool
end

(* ==================================================================================================== *)
(** {2 Deco} *)
(* ==================================================================================================== *)
module Deco: sig
  type t = Grew_graph.G_deco.t

  (** [build pattern matching] returns the deco to be used in the graphical representation.
      WARNING: the function supposes that [matching] was find with the given [pattern]! *)
  val build: Pattern.t -> Matching.t -> t
end


(* ==================================================================================================== *)
(** {2 Graph} *)
(* ==================================================================================================== *)
module Graph : sig

  type t = Grew_graph.G_graph.t

  (** number of nodes *)
  val size: t -> int

  (** get a graph from a file either in 'gr' or 'conll' format.
      File extension should be '.gr' or '.conll'.
      @raise Parsing_err if libgrew can't parse the file
      @raise File_not_found if the file doesn't exists. *)
  val load: config:Conllx_config.t -> string -> t

  val of_gr: config:Conllx_config.t -> string -> t

  val of_json_python: config:Conllx_config.t -> Yojson.Basic.t -> t
  val to_json_python: config:Conllx_config.t -> t -> Yojson.Basic.t

  val of_json: Yojson.Basic.t -> t
  val to_json: t -> Yojson.Basic.t

  val of_brown: config:Conllx_config.t -> ?sentid:string -> string -> t

  val of_pst: string -> t

  val sentence_of_pst: string -> string

  val to_sentence: ?pivot: string -> ?deco:Deco.t -> t -> string

  val to_sentence_audio: ?deco:Deco.t -> t -> string * (float * float) option

  val to_dot : ?main_feat:string -> config:Conllx_config.t -> ?deco:Deco.t -> t -> string

  val to_dep : ?filter: (string -> bool) -> ?no_root:bool -> ?main_feat:string -> ?deco:Deco.t -> config:Conllx_config.t -> t -> string

  val to_gr: config:Conllx_config.t -> t -> string

  (** [search_pattern pattern graph] returns the list of the possible matching of [pattern] in [graph] *)
  val search_pattern: config:Conllx_config.t -> Pattern.t -> t -> Matching.t list

  val get_meta_opt: string -> t -> string option

  val get_meta_list: t -> (string * string) list

  val set_meta: string -> string -> t -> t

  val insert_proj: string list -> t -> Projection.t -> Projection.t

  val get_feature_values: string -> t -> String_set.t
  val get_relations: config:Conllx_config.t -> t -> String_set.t
  val get_features: t -> String_set.t

  val get_history: t -> (Deco.t * (string * int) * Deco.t * t) list
  val trace_depth: t -> int

  val to_raw: config:Conllx_config.t -> t -> (string * string) list * (string * string) list list * (int * string * int) list
end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs : sig
  type t

  val empty: t

  val load: config:Conllx_config.t -> string -> t

  val parse: config:Conllx_config.t -> string -> t

  val dump: t -> unit

  val to_json_python: config:Conllx_config.t -> t -> Yojson.Basic.t

  val get_strat_list: t -> string list

  (** [get_strat_lists grs] returns two lists:
      * the full list of strategies defined
      * the sublist of "top" strategies: the ones not used as a substrat elsewhere *)
  val get_strat_lists: t -> (string list * string list)

  val get_package_list: t -> string list

  val get_rule_list: t -> string list
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
end

module Corpus_desc: sig

  type t

  val build_corpus: t -> Corpus.t
  val load_corpus_opt: t -> Corpus.t option

  val get_config: t -> Conllx_config.t
  val get_columns_opt: t -> Conllx_columns.t option
  val is_rtl: t -> bool
  val is_audio: t -> bool
  val get_id: t -> string
  val get_lang_opt: t -> string option
  val get_directory: t -> string

  val load_json: string -> t list

  val compile: ?force: bool -> ?grew_match: string ->  t -> unit

  val clean: t -> unit

end


