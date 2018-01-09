(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

val libgrew_debug_mode: unit -> unit
val get_version: unit -> string

(* ==================================================================================================== *)
(** {2 Exceptions} *)
(* ==================================================================================================== *)
exception Error of string
exception Bug of string

(* ==================================================================================================== *)
(** {2 Domain} *)
(* ==================================================================================================== *)
module Domain : sig
  type t
  val load: string -> t
  val dump: t option -> unit
  val feature_names: t -> string list
end

(* ==================================================================================================== *)
(** {2 Patterns} *)
(* ==================================================================================================== *)
module Pattern : sig
  type t

  (** [load_pattern domain filename] returns the pattern described in the file *)
  val load: ?domain:Domain.t -> string -> t

  (** [load_pattern domain description] returns the pattern described in the [descriprion] string *)
  val parse: ?domain:Domain.t -> string -> t

  val pid_name_list: t -> string list

end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching: sig
  type t

  val to_python: Pattern.t -> Grew_graph.G_graph.t -> t -> string
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
  (** get a graph from a file either in 'gr' or 'conll' format.
      File extension should be '.gr' or '.conll'.
      @raise Parsing_err if libgrew can't parse the file
      @raise File_not_found if the file doesn't exists. *)
  val load: ?domain:Domain.t -> string -> t

  val of_gr: ?domain:Domain.t -> ?grewpy:bool -> string -> t

  val of_conll: ?domain:Domain.t -> Conll.t -> t

  val of_brown: ?domain:Domain.t -> ?sentid:string -> string -> t

  val of_pst: ?domain:Domain.t -> string -> t
  val sentence_of_pst: ?domain:Domain.t -> string -> string

  val to_sentence: ?main_feat:string -> t -> string

  val to_dot : ?domain:Domain.t -> ?main_feat:string -> ?deco:Deco.t -> t -> string

  val to_dep : ?domain:Domain.t -> ?filter: (string -> bool) -> ?main_feat:string -> ?deco:Deco.t -> t -> string

  val to_gr: ?domain:Domain.t -> t -> string

  val to_conll: ?domain:Domain.t -> t -> Conll.t

  val to_conll_string: ?domain:Domain.t -> t -> string

  (** [search_pattern pattern graph] returns the list of the possible matching of [pattern] in [graph] *)
  val search_pattern: ?domain:Domain.t -> Pattern.t -> t -> Matching.t list

  val node_matching: Pattern.t -> t -> Matching.t -> (string * float) list
end

(* OBSOLETE
(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Old_grs: sig

  type t
  val empty: t

  (** [load filename] loads a graph rewriting system from [filename]
      @raise Parsing_err if libgrew can't parse the file
      @raise File_not_found if the file doesn't exists *)
  val load: string -> t

  (** [get_sequence_names t] returns the list of sequence names defined in a GRS *)
  val get_sequence_names: t -> string list

  val get_domain: t -> Domain.t option

  val to_json: t -> string
end
*)

(* ==================================================================================================== *)
(** {2 New Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs : sig
  type t

  val load: string -> t
  val load_old: string -> t

  val dump: t -> unit

  val domain: t -> Domain.t option

  val to_json: t -> string

  val get_strat_list: t -> string list
end

(* ==================================================================================================== *)
(** {2 Rewrite history} *)
(* ==================================================================================================== *)
module Rewrite: sig

  type display = Libgrew_types.rew_display
  type history

  val size: display -> int
  val set_max_depth_det: int -> unit
  val set_max_depth_non_det: int -> unit
  val set_debug_loop: unit -> unit

  (** [display gr grs seq] builds the [display] (datatype used by the GUI) given by
      the rewriting of graph [gr] with the sequence [seq] of [grs].
      @param gr the grapth to rewrite
      @param grs the graph rewriting system
      @param seq the name of the sequence to apply *)
  val display: gr:Graph.t -> grs:Grs.t -> strat:string -> display

  val at_least_one: grs:Grs.t -> strat:string -> bool
  val at_most_one: grs:Grs.t -> strat:string -> bool

  val set_timeout: float option -> unit

  val simple_rewrite: gr:Graph.t -> grs:Grs.t -> strat:string -> Graph.t list

  val is_empty: history -> bool

  val num_sol: history -> int

  val save_gr: ?domain:Domain.t -> string -> history -> unit

  val save_conll: ?domain:Domain.t -> string -> history -> unit

  (** [save_full_conll base_name rh] saves one conll_file for each normal form defined in [rh].
      Output files are named according to [base_name] and a secondary index after "__".
      The number of conll file produced is returned. *)
  val save_full_conll: ?domain:Domain.t -> string -> history -> int

  val save_det_gr: ?domain:Domain.t -> string -> history -> unit

  val save_index: dirname:string -> base_names: string list -> unit

  val save_index: dirname:string -> base_names: string list -> unit

  val save_det_conll: ?domain:Domain.t -> ?header:string -> string -> history -> unit

  val det_dep_string: ?domain:Domain.t -> history -> string option

  val conll_dep_string: ?domain:Domain.t -> ?keep_empty_rh:bool -> history -> string option

  val save_index: dirname:string -> base_names: string array -> unit
end
