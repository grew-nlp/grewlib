(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll
open Yojson.Basic

(* ==================================================================================================== *)
(** {2 General definitions} *)
(* ==================================================================================================== *)
module Libgrew : sig
  val get_version: unit -> string
  val set_debug_mode: bool -> unit
  val set_safe_commands: bool -> unit

  exception Error of string
  exception Bug of string
end

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

  val to_json: Pattern.t -> Grew_graph.G_graph.t -> t -> json

  val nodes: Pattern.t -> Grew_graph.G_graph.t -> t -> (string * string) list
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

  val of_json: json -> t
  val to_json: t -> json

  val of_brown: ?domain:Domain.t -> ?sentid:string -> string -> t

  val of_pst: ?domain:Domain.t -> string -> t

  val sentence_of_pst: ?domain:Domain.t -> string -> string

  val to_sentence: ?main_feat:string -> ?deco:Deco.t -> t -> string

  val to_dot : ?main_feat:string -> ?deco:Deco.t -> t -> string

  val to_dep : ?filter: (string -> bool) -> ?main_feat:string -> ?deco:Deco.t -> t -> string

  val to_gr: t -> string

  val to_conll: t -> Conll.t

  val to_conll_string: ?cupt:bool -> t -> string

  (** [search_pattern pattern graph] returns the list of the possible matching of [pattern] in [graph] *)
  val search_pattern: ?domain:Domain.t -> Pattern.t -> t -> Matching.t list
end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs : sig
  type t

  val load: string -> t

  val dump: t -> unit

  val domain: t -> Domain.t option

  val to_json: t -> json

  val get_strat_list: t -> string list
end

(* ==================================================================================================== *)
(** {2 Rewrite history} *)
(* ==================================================================================================== *)
module Rewrite: sig

  type display = Libgrew_types.rew_display

  val size: display -> int
  val set_max_rules: int -> unit
  val set_debug_loop: unit -> unit

  (** [display gr grs seq] builds the [display] (datatype used by the GUI) given by
      the rewriting of graph [gr] with the strategy [strat] of [grs].
      @param gr the graph to rewrite
      @param grs the graph rewriting system
      @param strat the name of the strategy to apply *)
  val display: gr:Graph.t -> grs:Grs.t -> strat:string -> display

  val at_least_one: grs:Grs.t -> strat:string -> bool
  val at_most_one: grs:Grs.t -> strat:string -> bool

  val set_timeout: float option -> unit

  val simple_rewrite: gr:Graph.t -> grs:Grs.t -> strat:string -> Graph.t list
end
