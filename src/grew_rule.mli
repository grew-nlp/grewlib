(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_types

open Grew_graph
open Grew_command
open Grew_edge
open Grew_fs
open Grew_ast

(* ================================================================================ *)
module Instance : sig
  type t = {
    graph: G_graph.t;
    history: Command.h list;
    rules: string list;
    big_step: Libgrew_types.big_step option;
  }

  (** [from_graph graph] return a fresh instance based on the input [graph]. *)
  val from_graph: G_graph.t -> t

  (** [rev_steps t] reverses the small step list: during rewriting, the last rule
      is in the head of the list and the reverse is needed for display. *)
  val rev_steps: t -> t

  (** [refresh t] returns a fresh representation of the graph.
      Graphs are refreshed after each module. *)
  val refresh: t -> t

  (** [to_gr t] returns a string which contains the "gr" code of the current graph *)
  val to_gr: ?domain:Domain.t -> t -> string

  (** [to_conll_string t] returns a string which contains the "conll" code of the current graph *)
  val to_conll_string: ?domain:Domain.t -> t -> string

  (** [save_dep_png base t] writes a file "base.png" with the dep representation of [t].
      NB: if the Dep2pict is not available, nothing is done *)
  val save_dep_png: ?domain:Domain.t -> ?filter: string list -> ?main_feat: string -> string -> t -> float option
  val save_dep_svg: ?domain:Domain.t -> ?filter: string list -> ?main_feat: string -> string -> t -> float option

  (** [save_dot_png base t] writes a file "base.png" with the dot representation of [t] *)
  val save_dot_png: ?domain:Domain.t -> ?filter: string list -> ?main_feat: string -> string -> t -> unit
end (* module Instance *)

(* ================================================================================ *)
module Instance_set : Set.S with type elt = Instance.t

(* ================================================================================ *)
module Rule : sig
  type t

  (** [set_max_depth_det value] set the maximum rewriting depth in deterministic application of a module. *)
  val set_max_depth_det: int -> unit

  (** [set_max_depth_non_det value] set the maximum rewriting depth in non-deterministic application of a module. *)
  val set_max_depth_non_det: int -> unit

  (** [set_debug_loop ()] turns the debug mode on for loop: when the bound is reached, the graph is considered as a normal form.
      This is a kind of hack to be able to explore loops in GUI. *)
  val set_debug_loop: unit -> unit

  (** [get_name t] returns the name of the rule [t]. *)
  val get_name: t -> string

  (** [get_loc t] returns the file location of the rule [t]. *)
  val get_loc: t -> Loc.t

  (** [is_filter t] returns [true] iff the rule [t] is a filter rule. *)
  val is_filter: t -> bool

  (** [to_dep t] returns a string in the [dep] language describing the match basic of the rule *)
  val to_dep: ?domain:Domain.t -> t -> string

  (** [build domain ?local dir ast_rule] returns the Rule.t value corresponding to [ast_rule].
      [dir] is used for localisation of lp files *)
  val build: ?domain:Domain.t -> ?locals:Label_domain.decl array -> string -> Ast.rule -> t

  (** [normalize domain module_name ?confluent rule_list filter_list instance] returns two sets of good normal forms and bad normal forms *)
  (* raise Stop if some command fails to apply *)
  val normalize:
    ?domain:Domain.t ->
    string -> (* module name *)
    ?confluent:bool ->
    t list -> (* rule list *)
    t list -> (* filter list *)
    Instance.t ->
      Instance_set.t * Instance_set.t

  val one_step: ?domain: Domain.t -> string -> Instance.t -> t list -> Instance.t list

  (** the type matching encodes the graph morphism from a pattern to a graph *)
  (* NB: it was made public for the grep mode *)
  type matching
  type pattern

  val build_pattern: ?domain:Domain.t -> Ast.pattern -> pattern

  (** [node_matching pattern graph matching] return a assoc list (pid_name, gid.position) *)
  val node_matching: pattern -> G_graph.t -> matching -> (string * int) list

  (** [match_in_graph rule graph] returns the list of matching of the pattern of the rule into the graph *)
  val match_in_graph: ?domain:Domain.t -> ?param:Lex_par.t -> pattern -> G_graph.t -> matching list

  (** [match_deco rule matching] builds the decoration of the [graph] illustrating the given [matching] of the [rule] *)
  (* NB: it can be computed independly from the graph itself! *)
  val match_deco: pattern -> matching -> G_deco.t
end (* module Rule *)
