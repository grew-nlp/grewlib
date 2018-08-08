(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Yojson.Basic
open Grew_base
open Grew_types
open Grew_domain
open Grew_graph
open Grew_command
open Grew_edge
open Grew_fs
open Grew_ast


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

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.json

  (** [to_dep t] returns a string in the [dep] language describing the match basic of the rule *)
  val to_dep: ?domain:Domain.t -> t -> string

  (** [build domain ?local dir ast_rule] returns the Rule.t value corresponding to [ast_rule].
      [dir] is used for localisation of lp files *)
  val build: ?domain:Domain.t -> string -> Ast.rule -> t


  (** the type matching encodes the graph morphism from a pattern to a graph *)
  (* NB: it was made public for the grep mode *)
  type matching
  type pattern

  val pid_name_list: pattern -> Id.name list

  val to_python: pattern -> G_graph.t -> matching -> json

  val build_pattern: ?domain:Domain.t -> ?lexicons: Lexicons.t -> Ast.pattern -> pattern

  (** [node_matching pattern graph matching] return a assoc list (pid_name, gid.position) *)
  val node_matching: pattern -> G_graph.t -> matching -> (string * float) list

  (** [match_in_graph rule graph] returns the list of matching of the pattern of the rule into the graph *)
  val match_in_graph: ?domain:Domain.t -> ?lexicons: Lexicons.t -> ?param:Lex_par.t -> pattern -> G_graph.t -> matching list

  (** [match_deco rule matching] builds the decoration of the [graph] illustrating the given [matching] of the [rule] *)
  (* NB: it can be computed independly from the graph itself! *)
  val match_deco: pattern -> matching -> G_deco.t




  val wrd_apply: ?domain: Domain.t -> t -> (G_graph.t * Libgrew_types.big_step option) -> (G_graph.t * Libgrew_types.big_step) option

  val onf_apply: ?domain: Domain.t -> t -> G_graph.t -> G_graph.t option

  val gwh_apply: ?domain: Domain.t -> t -> Graph_with_history.t -> Graph_with_history_set.t




end (* module Rule *)
