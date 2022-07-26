(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
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
module Pattern : sig
  type basic

  type t

  val pid_name_list: t -> Id.name list

  val of_ast: config:Conllx.Conllx_config.t -> ?lexicons: Lexicons.t -> Ast.pattern -> t

  val build_whether: config:Conllx.Conllx_config.t -> t -> Ast.basic -> basic
end

(* ================================================================================ *)
module Matching : sig
  (** the type t encodes the graph morphism from a pattern to a graph *)
  type t

  val to_json: ?all_edges: bool -> Pattern.t -> G_graph.t -> t -> Yojson.Basic.t

  (** [node_matching pattern graph matching] return a assoc list (pid_name, gid_name) *)
  val node_matching: Pattern.t -> G_graph.t -> t -> (string * string) list

  (** [search_pattern_in_graph pattern graph] returns the list of matching of the [pattern] into the [graph] *)
  val search_pattern_in_graph: config:Conllx.Conllx_config.t -> ?lexicons: Lexicons.t -> Pattern.t -> G_graph.t -> t list

  (** [build_deco rule matching] builds the decoration of the [graph] illustrating the given [matching] of the [rule] *)
  (* NB: it can be computed independly from the graph itself! *)
  val build_deco: Pattern.t -> t -> G_deco.t

  (* [get_string_value_opt request pattern graph matching] returns the value corresponding to the request in the result of a previou result of match
     [request] can be:
    * the name of a feature value [N.feat] where [N] is a node declared in the kernel part of the pattern
    * the name of an edge featue [E.feat] where [e] is a edge declared in the kernel part of the pattern
    * one of the pseudo features [e.label], [e.length] or [e.delta]
  *)
  val get_string_value_opt: config:Conllx.Conllx_config.t -> string -> Pattern.t -> G_graph.t -> t -> string option

  val whether: config:Conllx.Conllx_config.t -> Pattern.basic -> Pattern.t -> G_graph.t -> t -> bool
  
  val subgraph: G_graph.t -> t -> int -> G_graph.t
end

(* ================================================================================ *)
module Rule : sig
  type t

  val reset_rules: unit -> unit

  val set_max_rules: int -> unit
  
  val get_nb_rules: unit -> int

  (** [get_name t] returns the name of the rule [t]. *)
  val get_name: t -> string

  (** [get_loc t] returns the file location of the rule [t]. *)
  val get_loc: t -> Loc.t

  val to_json_python: config:Conllx.Conllx_config.t -> t -> Yojson.Basic.t

  (** [to_dep t] returns a string in the [dep] language describing the match basic of the rule *)
  val to_dep: config:Conllx.Conllx_config.t -> t -> string

  (** [of_ast ast_rule] returns the Rule.t value corresponding to [ast_rule] *)
  val of_ast: config:Conllx.Conllx_config.t -> Ast.rule -> t

  val onf_apply_opt: config:Conllx.Conllx_config.t -> t -> G_graph.t -> G_graph.t option

  val gwh_apply: config:Conllx.Conllx_config.t -> t -> Graph_with_history.t -> Graph_with_history_set.t

  val owh_apply_opt: config:Conllx.Conllx_config.t -> t -> Graph_with_history.t -> Graph_with_history.t option

end (* module Rule *)
