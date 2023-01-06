(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_types
open Grew_utils
open Grew_graph
open Grew_command
open Grew_edge
open Grew_fs
open Grew_ast


(* ================================================================================ *)
module Request : sig
  type basic

  type t

  val pid_name_list: t -> Id.name list

  val of_ast: config:Conll.Conll_config.t -> ?lexicons: Lexicons.t -> Ast.request -> t

  val build_whether: config:Conll.Conll_config.t -> t -> Ast.basic -> basic

  val string_of_json: Yojson.Basic.t -> string

  val of_json: config:Conll.Conll_config.t -> Yojson.Basic.t -> t
end

(* ================================================================================ *)
module Matching : sig
  (** the type t encodes the graph morphism from a request to a graph *)
  type t

  val to_json: ?all_edges: bool -> Request.t -> G_graph.t -> t -> Yojson.Basic.t

  (** [node_matching request graph matching] return a assoc list (pid_name, gid_name) *)
  val node_matching: Request.t -> G_graph.t -> t -> (string * string) list

  (** [search_request_in_graph request graph] returns the list of matching of the [request] into the [graph] *)
  val search_request_in_graph: config:Conll.Conll_config.t -> ?lexicons: Lexicons.t -> Request.t -> G_graph.t -> t list

  (** [build_deco rule matching] builds the decoration of the [graph] illustrating the given [matching] of the [rule] *)
  (* NB: it can be computed independly from the graph itself! *)
  val build_deco: Request.t -> t -> G_deco.t

  (* [get_value_opt cluster_key request graph matching] returns the value corresponding to the cluster_key in the result of a previous result of match
     [cluster_key] can be:
     * the name of a feature value [N.feat] where [N] is a node declared in the kernel part of the request
     * the name of an edge featue [E.feat] where [e] is a edge declared in the kernel part of the request
     * one of the pseudo features [e.label], [e.length] or [e.delta]
  *)
  val get_value_opt: ?json_label:bool -> config:Conll.Conll_config.t -> string -> Request.t -> G_graph.t -> t -> string option

  val whether: config:Conll.Conll_config.t -> Request.basic -> Request.t -> G_graph.t -> t -> bool
  
  val subgraph: G_graph.t -> t -> int -> G_graph.t

  val get_clust_value_opt: ?json_label:bool -> config:Conll.Conll_config.t -> cluster_item ->  Request.t -> G_graph.t -> t -> string option
end (* module Matching *)

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

  val to_json: config:Conll.Conll_config.t -> t -> Yojson.Basic.t

  (** [to_dep t] returns a string in the [dep] language describing the match basic of the rule *)
  val to_dep: config:Conll.Conll_config.t -> t -> string

  (** [of_ast ast_rule] returns the Rule.t value corresponding to [ast_rule] *)
  val of_ast: config:Conll.Conll_config.t -> Ast.rule -> t

  val onf_apply_opt: config:Conll.Conll_config.t -> t -> G_graph.t -> G_graph.t option

  val gwh_apply: config:Conll.Conll_config.t -> t -> Graph_with_history.t -> Graph_with_history_set.t

  val owh_apply_opt: config:Conll.Conll_config.t -> t -> Graph_with_history.t -> Graph_with_history.t option

  val string_of_json: Yojson.Basic.t -> Yojson.Basic.t -> string
  
end (* module Rule *)
