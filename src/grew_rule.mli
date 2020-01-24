(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_types
open Grew_domain
open Grew_graph
open Grew_command
open Grew_edge
open Grew_fs
open Grew_ast


(* ================================================================================ *)
module Pattern : sig
  type t

  val pid_name_list: t -> Id.name list

  val build: ?domain:Domain.t -> ?lexicons: Lexicons.t -> Ast.pattern -> t
end

(* ================================================================================ *)
module Rule : sig
  type t

  val reset_rules: unit -> unit

  val incr_rules: unit -> unit

  val set_max_rules: int -> unit

  (** [get_name t] returns the name of the rule [t]. *)
  val get_name: t -> string

  val get_long_name: t -> string

  (** [get_loc t] returns the file location of the rule [t]. *)
  val get_loc: t -> Loc.t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t

  (** [to_dep t] returns a string in the [dep] language describing the match basic of the rule *)
  val to_dep: ?domain:Domain.t -> t -> string

  (** [build ?domain ast_rule] returns the Rule.t value corresponding to [ast_rule] *)
  val build: ?domain:Domain.t -> Ast.rule -> t


  (** the type matching encodes the graph morphism from a pattern to a graph *)
  (* NB: it was made public for the grep mode *)
  type matching


  val matching_to_json: ?all_edges: bool -> Pattern.t -> G_graph.t -> matching -> Yojson.Basic.t


  (** [node_matching pattern graph matching] return a assoc list (pid_name, gid_name) *)
  val node_matching: Pattern.t -> G_graph.t -> matching -> (string * string) list

  (** [match_in_graph rule graph] returns the list of matching of the pattern of the rule into the graph *)
  val match_in_graph: ?domain:Domain.t -> ?lexicons: Lexicons.t -> Pattern.t -> G_graph.t -> matching list

  (** [match_deco rule matching] builds the decoration of the [graph] illustrating the given [matching] of the [rule] *)
  (* NB: it can be computed independly from the graph itself! *)
  val match_deco: Pattern.t -> matching -> G_deco.t




  val wrd_apply: ?domain: Domain.t -> t -> (G_graph.t * Libgrew_types.big_step option) -> (G_graph.t * Libgrew_types.big_step) option

  val onf_apply: ?domain: Domain.t -> t -> G_graph.t -> G_graph.t option

  val gwh_apply: ?domain: Domain.t -> t -> Graph_with_history.t -> Graph_with_history_set.t

  val owh_apply: ?domain: Domain.t -> t -> Graph_with_history.t -> Graph_with_history.t option

  (* [get value request pattern graph matching] returns the value corresponding to the request in the result of a previou result of match
    [request] can be:
    * the name of a edge declared in the positive part of the pattern
    * the name of a feature value [N.feat] where [N] is a node declared in the positive part of the pattern
    * the name of an edge featue [E.feat] where [e] is a edge declared in the positive part of the pattern
  *)
  val get_value: string -> Pattern.t -> G_graph.t -> matching -> string option



end (* module Rule *)
