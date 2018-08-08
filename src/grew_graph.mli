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

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain
open Grew_fs
open Grew_edge
open Grew_node
open Grew_command

(* ================================================================================ *)
module P_deco: sig
  type t =
      { nodes: Pid.t list;
        edges: (Pid.t * P_edge.t * Pid.t) list;
      }

  val empty:t
end (* module P_deco *)

(* ================================================================================ *)
module G_deco: sig
  (* value is (f, Some g) for combined request "f=v/g=u" and (j, None) else *)
  type highlighted_feat = string * string option

  type t = {
    nodes: (Gid.t * (string * highlighted_feat list)) list;  (* a list of (node, (pattern_id, features of nodes implied in the step)) *)
    edges: (Gid.t * G_edge.t * Gid.t) list;        (* an edge list *)
  }

  val empty:t
end (* module G_deco *)

(* ================================================================================ *)
module P_graph: sig
  type t = P_node.t Pid_map.t

  val empty: t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.json

  val find: Pid.t -> t -> P_node.t

  val roots: t -> Pid.t list

  val pid_name_list: t -> Id.name list

  type extension = {
      ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
      old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *)
    }

  (** It raises [P_fs.Fail_unif] exception in case of inconsistent feature structures. *)
  val build:
      ?domain:Domain.t ->
      Lexicons.t ->
      Ast.node list ->
      Ast.edge list ->
      (t * Id.table)

  (** It raises [P_fs.Fail_unif] exception in case of inconsistent feature structures. *)
  val build_extension:
      ?domain:Domain.t ->
      Lexicons.t ->
      Id.table ->
      Ast.node list ->
      Ast.edge list ->
      (extension * Id.table)
end (* module P_graph *)

(* ================================================================================ *)
module G_graph: sig
  type t

  val empty: t

  (** [find gid graph] return the g_node associated with [gid] in [graph].
      [Not_found] is raised if [gid] is not defined in [graph]. *)
  val find: Gid.t -> t -> G_node.t

  val equals: t -> t -> bool

  val node_exists: (G_node.t -> bool) -> t -> bool

  val fold_gid: (Gid.t -> 'a -> 'a) -> t -> 'a -> 'a

  val get_highest: t -> int

  (** [edge_out t id label_cst] returns true iff there is an out-edge from the node [id] with a label compatible with [label_cst] *)
  val edge_out: t -> Gid.t -> Label_cst.t -> bool

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  val build: ?domain:Domain.t -> ?grewpy: bool -> Ast.gr -> t

  val of_conll: ?domain:Domain.t -> Conll.t -> t
  val of_json: json -> t

  (** input : "Le/DET/le petit/ADJ/petit chat/NC/chat dort/V/dormir ./PONCT/."
      It supposes that "SUC" is defined in current relations *)
  val of_brown: ?domain:Domain.t -> ?sentid: string -> string -> t

  val of_pst: ?domain:Domain.t -> Ast.pst -> t

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Update functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  (** [add_edge graph id_src label id_tar] tries to add an edge grom [id_src] to [id_tar] with [label] to [graph].
      if it succeeds, [Some new_graph] is returned
      if it fails (the edge already exists), [None] is returned
  *)
  val add_edge: t -> Gid.t -> G_edge.t -> Gid.t -> t option

  (** [del_edge ?edge_ident loc graph id_src label id_tar] removes the edge (id_src -[label]-> id_tar) from graph.
     Log.critical if the edge is not in graph *)
  val del_edge: ?edge_ident: string -> Loc.t -> t -> Gid.t -> G_edge.t -> Gid.t -> t option

  (** [del_node graph id] remove node [id] from [graph], with all its incoming and outcoming edges.
      None is returned if [id] not defined in [graph]*)
  val del_node: t -> Gid.t -> t option

  val add_before: Gid.t -> t -> (Gid.t * t)
  val add_after: Gid.t -> t -> (Gid.t * t)
  val add_unordered: t -> (Gid.t * t)

  (** shift all crown-edges ending in [src_gid] to edges ending in [tar_gid] *)
  val shift_in:
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_in" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_in" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a pattern node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
      ( t *                                (* output graph *)
        (Gid.t * G_edge.t * Gid.t) list *  (* list of really deleted edges *)
        (Gid.t * G_edge.t * Gid.t) list    (* list of really added edges *)
      )

  (** shift all crown-edges starting from [src_gid] to edges starting from [tar_gid] *)
  val shift_out:
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_out" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_out" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a pattern node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
      ( t *                                (* output graph *)
        (Gid.t * G_edge.t * Gid.t) list *  (* list of really deleted edges *)
        (Gid.t * G_edge.t * Gid.t) list    (* list of really added edges *)
      )

  (** move all incident crown-edges from/to [src_gid] are moved to incident edges on node [tar_gid] from graph *)
  val shift_edges:
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_edges" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_edges" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a pattern node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
      ( t *                                (* output graph *)
        (Gid.t * G_edge.t * Gid.t) list *  (* list of really deleted edges *)
        (Gid.t * G_edge.t * Gid.t) list    (* list of really added edges *)
      )

  (** [update_feat domain tar_id tar_feat_name concat_items] sets the feature of the node [tar_id]
      with feature name [tar_feat_name] to be the contatenation of values described by the [concat_items].
      It returns both the new graph and the new feature value produced as the second element *)
  val update_feat: ?loc:Loc.t -> t -> Gid.t -> string -> Concat_item.t list -> (t * string)

  val set_feat: ?loc:Loc.t -> t -> Gid.t -> string -> string -> t

  (** [del_feat graph node_id feat_name] returns [graph] where the feat [feat_name] of [node_id] is deleted
      If the feature is not present, None is returned. *)
  val del_feat: t -> Gid.t -> string -> t option

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Output functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  val to_gr: t -> string
  val to_dot: ?main_feat:string -> ?deco:G_deco.t -> t -> string
  val to_sentence: ?main_feat:string -> ?deco:G_deco.t -> t -> string
  val to_dep: ?filter: (string -> bool) -> ?main_feat:string -> ?deco:G_deco.t -> t -> string
  val to_conll: t -> Conll.t
  val to_conll_string: t -> string
  val to_json: t -> json

  val cast: ?domain:Domain.t -> t -> t
end (* module G_graph *)

(* ================================================================================ *)
module Delta : sig
  type t

  val empty: t

  val del_node: Gid.t -> t -> t
  val add_edge: Gid.t -> Label.t -> Gid.t -> t -> t
  val del_edge: Gid.t -> Label.t -> Gid.t -> t -> t
  val set_feat: G_graph.t -> Gid.t -> feature_name -> value option -> t -> t
end (* module Delta *)

(* ================================================================================ *)
module Graph_with_history : sig
  type t = {
    seed: G_graph.t;
    delta: Delta.t;
    graph: G_graph.t;
    added_gids: (string * Gid.t) list;
  }

  val from_graph: G_graph.t -> t

  val compare: t -> t -> int
end (* module Graph_with_history *)

(* ================================================================================ *)
module Graph_with_history_set : Set.S with type elt = Graph_with_history.t
