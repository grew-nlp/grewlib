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

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain
open Grew_fs
open Grew_edge
open Grew_node
open Grew_command


(* ================================================================================ *)
module P_graph: sig
  type t = P_node.t Pid_map.t

  val empty: t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t

  val find: Pid.t -> t -> P_node.t

  val roots: t -> Pid.t list

  val pid_name_list: t -> Id.name list

  type extension = {
    ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
    old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *)
  }

  (** [P_fs.Fail_unif] exception is raised in case of inconsistent feature structures. *)
  val build:
    ?domain:Domain.t ->
    Lexicons.t ->
    Ast.basic ->
    (t * Id.table * string list)

  (** [P_fs.Fail_unif] exception is raised in case of inconsistent feature structures. *)
  val build_extension:
    ?domain:Domain.t ->
    Lexicons.t ->
    Id.table ->
    string list ->
    Ast.node list ->
    Ast.edge list ->
    (extension * Id.table * string list)
end (* module P_graph *)

(* ================================================================================ *)
module G_deco: sig
  (* value is (f, Some g) for combined request "f=v/g=u" and (j, None) else *)
  type highlighted_feat = string * string option

  type t = {
    (* a list of (node, (pattern_id, features of nodes implied in the step)) *)
    nodes: (Gid.t * (string * highlighted_feat list)) list;
    (* an edge list *)
    edges: (Gid.t * G_edge.t * Gid.t) list;
  }

  val empty:t
end (* module G_deco *)

(* ================================================================================ *)
module G_graph: sig
  type t

  val empty: t

  (** Number of nodes *)
  val size: t -> int

  val is_initial: t -> bool

  (** [find gid graph] return the g_node associated with [gid] in [graph].
      [Not_found] is raised if [gid] is not defined in [graph]. *)
  val find: Gid.t -> t -> G_node.t
  val find_opt: Gid.t -> t -> G_node.t option

  val node_exists: (G_node.t -> bool) -> t -> bool

  val fold_gid: (Gid.t -> 'a -> 'a) -> t -> 'a -> 'a

  val push_rule: string -> t -> t

  val clear_rules: t -> t

  (** [edge_out t id label_cst] returns true iff there is an out-edge from the node [id] with a label compatible with [label_cst] *)
  val edge_out: t -> Gid.t -> Label_cst.t -> bool

  (** [covered t node edge graph] returns true iff the node is covered by the [edge]
      i.e. the position of the node is strictly between the two nodes of the edge *)
  val covered: G_node.t -> (Gid.t * G_edge.t * Gid.t ) -> t -> bool

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  val build: ?domain:Domain.t -> Ast.gr -> t

  val of_conll: ?domain:Domain.t -> Conll.t -> t
  val of_json: Yojson.Basic.t -> t

  (** input : "Le/DET/le petit/ADJ/petit chat/NC/chat dort/V/dormir ./PONCT/."
      It supposes that "SUC" is defined in current relations *)
  val of_brown: ?domain:Domain.t -> ?sentid: string -> string -> t

  val of_pst: ?domain:Domain.t -> Ast.pst -> t

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Update functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  (** [add_edge_opt graph src_gid label tar_gid] tries to add an edge grom [src_gid] to [tar_gid] with [label] to [graph].
      if it succeeds, [Some new_graph] is returned
      if it fails (the edge already exists), [None] is returned
  *)
  val add_edge_opt: Gid.t -> G_edge.t -> Gid.t -> t -> t option

  (** [del_edge ?loc src_gid label tar_gid graph] removes the edge (src_gid -[label]-> tar_gid) from graph. *)
  val del_edge_opt: ?loc:Loc.t -> Gid.t -> G_edge.t -> Gid.t -> t -> t option


  val update_edge_feature_opt: ?loc:Loc.t -> string -> string -> feature_value -> (Gid.t * G_edge.t * Gid.t ) -> t -> (t * G_edge.t) option

  val del_edge_feature_opt: ?loc:Loc.t -> string -> string -> (Gid.t * G_edge.t * Gid.t ) -> t -> (t * G_edge.t) option


  (** [del_node graph id] remove node [id] from [graph], with all its incoming and outcoming edges.
      None is returned if [id] is not defined in [graph]*)
  val del_node_opt: Gid.t -> t -> t option

  val add_before: Gid.t -> t -> (Gid.t * t)
  val add_after: Gid.t -> t -> (Gid.t * t)
  val add_unordered: t -> (Gid.t * t)

  val unorder_opt: Gid.t -> t -> t option

  (** shift all crown-edges ending in [src_gid] to edges ending in [tar_gid] *)
  val shift_in:
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_in" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_in" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a pattern node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
    ( t                                  (* output graph *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really deleted edges *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really added edges *)
    )

  (** shift all crown-edges starting from [src_gid] to edges starting from [tar_gid] *)
  val shift_out:
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_out" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_out" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a pattern node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
    ( t                                  (* output graph *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really deleted edges *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really added edges *)
    )

  (** move all incident crown-edges from/to [src_gid] are moved to incident edges on node [tar_gid] from graph *)
  val shift_edges:
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_edges" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_edges" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a pattern node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
    ( t                                  (* output graph *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really deleted edges *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really added edges *)
    )

  (** [update_feat graph tar_id tar_feat_name concat_items] sets the feature of the node [tar_id]
      with feature name [tar_feat_name] to be the contatenation of values described by the [concat_items].
      It returns both the new graph and the new feature value produced as the second element *)
  val update_feat: ?loc:Loc.t -> t -> Gid.t -> string -> Concat_item.t list -> (t * feature_value)


  (** [append_feats_opt graph src_id tar_id separator regexp] copy all feats of nodes [src_id] that match [regexp] to the node [tar_id].
      If a feature of the same name already exists in [tar_id], the two values are concatenated (separated by [separator]).
      The output is [None] if no changes are made on [tar_id], [Some (new_graph, trace)] else where [trace] is the list of updated features in [tar_id]
  *)
  val append_feats_opt: ?loc:Loc.t -> t -> Gid.t -> Gid.t -> string -> string -> (t * (string * feature_value) list) option

  val set_feat: ?loc:Loc.t -> t -> Gid.t -> string -> feature_value -> t

  (** [del_feat_opt graph node_id feat_name] returns [graph] where the feat [feat_name] of [node_id] is deleted
      If the feature is not present, [None] is returned. *)
  val del_feat_opt: t -> Gid.t -> string -> t option

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Output functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  val to_gr: t -> string
  val to_dot: ?main_feat:string -> ?get_url:(string -> string option) -> ?deco:G_deco.t -> t -> string
  val to_sentence: ?pivot: string -> ?deco:G_deco.t -> t -> string

  val to_orfeo: ?deco:G_deco.t -> t -> (string * (float * float) option)

  val to_dep: ?filter: (string -> bool) -> ?main_feat:string -> ?deco:G_deco.t -> t -> string
  val to_conll: t -> Conll.t
  val to_conll_string: ?cupt:bool -> t -> string
  val to_json: t -> Yojson.Basic.t

  val cast: ?domain:Domain.t -> t -> t

  val is_projective: t -> bool

  type dfs_output = {
    forest: bool;
    tree: bool;
    cyclic: bool;
  }

  val depth_first_search: t -> dfs_output

  val get_meta_opt: string -> t -> string option

  val set_meta: string -> string -> t -> t
end (* module G_graph *)

(* ================================================================================ *)
module Delta : sig
  type t

  val empty: t

  val del_node: Gid.t -> t -> t
  val add_edge: Gid.t -> G_edge.t -> Gid.t -> t -> t
  val del_edge: Gid.t -> G_edge.t -> Gid.t -> t -> t
  val set_feat: G_graph.t -> Gid.t -> feature_name -> feature_value option -> t -> t
  val unorder: Gid.t -> t -> t
end (* module Delta *)

(* ================================================================================ *)
module Graph_with_history : sig
  type t = {
    seed: G_graph.t;
    delta: Delta.t;
    graph: G_graph.t;
    added_gids: (string * Gid.t) list;
    e_mapping: (Gid.t * G_edge.t * Gid.t) String_map.t;
    added_gids_in_rule: (string * Gid.t) list;
  }

  val from_graph: G_graph.t -> t

  val compare: t -> t -> int
end (* module Graph_with_history *)

(* ================================================================================ *)
module Graph_with_history_set : Set.S with type elt = Graph_with_history.t

