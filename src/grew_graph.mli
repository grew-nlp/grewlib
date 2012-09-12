open Grew_ast
open Grew_fs
open Grew_edge
open Grew_node
open Grew_utils
open Grew_command

(* ==================================================================================================== *)
module P_deco: sig
  type t =
      { nodes: Pid.t list;
	edges: (Pid.t * P_edge.t * Pid.t) list;
      }

  val empty:t
end (* module P_deco *)

(* ==================================================================================================== *)
module G_deco: sig
  type t =
      { nodes: Gid.t list;
	edges: (Gid.t * G_edge.t * Gid.t) list;
      }

  val empty:t
end (* module G_deco *)

(* ==================================================================================================== *)
module P_graph: sig
  type t = P_node.t Pid_map.t

  val empty: t

  val find: Pid.t -> t -> P_node.t

  val roots: t -> Pid.t list

  type extension = {
      ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
      old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *) 	
    }

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  val build:
      ?pat_vars: string list ->
      ?locals: Label.decl array ->
      Ast.node list ->
      Ast.edge list ->
	(t * Id.table)

  val build_extension:
      ?locals: Label.decl array ->
      Id.table ->
      Ast.node list ->
      Ast.edge list ->
	(extension * Id.table)
end (* module P_graph *)

(* ==================================================================================================== *)
module Concat_item : sig
  type t =
    | Feat of (Gid.t * string)
    | String of string
end (* module Concat_item *)

(* ==================================================================================================== *)
module G_graph: sig
  type t = G_node.t Gid_map.t

  val empty: t

  val find: Gid.t -> t -> G_node.t

  val equals: t -> t -> bool

  (** [edge_out t id edge] returns true iff there is an out-edge from the node [id] with a label compatible with [edge] *)
  val edge_out: t -> Gid.t -> P_edge.t -> bool

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  val build: ?locals: Label.decl array -> Ast.gr -> t

  val of_conll: ?loc:Loc.t -> Conll.line list -> t

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Update functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


  val rename: (Gid.t * Gid.t) list -> t -> t

  (** [add_edge graph id_src label id_tar] tries to add an edge grom [id_src] to [id_tar] with [label] to [graph].
      if it succeeds, [Some new_graph] is returned
      if it fails (the edge already exists), [None] is returned
  *)
  val add_edge: t -> Gid.t -> G_edge.t -> Gid.t -> t option

  (** [del_edge ?edge_ident loc graph id_src label id_tar] removes the edge (id_src -[label]-> id_tar) from graph.
     Log.critical if the edge is not in graph *)
  val del_edge: ?edge_ident: string -> Loc.t -> t -> Gid.t -> G_edge.t -> Gid.t -> t

  (** [del_node graph id] remove node [id] from [graph], with all its incoming and outcoming edges.
      [graph] is unchanged if the node is not in it. *)
  val del_node: t -> Gid.t -> t

  val add_neighbour: Loc.t -> t -> Gid.t -> G_edge.t -> (Gid.t * t)

  val merge_node: Loc.t -> t -> Gid.t -> Gid.t -> t option

  (** move all in arcs to id_src are moved to in arcs on node id_tar from graph, with all its incoming edges *)
  val shift_in: Loc.t -> t -> Gid.t -> Gid.t -> t

  (** move all out-edges from id_src are moved to out-edges out off node id_tar *)
  val shift_out: Loc.t -> t -> Gid.t -> Gid.t -> t

  (** move all incident arcs from/to id_src are moved to incident arcs on node id_tar from graph, with all its incoming and outcoming edges *)
  val shift_edges: Loc.t -> t -> Gid.t -> Gid.t -> t

  (** [update_feat tar_id tar_feat_name concat_items] sets the feature of the node [tar_id]
      with feature name [tar_feat_name] to be the contatenation of values described by the [concat_items].
      It returns both the new graph and the new feature value produced as the second element *)
  val update_feat: ?loc:Loc.t -> t -> Gid.t -> string -> Concat_item.t list -> (t * string)

  val set_feat: ?loc:Loc.t -> t -> Gid.t -> string -> string -> t

  (** [del_feat graph node_id feat_name] returns [graph] where the feat [feat_name] of [node_id] is deleted
      If the feature is not present, [graph] is returned. *)
  val del_feat: t -> Gid.t -> string -> t

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Output functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  val to_gr: t -> string
  val to_dot: ?main_feat:string -> ?deco:G_deco.t -> t -> string
  val to_sentence: ?main_feat:string -> t -> string
  val to_dep: ?main_feat:string -> ?deco:G_deco.t -> t -> string
end

