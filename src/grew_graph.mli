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

open Grew_ast
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
  type t =
      { nodes: (Gid.t * (string * string list)) list;
        edges: (Gid.t * G_edge.t * Gid.t) list;
      }

  val empty:t
end (* module G_deco *)

(* ================================================================================ *)
module P_graph: sig
  type t = P_node.t Pid_map.t

  val empty: t

  val find: Pid.t -> t -> P_node.t

  val roots: t -> Pid.t list

  type extension = {
      ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
      old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *)
    }

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

  val normalize: t -> t

  (** raise ??? *)
  val max_binding: t -> int

  (** [edge_out t id edge] returns true iff there is an out-edge from the node [id] with a label compatible with [edge] *)
  val edge_out: t -> Gid.t -> P_edge.t -> bool

  (** [get_annot_info graph] searches for exactly one node with an annot-feature (with name starting with "__").
      It returns the annot-feature name without the prefix "__" together with the position.
      raise an [Build] exception if there is not exactly one annot-feature (with name starting with "__")  *)
  val get_annot_info: t -> (string * float)

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  val build: ?locals: Label.decl array -> Ast.gr -> t

  val of_conll: ?loc:Loc.t -> Conll.line list -> t

  val of_xml: Xml.xml -> t
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
  val activate: Loc.t -> Gid.t -> string -> t -> (Gid.t * t)

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
  val to_dep: ?filter : string list -> ?main_feat:string -> ?deco:G_deco.t -> t -> string
  val to_conll: t -> string

  val to_raw: t ->
    (string * string) list *
    (string * string) list list *
    (int * string * int) list
end (* module G_graph *)