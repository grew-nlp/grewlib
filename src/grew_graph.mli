(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

open Grew_types
open Grew_utils
open Grew_ast
open Grew_fs
open Grew_edge
open Grew_node

(* ================================================================================ *)
module P_graph: sig
  type t = P_node.t Pid_map.t

  val empty: t

  val to_json_list: config:Conll_config.t -> ?base:t -> t -> Yojson.Basic.t list

  val find: Pid.t -> t -> P_node.t

  val get_name: Pid.t -> t list -> string
  val dump: t list -> t -> unit

  val roots: t -> Pid.t list

  val pid_name_list: t -> Id.name list

  (** [P_fs.Fail_unif] exception is raised in case of inconsistent feature structures. *)
  val of_ast:
    config:Conll_config.t ->
    Lexicons.t ->
    Ast.basic ->
    (t * Id.table * string list)

  (** [P_fs.Fail_unif] exception is raised in case of inconsistent feature structures. *)
  val of_ast_extension:
    config:Conll_config.t ->
    Lexicons.t ->
    Id.table ->
    string list ->
    Ast.node list ->
    Ast.edge list ->
    (
      (* description for new nodes and for all edges (Old -> New, Old -> Old or New -> New)
         Old nodes used as source in a edges (Old -> New, Old -> Old2) are also defined in this maps *)
      t *
      (* encoding of N[…] when N is already defined in core request: just consider the new fs as a filter *)
      P_fs.t Pid_map.t *
      (* table of indentifier for nodes specific to the extension *)
      Id.table *
      (* full set of edge identifier *)
      string list
    )
end (* module P_graph *)

(* ================================================================================ *)
module G_deco: sig
  (* value is (f, Some g) for combined clause "f=v/g=u" and (j, None) else *)
  type highlighted_feat = string * string option

  type t = {
    (* a list of (node, (pid, features of nodes implied in the step)) *)
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

  val track: G_deco.t -> (string * int) -> G_deco.t -> t -> t -> t

  val get_history: t -> (G_deco.t * (string * int) * G_deco.t * t) list

  val trace_depth: t -> int

  val clear_rules: t -> t

  (** [edge_out t id label_cst] returns true iff there is an out-edge from the node [id] with a label compatible with [label_cst] *)
  val edge_out: config:Conll_config.t -> t -> Gid.t -> Label_cst.t -> bool

  (** [covered t node edge graph] returns true iff the node is covered by the [edge]
      i.e. the position of the node is strictly between the two nodes of the edge *)
  val covered: G_node.t -> (Gid.t * G_edge.t * Gid.t ) -> t -> bool

  val edge_length_opt: (Gid.t * G_edge.t * Gid.t) -> t -> int option
  val edge_delta_opt: (Gid.t * G_edge.t * Gid.t) -> t -> int option

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  val of_pst: Ast.pst -> t

  val of_json: Yojson.Basic.t -> t
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


  val del_edge_feature_opt: ?loc:Loc.t -> string -> string -> (Gid.t * G_edge.t * Gid.t ) -> t -> (t * G_edge.t) option


  (** [del_node graph id] remove node [id] from [graph], with all its incoming and outcoming edges.
      None is returned if [id] is not defined in [graph]*)
  val del_node_opt: Gid.t -> t -> t option

  val add_before: Gid.t -> t -> (Gid.t * t)
  val add_after: Gid.t -> t -> (Gid.t * t)
  val add_unordered: t -> (Gid.t * t)

  val insert_before: Gid.t -> Gid.t -> t -> t
  val insert_after: Gid.t -> Gid.t -> t -> t

  val unorder_opt: Gid.t -> t -> t option

  (** shift all crown-edges ending in [src_gid] to edges ending in [tar_gid] *)
  val shift_in:
    config:Conll_config.t ->
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_in" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_in" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a request node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
    ( t                                  (* output graph *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really deleted edges *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really added edges *)
    )

  (** shift all crown-edges starting from [src_gid] to edges starting from [tar_gid] *)
  val shift_out:
    config:Conll_config.t ->
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_out" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_out" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a request node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
    ( t                                  (* output graph *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really deleted edges *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really added edges *)
    )

  (** move all incident crown-edges from/to [src_gid] are moved to incident edges on node [tar_gid] from graph *)
  val shift_edges:
    config:Conll_config.t ->
    Loc.t ->            (* localization of the command *)
    Gid.t ->            (* [src_gid] the source gid of the "shift_edges" *)
    Gid.t ->            (* [tar_gid] the target gid of the "shift_edges" *)
    (Gid.t -> bool) ->  (* a locality test: true iff the node is a request node *)
    Label_cst.t ->      (* what are the constraint on edge label *)
    t ->                (* input graph *)
    ( t                                  (* output graph *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really deleted edges *)
      * (Gid.t * G_edge.t * Gid.t) list  (* list of really added edges *)
    )

  (** [update_feat graph tar_id tar_feat_name value] sets the feature of the node [tar_id]
      with feature name [tar_feat_name] to be [value]. *)
  val update_feat: ?loc:Loc.t -> t -> Gid.t -> string -> Feature_value.t -> t

  (** [concat_feats_opt graph side src_id tar_id separator regexp] copy all feats of nodes [src_id] that match [regexp] to the node [tar_id].
      If a feature of the same name already exists in [tar_id], the two values are concatenated (separated by [separator]).
      The output is [None] if no changes are made on [tar_id], [Some (new_graph, trace)] else where [trace] is the list of updated features in [tar_id]
  *)
  val concat_feats_opt: ?loc:Loc.t -> t -> Ast.side ->  Gid.t -> Gid.t -> string -> string -> (t * (string * Feature_value.t) list) option

  (** [del_feat_opt graph node_id feat_name] returns [graph] where the feat [feat_name] of [node_id] is deleted
      If the feature is not present, [None] is returned. *)
  val del_feat_opt: t -> Gid.t -> string -> t option

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Output functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  val to_dot: ?main_feat:string -> ?deco:G_deco.t -> config:Conll_config.t -> t -> string
  val to_sentence: ?pivot: string -> ?deco:G_deco.t -> t -> string

  val to_sentence_audio: ?deco:G_deco.t -> t -> (string * (float * float) option)

  val to_dep: ?filter: (string -> bool) -> ?no_root:bool -> ?main_feat:string -> ?deco:G_deco.t -> config:Conll_config.t -> t -> string

  val to_json: t -> Yojson.Basic.t

  val append_in_ag_lex: string list -> t -> int Clustered.t -> int Clustered.t


  val is_projective: t -> bool

  type dfs_output = {
    forest: bool;
    tree: bool;
    cyclic: bool;
  }

  val depth_first_search: t -> dfs_output

  val get_meta_opt: string -> t -> string option

  val get_meta_list: t -> (string * string) list

  val set_meta: string -> string -> t -> t

  val get_feature_values: string -> t -> String_set.t
  val get_relations:  config:Conll_config.t -> t -> String_set.t
  val get_features: t -> String_set.t

  val subgraph: t -> Gid_map.key list -> int -> t
end (* module G_graph *)

(* ================================================================================ *)
module Delta : sig
  type t

  val init: Gid.t list -> t

  val del_node: Gid.t -> t -> t
  val add_edge: Gid.t -> G_edge.t -> Gid.t -> t -> t
  val del_edge: Gid.t -> G_edge.t -> Gid.t -> t -> t
  val set_feat: G_graph.t -> Gid.t -> string -> Feature_value.t option -> t -> t
  val unorder: Gid.t -> t -> t
  val insert_before: Gid.t -> Gid.t -> t -> t
  val insert_after: Gid.t -> Gid.t -> t -> t
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
    added_edges_in_rule: (Gid.t * G_edge.t * Gid.t) String_map.t;
  }

  val from_graph: G_graph.t -> t

  val compare: t -> t -> int
end (* module Graph_with_history *)

(* ================================================================================ *)
module Graph_with_history_set : Set.S with type elt = Graph_with_history.t

