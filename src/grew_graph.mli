open Grew_ast
open Grew_fs
open Grew_edge
open Grew_node
open Grew_utils
open Grew_command

(* ================================================================================ *)
module P_deco: sig
  type t =
      { nodes: Pid.t list;
	edges: (Pid.t * P_edge.t * Pid.t) list;
      }

  val empty:t
end
(* ================================================================================ *)

(* ================================================================================ *)
module G_deco: sig
  type t =
      { nodes: Gid.t list;
	edges: (Gid.t * G_edge.t * Gid.t) list;
      }

  val empty:t
end
(* ================================================================================ *)

(* ================================================================================ *)
module P_graph: sig
  type t = P_node.t Pid_map.t

  val empty: t
  val find: Pid.t -> t -> P_node.t

  type extension = {
      ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
      old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *) 	
    }

  val build:
      ?pat_vars: string list ->
      ?locals: Label.decl array -> 
      Ast.node list -> 
      Ast.edge list -> 
	(t * Id.table * (Pid.t * P_fs.t) list )

  val build_extension:
      ?locals: Label.decl array -> 
      Id.table ->
      Ast.node list -> 
      Ast.edge list -> 
	(extension * Id.table)

  val roots: t -> Pid.t list

end
(* ================================================================================ *)



module G_graph: sig
  type t = {
      map: G_node.t Gid_map.t; (* node description *)
      lub: int;             (* least upper bound *)
    }


  val empty: t

  val find: Gid.t -> t -> G_node.t

  val build:
      ?locals: Label.decl array -> 
      Ast.node list -> 
      Ast.edge list -> 
	t

  val of_conll: ?loc:Loc.t -> Conll.line list -> t

  val to_gr: t -> string
  val to_dot: ?main_feat:string -> ?deco:G_deco.t -> t -> string
  val to_sentence: ?main_feat:string -> t -> string
  val to_dep: ?main_feat:string -> ?deco:G_deco.t -> t -> string


  type concat_item =
    | Feat of (Gid.t * string)
    | String of string

  val add_edge: t -> Gid.t -> G_edge.t -> Gid.t -> t option
  val del_edge : ?edge_ident: string -> Loc.t -> t -> Gid.t -> G_edge.t -> Gid.t -> t
  val del_node : t -> Gid.t -> t

  val add_neighbour : Loc.t -> t -> Gid.t -> G_edge.t -> (Gid.t * t) 
  val merge_node : Loc.t -> t -> Gid.t -> Gid.t -> t option

  val shift_in : Loc.t -> t -> Gid.t -> Gid.t -> t
  val shift_out : Loc.t -> t -> Gid.t -> Gid.t -> t
  val shift_edges : Loc.t -> t -> Gid.t -> Gid.t -> t

  (** [update_feat tar_id tar_feat_name concat_items] sets the feature of the node [tar_id] 
      with feature name [tar_feat_name] to be the contatenation of values described by the [concat_items].
      It returns both the new graph and the new feature value produced as the second element *)
  val update_feat: ?loc:Loc.t -> t -> Gid.t -> string -> concat_item list -> (t * string)

  val set_feat: ?loc:Loc.t -> t -> Gid.t -> string -> string -> t

  val del_feat: t -> Gid.t -> string -> t

  (** [edge_out t id edge] returns true iff there is an out-edge from the node [id] with a label compatible with [edge] *)
  val edge_out: t -> Gid.t -> P_edge.t -> bool

  val equals: t -> t -> bool

end

