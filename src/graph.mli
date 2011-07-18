open Ast
open Grew_edge
open Grew_node
open Utils
open Edge
open Command

module Deco: sig
  type t = 
      { nodes: int list;
	edges: (int * Label.t * int) list;
      }
  val empty:t
end

module Graph : sig
  type t  = {
      map: Node.t IntMap.t; (* node description *)
      lub: int;             (* least upper bound *)
    }

  val empty: t

  val build:
      ?domain: Ast.domain -> 
      ?locals: Label.decl array -> 
      Ast.node list -> 
      Ast.edge list -> 
	(t * Id.table)

  (* a type for extension of graph: a former graph exists: in grew the former is a positive pattern and an extension is a "without" *)
  type extention = {
      ext_map: Node.t IntMap.t; (* node description *)
      old_map: Node.t IntMap.t; (* a partial map on old nodes for edge "Old -> New" and/or for new constraints on old nodes "Old [...]" *) 	
    }

  val build_extention:
      ?domain: Ast.domain -> 
      ?locals: Label.decl array -> 
      Id.table ->
      Ast.node list -> 
      Ast.edge list -> 
	(extention * Id.table)

  val find: int -> t -> Node.t
  val to_dot: ?deco:Deco.t -> t -> string
  val to_dep: ?main_feat:string -> ?deco:Deco.t -> t -> string

  val add_edge : t -> int -> Edge.t -> int -> t option
  val del_edge : Loc.t -> t -> int -> Edge.t -> int -> t
  val del_node : t -> int -> t

  val add_neighbour : Loc.t -> t -> int -> Edge.t -> (int * t) 
  val merge_node : Loc.t -> t -> int -> int -> t option
  val shift_edges : Loc.t -> t -> int -> int -> t

  (** [cpy_feat src_id tar_id src_feat_name tar_feat_name] copy the feature value associated with [src_feat_name] from 
   the node [src_id] to the node [tar_id] with feature name [tar_feat_name] *)
  val cpy_feat : t -> int -> int -> string -> string -> t

  val add_feat : t -> int -> string -> string -> t
  val del_feat : t -> int -> string -> t

  val equals : t -> t -> bool

  val edge_out: t -> int -> Edge.t -> bool

  val roots: t -> int list
end

