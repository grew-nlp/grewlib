open Grew_utils
open Grew_fs
open Grew_edge
open Grew_ast

(* ================================================================================ *)
module G_node: sig
  type t

  val empty: t

  val to_string: t -> string
  val to_gr: t -> string

  val get_fs: t -> Feature_structure.t
  val get_next: t -> G_edge.t Massoc.t

  val set_fs: t -> Feature_structure.t -> t

(* FIXME move Gid up and replace int by Gid.t *) 
  val remove: int -> G_edge.t -> t -> t 

  val remove_key: int -> t -> t 

  val merge_key: int -> int -> t -> t option
  val shift_out: t -> t -> t option

  val rm_out_edges: t -> t

  val add_edge: G_edge.t -> int -> t -> t option
  val build: ?domain:Ast.domain -> Ast.node -> (Id.name * t)
  val pos_comp: t -> t -> int

  val build_neighbour: t -> t
end
(* ================================================================================ *)

(* ================================================================================ *)
module P_node: sig
  type t

  val empty: t

  val get_fs: t -> Feature_structure.t
  val get_next: t -> P_edge.t Massoc.t

  val build: ?domain:Ast.domain -> Ast.node -> (Id.name * t)

  val add_edge: P_edge.t -> int -> t -> t option

  val is_a: t -> G_node.t -> bool

end
(* ================================================================================ *)
