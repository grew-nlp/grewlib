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

  val get_fs: t -> G_fs.t
  val get_next: t -> G_edge.t Massoc.t

  val set_fs: t -> G_fs.t -> t

(* FIXME move Gid up and replace int by Gid.t *) 
  val remove: int -> G_edge.t -> t -> t 

  val remove_key: int -> t -> t 

  val merge_key: int -> int -> t -> t option
  val shift_out: t -> t -> t option

  val rm_out_edges: t -> t

  val add_edge: G_edge.t -> int -> t -> t option
  val build: Ast.node -> (Id.name * t)
  val of_conll: Conll.line -> t

  val pos_comp: t -> t -> int

  val build_neighbour: t -> t
end
(* ================================================================================ *)

(* ================================================================================ *)
module P_node: sig
  type t

  val empty: t

  val get_fs: t -> P_fs.t
  val get_next: t -> P_edge.t Massoc.t

  val build: ?pat_vars: string list -> Ast.node -> (Id.name * t)

  val add_edge: P_edge.t -> int -> t -> t option

  val match_: ?param: Lex_par.t -> t -> G_node.t -> Lex_par.t option

end
(* ================================================================================ *)
