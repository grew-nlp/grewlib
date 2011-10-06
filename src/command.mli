open Ast
open Utils
open Grew_edge

module Command : sig
  type pid = int (* the int in the pattern *)
  type gid = int (* the int in the graph *)

  type cnode =       (* a command node is either: *)
    | Pid of pid     (* a node identified in the pattern *)
    | New of string  (* a node introduced by a new_neighbour *)

  type item =
    | Feat of (cnode * string)
    | String of string

  type p = 
    | DEL_NODE of cnode
    | DEL_EDGE_EXPL of (cnode * cnode *Edge.t) 
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (cnode * cnode * Edge.t)
    | DEL_FEAT of (cnode * string)
    | UPDATE_FEAT of (cnode * string * item list)
    | NEW_NEIGHBOUR of (string * Edge.t * pid)
    | SHIFT_EDGE of (cnode * cnode)
    | MERGE_NODE of (cnode * cnode)

	
  type t = (p * Loc.t)  
  type h = 
    | H_DEL_NODE of gid
    | H_DEL_EDGE_EXPL of (gid * gid *Edge.t) 
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (gid * gid * Edge.t)
    | H_DEL_FEAT of (gid *string)
    | H_UPDATE_FEAT of (gid * string * string)
    | H_NEW_NEIGHBOUR of (string * Edge.t * gid)
    | H_SHIFT_EDGE of (gid * gid)
    | H_MERGE_NODE of (gid * gid)

  val build: ?domain:Ast.domain -> Id.table -> Label.decl array -> Ast.command -> t

end
