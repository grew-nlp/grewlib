open Grew_ast
open Grew_utils
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
    | Param_in of int
    | Param_out of int

  type p = 
    | DEL_NODE of cnode
    | DEL_EDGE_EXPL of (cnode * cnode *G_edge.t) 
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (cnode * cnode * G_edge.t)
    | DEL_FEAT of (cnode * string)
    | UPDATE_FEAT of (cnode * string * item list)
    | NEW_NEIGHBOUR of (string * G_edge.t * pid)
    | SHIFT_EDGE of (cnode * cnode)
    | SHIFT_IN of (cnode * cnode)
    | SHIFT_OUT of (cnode * cnode)
    | MERGE_NODE of (cnode * cnode)

	
  type t = (p * Loc.t)  
  type h = 
    | H_DEL_NODE of gid
    | H_DEL_EDGE_EXPL of (gid * gid *G_edge.t) 
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (gid * gid * G_edge.t)
    | H_DEL_FEAT of (gid *string)
    | H_UPDATE_FEAT of (gid * string * string)
    | H_NEW_NEIGHBOUR of (string * G_edge.t * gid)
    | H_SHIFT_EDGE of (gid * gid)
    | H_SHIFT_IN of (gid * gid)
    | H_SHIFT_OUT of (gid * gid)
    | H_MERGE_NODE of (gid * gid)

  val build: 
      ?param: (string list * string list) ->
      (string list * string list) -> 
      Id.table -> 
      Label.decl array ->
      Ast.command -> 
        t * (string list * string list)
end

