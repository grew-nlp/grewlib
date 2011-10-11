open Grew_utils
open Grew_fs
open Grew_edge
open Grew_ast

module Node : sig
  type t = {
      fs : Feature_structure.t;
      pos : int option;
      next : Edge.t Massoc.t;
    }
	
  val build: ?domain:Ast.domain -> Ast.node -> (Id.name * t)

  val is_a : t -> t -> bool
  val empty : t
  val to_string : t -> string
end
    
