open Utils 
open Grew_edge
open Grew_fs

module Node = struct
  type t = {
      fs : Feature_structure.t;
      pos : int option;
      next : Edge.t Massoc.t; (* the massoc gives for each node [n], the (sorted) list of edge from the current node to [n] *)
    }

  let empty = {fs = Feature_structure.empty; pos=None; next = Massoc.empty}
      

  let build ?domain (ast_node, loc) =
    (ast_node.Ast.node_id, 
     { fs = Feature_structure.build ?domain ast_node.Ast.fs;
       pos = ast_node.Ast.position;
       next = Massoc.empty;
     } )
    

  (* Says that "pattern" t1 is a t2*)
  let is_a pattern graph = Feature_structure.compatible pattern.fs graph.fs

  let to_string t = 
    Printf.sprintf "[fs=%s ; next=%s]" 
      (Feature_structure.to_string t.fs)
      (Massoc.to_string Edge.to_string t.next)
end
