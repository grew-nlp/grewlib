open Printf

open Grew_utils
open Grew_ast
open Grew_edge
open Grew_fs

(* ================================================================================ *)
module G_node = struct
  type t = {
      fs: G_fs.t;
      pos: int option;
      next: G_edge.t Massoc.t;
    }

  let get_fs t = t.fs
  let get_next t = t.next

  let set_fs t fs = {t with fs = fs}

  let empty = { fs = G_fs.empty; pos = None; next = Massoc.empty }

  let to_string t = 
    Printf.sprintf "  fs=[%s]\n  next=%s\n" 
      (G_fs.to_string t.fs)
      (Massoc.to_string G_edge.to_string t.next)

  let to_gr t = 
    sprintf "%s [%s] " 
      (match t.pos with Some i -> sprintf "(%d)" i | None -> "")
      (G_fs.to_gr t.fs)

  let add_edge g_edge gid_tar t =
    match Massoc.add gid_tar g_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let build (ast_node, loc) =
    (ast_node.Ast.node_id, 
     { fs = G_fs.build ast_node.Ast.fs;
       pos = ast_node.Ast.position;
       next = Massoc.empty;
     } )

  let of_conll line = {
      fs = G_fs.of_conll line;
      pos = Some line.Conll.num;
      next = Massoc.empty;
    }
    
    
  let remove id_tar label t = {t with next = Massoc.remove id_tar label t.next}
      
  let remove_key node_id t = 
    try {t with next = Massoc.remove_key node_id t.next} with Not_found -> t

  let merge_key src_id tar_id t = 
    try Some {t with next = Massoc.merge_key src_id tar_id t.next}
    with Massoc.Duplicate -> None

  let shift_out src_t tar_t = 
    try Some {tar_t with next = Massoc.disjoint_union src_t.next tar_t.next}
    with Massoc.Not_disjoint -> None

  let rm_out_edges t = {t with next = Massoc.empty}


  let build_neighbour t = {empty with pos = match t.pos with Some x -> Some (x+1) | None -> None}

  let pos_comp n1 n2 = Pervasives.compare n1.pos n2.pos
end
(* ================================================================================ *)

(* ================================================================================ *)
module P_node = struct
  type t = {
      fs: P_fs.t;
      next: P_edge.t Massoc.t;
    }

  let get_fs t = t.fs
  let get_next t = t.next

  let empty = { fs = P_fs.empty; next = Massoc.empty }
        
  let build ?pat_vars (ast_node, loc) =
    (ast_node.Ast.node_id, 
     { fs = P_fs.build ?pat_vars ast_node.Ast.fs;
       next = Massoc.empty;
     } )

  let add_edge p_edge pid_tar t =
    match Massoc.add pid_tar p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let match_ ?param p_node g_node = P_fs.match_ ?param p_node.fs (G_node.get_fs g_node)

end
(* ================================================================================ *)



