(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

open Grew_base
open Grew_types

open Grew_ast
open Grew_edge
open Grew_fs

(* ================================================================================ *)
module G_node = struct
  type t = {
      fs: G_fs.t;
      next: G_edge.t Massoc_gid.t;
      position: float;
      conll_root: bool;
    }

  let get_fs t = t.fs
  let set_fs t fs = {t with fs = fs}

  let get_next t = t.next

  let get_position t = t.position
  let set_position position t = { t with position }

  let empty = { fs = G_fs.empty; next = Massoc_gid.empty; position = -1.; conll_root=false }

  let is_conll_root t = t.conll_root

  let to_string t =
    Printf.sprintf "  fs=[%s]\n  next=%s\n"
      (G_fs.to_string t.fs)
      (Massoc_gid.to_string G_edge.to_string t.next)

  let to_gr t = if t.position < 0.
    then sprintf "[%s] " (G_fs.to_gr t.fs)
    else sprintf "(%g) [%s] " t.position (G_fs.to_gr t.fs)

  let add_edge g_edge gid_tar t =
    match Massoc_gid.add gid_tar g_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let get_annot_info t = G_fs.get_annot_info t.fs

  let build ?def_position (ast_node, loc) =
    let fs = G_fs.build ast_node.Ast.fs in
    let position = match (ast_node.Ast.position, def_position) with
      | (Some position, _) -> position
      | (None, Some position) -> position
      | (None, None) -> Error.bug "Cannot build a node without position" in
    (ast_node.Ast.node_id, { empty with fs; position })

  let of_conll ?loc line =
    if line = Conll.root
    then { empty with conll_root=true }
    else { empty with fs = G_fs.of_conll ?loc line; position = float_of_string line.Conll.num }

  let remove (id_tar : Gid.t) label t = {t with next = Massoc_gid.remove id_tar label t.next}

  let remove_key node_id t =
    try {t with next = Massoc_gid.remove_key node_id t.next} with Not_found -> t

  let merge_key ?(strict=false) src_id tar_id t =
    try Some {t with next = Massoc_gid.merge_key src_id tar_id t.next}
    with Massoc_gid.Duplicate -> if strict then None else Some t

  let shift_out ?(strict=false) src_t tar_t =
    try Some {tar_t with next = Massoc_gid.disjoint_union src_t.next tar_t.next}
    with Massoc_gid.Not_disjoint -> if strict then None else Some tar_t

  let rm_out_edges t = {t with next = Massoc_gid.empty}

  let build_neighbour t = { empty with position = (get_position t) +. 0.01 }

  let build_new t = { empty with position = (get_position t) +. 0.01 }

  let position_comp n1 n2 = Pervasives.compare n1.position n2.position

  let rename mapping n = {n with next = Massoc_gid.rename mapping n.next}
end
(* ================================================================================ *)

(* ================================================================================ *)
module P_node = struct
  type t = {
      name: Id.name;
      fs: P_fs.t;
      next: P_edge.t Massoc_pid.t;
      loc: Loc.t option;
    }

  let get_name t = t.name
  let get_fs t = t.fs
  let get_next t = t.next

  let unif_fs fs t = { t with fs = P_fs.unif fs t.fs }

  let empty = { fs = P_fs.empty; next = Massoc_pid.empty; name = ""; loc=None   }

  let build ?pat_vars (ast_node, loc) =
    (ast_node.Ast.node_id,
     {
       name = ast_node.Ast.node_id;
       fs = P_fs.build ?pat_vars ast_node.Ast.fs;
       next = Massoc_pid.empty;
       loc = Some loc;
     } )

  let add_edge p_edge pid_tar t =
    match Massoc_pid.add pid_tar p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let match_ ?param p_node g_node =
    (* (match param with None -> printf "<None>" | Some p -> printf "<Some>"; Lex_par.dump p); *)
    if P_fs.check_position ?param (G_node.get_position g_node) p_node.fs
    then P_fs.match_ ?param p_node.fs (G_node.get_fs g_node)
    else raise P_fs.Fail

  let compare_pos t1 t2 = Pervasives.compare t1.loc t2.loc
end
(* ================================================================================ *)
