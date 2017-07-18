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
open Conll

open Grew_base
open Grew_types

open Grew_ast
open Grew_edge
open Grew_fs

(* ================================================================================ *)
module G_node = struct
  type position =
  | Ordered of float
  | Unordered of int

  type t = {
      name: Id.name option;
      fs: G_fs.t;
      next: G_edge.t Massoc_gid.t;
      succ: Gid.t option;
      prec: Gid.t option;
      position: position;
      conll_root: bool;
      efs: (string * string) list;
    }

  let get_fs t = t.fs
  let set_fs fs t = {t with fs }

  let get_next t = t.next
  let set_next next t = {t with next }

  let get_position t = t.position
  let set_position p t = { t with position = Ordered p }

  let get_float t = match t.position with Ordered p -> p | Unordered i -> float i

  let get_prec t = t.prec
  let get_succ t = t.succ

  let set_succ id t = { t with succ = Some id }
  let set_prec id t = { t with prec = Some id }

  let remove_succ t = { t with succ = None }
  let remove_prec t = { t with prec = None }

  let get_name gid t = match t.name with
    | Some n -> n
    | None -> sprintf "_%s_" (Gid.to_string gid)

  let empty = { name=None; fs = G_fs.empty; next = Massoc_gid.empty; succ = None; prec = None; position = Unordered 0; conll_root=false; efs=[] }

  let is_conll_root t = t.conll_root

  let to_string ?domain t =
    Printf.sprintf "  fs=[%s]\n  next=%s\n"
      (G_fs.to_string t.fs)
      (Massoc_gid.to_string (G_edge.to_string ?domain) t.next)

  let to_gr t = sprintf "[%s] " (G_fs.to_gr t.fs)

  let add_edge g_edge gid_tar t =
    match Massoc_gid.add gid_tar g_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let string_efs n = match n.efs with
    | [] -> "_"
    | list -> String.concat "|" (List.map (fun (f,v) -> sprintf "%s=%s" f v) list)

  let current_index = ref 0
  let fresh_index () = decr current_index; !current_index

  let build ?domain ?prec ?succ ?position (ast_node, loc) =
    let fs = G_fs.build ?domain ast_node.Ast.fs in
    let pos = match position with None -> Unordered (fresh_index ()) | Some p -> Ordered p in
    { empty with name=Some ast_node.Ast.node_id; fs; position = pos; prec; succ }

  let float_of_conll_id = function
  | (i,None) -> float i
  | (i, Some j) when j >0 && j < 10 -> (float i) +. (float j) /. 10.
  | _ -> Error.bug "[float_of_conll_id]"

  let of_conll ?loc ?prec ?succ ?domain line =
    if line = Conll.root
    then { empty with conll_root=true; succ}
    else { empty with fs = G_fs.of_conll ?loc ?domain line; position = Ordered (float_of_conll_id line.Conll.id); prec; succ; efs=line.Conll.efs }

  let pst_leaf ?loc ?domain phon position =
    { empty with fs = G_fs.pst_leaf ?loc ?domain phon; position = Ordered (float position) }
  let pst_node ?loc ?domain cat position =
    { empty with fs = G_fs.pst_node ?loc ?domain cat; position = Ordered (float position) } (* TODO : change to Unordered *)

  let fresh ?prec ?succ pos = { empty with position = Ordered pos; prec; succ }
  let fresh_unordered () = { empty with position = Unordered (fresh_index ())}

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

  (* let build_neighbour t = { empty with position = (get_position t) +. 0.01 }

  let build_new t = { empty with position = (get_position t) +. 0.01 } *)

  let position_comp n1 n2 = Pervasives.compare n1.position n2.position

  let rename mapping n = {n with next = Massoc_gid.rename mapping n.next}
end (* module G_node *)

(* ================================================================================ *)
module P_node = struct
  type t = {
      name: Id.name;
      fs: P_fs.t;
      next: P_edge.t Massoc_pid.t;
      loc: Loc.t option;
    }

  let to_json ?domain t =
    let json_next = `List (
      Massoc_pid.fold
        (fun acc pid p_edge ->
          `Assoc [
            ("id", `String (Pid.to_string pid));
            ("label", P_edge.to_json ?domain p_edge);
          ] :: acc
        ) [] t.next
    ) in
    `Assoc [
      ("node_name", `String t.name);
      ("fs", P_fs.to_json ?domain t.fs);
      ("next", json_next)
    ]

  let get_name t = t.name
  let get_fs t = t.fs
  let get_next t = t.next

  let unif_fs fs t = { t with fs = P_fs.unif fs t.fs }

  let empty = { fs = P_fs.empty; next = Massoc_pid.empty; name = ""; loc=None   }

  let build ?domain ?pat_vars (ast_node, loc) =
    (ast_node.Ast.node_id,
     {
       name = ast_node.Ast.node_id;
       fs = P_fs.build ?domain ?pat_vars ast_node.Ast.fs;
       next = Massoc_pid.empty;
       loc = Some loc;
     } )

  let add_edge p_edge pid_tar t =
    match Massoc_pid.add pid_tar p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let match_ ?param p_node g_node =
    (* (match param with None -> printf "<None>" | Some p -> printf "<Some>"; Lex_par.dump p); *)
    match G_node.get_position g_node with
    | G_node.Unordered _ -> None
    | G_node.Ordered p ->
      if P_fs.check_position ?param (Some p) p_node.fs
      then P_fs.match_ ?param p_node.fs (G_node.get_fs g_node)
      else raise P_fs.Fail

  let compare_pos t1 t2 = Pervasives.compare t1.loc t2.loc
end (* module P_node *)
