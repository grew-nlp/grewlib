(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open CCOpt.Infix

open Grew_base
open Grew_types

open Grew_ast
open Grew_edge
open Grew_fs

(* ================================================================================ *)
module G_node = struct
  type t = {
    name: Id.name option;
    fs: G_fs.t;
    next: G_edge.t Massoc_gid.t;
    position: int option;
    efs: (string * string) list;
  }

  let compare n1 n2 = match (n1.position, n2.position) with
    | (Some i, Some j) -> Stdlib.compare i j
    | _ -> 0

  let get_name gid t = match t.name with
    | Some n -> n
    | None -> sprintf "_%s_" (Gid.to_string gid)
  let set_name n t = { t with name = Some n }

  let get_fs t = t.fs
  let set_fs fs t = {t with fs }

  let get_next t = t.next
  let set_next next t = { t with next }

  let get_next_without_pred_succ_enhanced t = Massoc_gid.filter
    (fun e -> not (G_edge.ordering e || G_edge.enhanced e)) t.next

  let get_pred_opt t = Massoc_gid.find_opt (fun _ v -> v = G_edge.pred) t.next
  let get_succ_opt t = Massoc_gid.find_opt (fun _ v -> v = G_edge.succ) t.next

  let get_position_opt t = t.position
  let set_position p t = { t with position = Some p }
  let unset_position t = { t with position = None }

  let is_conll_zero t = G_fs.get_value_opt "form" t.fs = Some (String "__0__")

  let is_eud_empty t = match G_fs.get_value_opt "_UD_empty" t.fs with
    | Some (String "Yes") -> true
    | _ -> false

  let dump ~config t =
    Printf.sprintf "  fs=[%s]\n  next=%s\n"
      (G_fs.to_string t.fs)
      (Massoc_gid.to_string (G_edge.dump ~config) t.next)

  let to_gr t = sprintf "[%s] " (G_fs.to_gr t.fs)

  let empty = {
    name = None;
    fs = G_fs.empty;
    next = Massoc_gid.empty;
    position = None;
    efs=[]
  }

  let of_ast ?position (ast_node, loc) =
    let fs = G_fs.of_ast ast_node.Ast.fs in
    { empty with name=Some ast_node.Ast.node_id; fs; position; }

  let build_pst_leaf ?loc phon =
    { empty with fs = G_fs.pst_leaf ?loc phon }

  let build_pst_node ?loc cat =
    { empty with fs = G_fs.pst_node ?loc cat }


  let add_edge g_edge gid_tar t =
    match Massoc_gid.add_opt gid_tar g_edge t.next with
    | Some l -> {t with next = l}
    | None -> t

  let add_edge_opt g_edge gid_tar t =
    match Massoc_gid.add_opt gid_tar g_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let remove_edge_opt gid_tar label t =
    match Massoc_gid.remove_opt gid_tar label t.next with
    | Some new_next -> Some {t with next = new_next}
    | None -> None

  let del_edge_feature_opt gid_tar old_edge feat_name t =
    match G_edge.remove_feat_opt feat_name old_edge with
    | None -> None
    | Some new_edge ->
      match Massoc_gid.add_opt gid_tar new_edge (Massoc_gid.remove gid_tar old_edge t.next) with
      | Some new_next -> Some ({t with next = new_next }, new_edge)
      | None -> None

  let remove_key node_id t =
    try {t with next = Massoc_gid.remove_key node_id t.next}
    with Not_found -> t

  let rename mapping n = {n with next = Massoc_gid.rename mapping n.next}

  let append_feats_opt ?loc src tar separator regexp =
    let src_fs = get_fs src in
    let tar_fs = get_fs tar in
    match G_fs.append_feats_opt ?loc src_fs tar_fs separator regexp with
    | Some (new_tar_fs, updated_feats) -> Some (set_fs new_tar_fs tar, updated_feats)
    | None -> None

  let shift user_id delta t =
    { t with
      name = CCOpt.map (fun n -> user_id ^ "_" ^ n) t.name;
      fs = G_fs.set_atom "user" user_id t.fs;
      next = Massoc_gid.map_key ((+) delta) t.next;
    }

  let unshift user_id t =
    match (
      CCOpt.map (fun x -> CCString.chop_prefix ~pre:(user_id^"_") x) t.name,
      G_fs.del_feat_opt "user" t.fs
    ) with
    | (Some name, Some fs) -> { t with name; fs }
    | (Some name, None) -> { t with name }
    | _ -> Error.run "[G_node.unshift] Inconsistent data"

  let insert_proj keys t proj =
    let fs = get_fs t in
    let values = List.map (fun k -> string_of_value <$> (G_fs.get_value_opt k fs)) keys in
    Projection.insert values proj
end (* module G_node *)

(* ================================================================================ *)
module P_node = struct
  type t = {
    name: Id.name;
    fs: P_fs.t;
    next: P_edge.t Massoc_pid.t;
    loc: Loc.t option;
  }

  let empty = { fs = P_fs.empty; next = Massoc_pid.empty; name = ""; loc=None }

  let get_name t = t.name

  let get_fs t = t.fs

  let get_next t = t.next

  let of_ast lexicons (ast_node, loc) =
    (ast_node.Ast.node_id,
     {
       name = ast_node.Ast.node_id;
       fs = P_fs.of_ast lexicons ast_node.Ast.fs;
       next = Massoc_pid.empty;
       loc = Some loc;
     } )

  let to_json_python ~config t =
    let json_next = `List (
        Massoc_pid.fold
          (fun acc pid p_edge ->
             `Assoc [
               ("id", `String (Pid.to_string pid));
               ("label", P_edge.to_json_python ~config p_edge);
             ] :: acc
          ) [] t.next
      ) in
    `Assoc [
      ("node_name", `String t.name);
      ("fs", P_fs.to_json_python t.fs);
      ("next", json_next)
    ]

  let unif_fs fs t = { t with fs = P_fs.unif fs t.fs }

  let add_edge_opt p_edge tar_pid t =
    match Massoc_pid.add_opt tar_pid p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let match_ ?lexicons p_node g_node = P_fs.match_ ?lexicons p_node.fs (G_node.get_fs g_node)

  let compare_pos t1 t2 = Stdlib.compare t1.loc t2.loc
end (* module P_node *)
