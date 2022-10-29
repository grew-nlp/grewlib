(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open CCOption.Infix

open Grew_types
open Grew_utils

open Grew_ast
open Grew_edge
open Grew_fs

(* ================================================================================ *)
module G_node = struct
  type t = {
    name: Id.name option;
    fs: G_fs.t;
    next: G_edge.t Gid_massoc.t;
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

  let get_next_without_pred_succ_enhanced t = Gid_massoc.filter
    (fun e -> not (G_edge.ordering e || G_edge.enhanced e)) t.next

  let get_pred_opt t = Gid_massoc.find_opt (fun _ v -> v = G_edge.pred) t.next
  let get_succ_opt t = Gid_massoc.find_opt (fun _ v -> v = G_edge.succ) t.next

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
      (Gid_massoc.to_string (G_edge.dump ~config) t.next)

  let empty = {
    name = None;
    fs = G_fs.empty;
    next = Gid_massoc.empty;
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
    match Gid_massoc.add_opt gid_tar g_edge t.next with
    | Some l -> {t with next = l}
    | None -> t

  let add_edge_opt g_edge gid_tar t =
    match Gid_massoc.add_opt gid_tar g_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let remove_edge_opt gid_tar label t =
    match Gid_massoc.remove_opt gid_tar label t.next with
    | Some new_next -> Some {t with next = new_next}
    | None -> None

  let del_edge_feature_opt gid_tar old_edge feat_name t =
    match G_edge.remove_feat_opt feat_name old_edge with
    | None -> None
    | Some new_edge ->
      match Gid_massoc.add_opt gid_tar new_edge (Gid_massoc.remove gid_tar old_edge t.next) with
      | Some new_next -> Some ({t with next = new_next }, new_edge)
      | None -> None

  let remove_key node_id t =
    try {t with next = Gid_massoc.remove_key node_id t.next}
    with Not_found -> t

  let rename mapping n = {n with next = Gid_massoc.rename mapping n.next}

  let concat_feats_opt ?loc side src tar separator regexp =
    let src_fs = get_fs src in
    let tar_fs = get_fs tar in
    match G_fs.concat_feats_opt ?loc side src_fs tar_fs separator regexp with
    | Some (new_tar_fs, updated_feats) -> Some (set_fs new_tar_fs tar, updated_feats)
    | None -> None

  let shift user_id delta t =
    { t with
      name = CCOption.map (fun n -> user_id ^ "_" ^ n) t.name;
      fs = G_fs.set_atom "user" user_id t.fs;
      next = Gid_massoc.map_key ((+) delta) t.next;
    }

  let unshift user_id t =
    match (
      CCOption.map (fun x -> CCString.chop_prefix ~pre:(user_id^"_") x) t.name,
      G_fs.del_feat_opt "user" t.fs
    ) with
    | (Some name, Some fs) -> { t with name; fs }
    | (Some name, None) -> { t with name }
    | _ -> Error.run "[G_node.unshift] Inconsistent data"

  let append_in_ag_lex feature_name_list t ag_lex =
    let fs = get_fs t in
    let value_list = List.map (fun feature_name -> Feature_value.to_string <$> (G_fs.get_value_opt feature_name fs)) feature_name_list in
    Clustered.update (fun x -> x+1) value_list 0 ag_lex
end (* module G_node *)

(* ================================================================================ *)
module P_node = struct
  type t = {
    name: Id.name;
    fs: P_fs.t;
    next: P_edge.t Pid_massoc.t;
    loc: Loc.t option;
  }

  let empty = { fs = P_fs.empty; next = Pid_massoc.empty; name = ""; loc=None }

  let get_name t = t.name

  let get_fs t = t.fs

  let get_next t = t.next

  let of_ast lexicons (ast_node, loc) =
    (ast_node.Ast.node_id,
     {
       name = ast_node.Ast.node_id;
       fs = P_fs.of_ast lexicons ast_node.Ast.fs;
       next = Pid_massoc.empty;
       loc = Some loc;
     } )

  let to_json_python ~config t =
    let json_next = `List (
        Pid_massoc.fold
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
    match Pid_massoc.add_opt tar_pid p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let match_ ?lexicons p_node g_node = P_fs.match_ ?lexicons p_node.fs (G_node.get_fs g_node)

  let compare_pos t1 t2 = Stdlib.compare t1.loc t2.loc
end (* module P_node *)
