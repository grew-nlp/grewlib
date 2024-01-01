(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
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

  let get_next_basic t = Gid_massoc.filter G_edge.is_basic t.next

  let get_pred_opt t = Gid_massoc.find_opt (fun _ v -> v = G_edge.pred) t.next
  let get_succ_opt t = Gid_massoc.find_opt (fun _ v -> v = G_edge.succ) t.next

  let get_position_opt t = t.position
  let set_position p t = { t with position = Some p }
  let unset_position t = { t with position = None }

  let is_conll_zero t = G_fs.get_value_opt "form" t.fs = Some (String "__0__")

  let is_eud_empty t = match G_fs.get_value_opt "_UD_empty" t.fs with
    | Some (String "Yes") -> true
    | _ -> false

  let out_edges n =
    Gid_massoc.fold (fun acc _ edge -> if G_edge.is_fs edge then acc+1 else acc) 0 n.next
  
  let dump ~config t =
    Printf.sprintf "  fs=[%s]\n  next=%s\n"
      (G_fs.to_string t.fs)
      (Gid_massoc.to_string (G_edge.dump ~config) t.next)

  let empty = {
    name = None;
    fs = G_fs.empty;
    next = Gid_massoc.empty;
    position = None;
  }

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

  (* The third element in the output is [true] iff the is a capture:
    The edge resulting from the del_feat already exist in the graphs
    TODO: this should be documented and maybe a warning/error would be usefull *)
  let del_edge_feature_opt gid_tar old_edge feat_name t =
    match G_edge.remove_feat_opt feat_name old_edge with
    | None -> None
    | Some new_edge ->
      let without_old = Gid_massoc.remove gid_tar old_edge t.next in
      match Gid_massoc.add_opt gid_tar new_edge without_old with
      | Some new_next ->  Some ({t with next = new_next }, new_edge, false)
      | None -> Some ({t with next = without_old }, new_edge, true)

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
    fs_disj: P_fs.t list; (* dijunsction of matching constraints *)
    next: P_edge.t Pid_massoc.t;
    loc: Loc.t option;
  }

  let empty = { fs_disj = [P_fs.empty]; next = Pid_massoc.empty; name = "__empty__"; loc=None }
  let is_empty t = t.name = "__empty__"
  let get_name t = t.name

  let get_fs_disj t = t.fs_disj

  let get_next t = t.next

  let of_ast lexicons (ast_node, loc) =
    (ast_node.Ast.node_id,
      {
        name = ast_node.Ast.node_id;
        fs_disj = List.map (P_fs.of_ast lexicons) ast_node.Ast.fs_disj;
        next = Pid_massoc.empty;
        loc = Some loc;
      }
    )

  let unif_fs_disj fs_disj t = { t with fs_disj = P_fs.unif_disj t.fs_disj fs_disj }

  let add_edge_opt p_edge tar_pid t =
    match Pid_massoc.add_opt tar_pid p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let match_ ?(lexicons=[]) p_node g_node =
    match p_node.fs_disj with
    | [one] -> (P_fs.match_ ~lexicons one (G_node.get_fs g_node), 0)
    | fs_list ->
      (* NB: we compute all bool before [List.exists] in order to have the same behavior (run exception) whathever is the order of fs *)
      let is_fs_match_list = List.map
      (fun fs -> 
        try
          match P_fs.match_ ~lexicons fs (G_node.get_fs g_node) with
          | (true, _) -> Error.run "Lexicons and disjunction on nodes are incompatible"
          | _ -> true
        with P_fs.Fail -> false
      ) fs_list in
      match CCList.find_idx (fun x -> x) is_fs_match_list with
        | None -> raise P_fs.Fail
        | Some (idx,_) -> ((false, lexicons), idx)
  let compare_loc t1 t2 = Stdlib.compare t1.loc t2.loc
end (* module P_node *)
