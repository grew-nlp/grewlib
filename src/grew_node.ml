(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
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
  type t = {
    name: Id.name option;
    fs: G_fs.t;
    next: G_edge.t Massoc_gid.t;
    succ: Gid.t option;
    prec: Gid.t option;
    position: int option;
    efs: (string * string) list;
  }

  let compare n1 n2 = match (n1.position, n2.position) with
    | (Some i, Some j) -> Stdlib.compare i j
    | _ -> 0

  let get_name gid t = match t.name with
    | Some n -> n
    | None -> sprintf "_%s_" (Gid.to_string gid)

  let get_fs t = t.fs
  let set_fs fs t = {t with fs }

  let get_next t = t.next
  let set_next next t = {t with next }

  let get_prec t = t.prec
  let set_prec id t = { t with prec = Some id }
  let remove_prec t = { t with prec = None }

  let get_succ t = t.succ
  let set_succ id t = { t with succ = Some id }
  let remove_succ t = { t with succ = None }

  let get_position t = t.position
  let set_position p t = { t with position = Some p }

  let is_eud_empty t = match G_fs.get_string_atom "_UD_empty" t.fs with
    | Some "Yes" -> true
    | _ -> false

  let to_string ?domain t =
    Printf.sprintf "  fs=[%s]\n  next=%s\n"
      (G_fs.to_string t.fs)
      (Massoc_gid.to_string (G_edge.to_string ?domain) t.next)

  let to_gr t = sprintf "[%s] " (G_fs.to_gr t.fs)

  let empty = {
    name = None;
    fs = G_fs.empty;
    next = Massoc_gid.empty;
    succ = None;
    prec = None;
    position = None;
    efs=[]
  }

  let build ?prec ?succ ?position () = { empty with position; prec; succ }

  let build_from_ast ?domain ?prec ?succ ?position (ast_node, loc) =
    let fs = G_fs.build ?domain ast_node.Ast.fs in
    { empty with name=Some ast_node.Ast.node_id; fs; position; prec; succ }

  let build_from_conll ?loc ?domain ?prec ?succ position line =
    if line = Conll.root
    then { empty with position=Some 0; succ; name = Some "ROOT" }
    else { empty with
           fs = G_fs.of_conll ?loc ?domain line;
           position;
           prec;
           succ;
           efs=line.Conll.efs;
           name = Some (Conll_types.Id.to_string line.Conll.id)
         }

  let build_pst_leaf ?loc ?domain phon =
    { empty with fs = G_fs.pst_leaf ?loc ?domain phon }

  let build_pst_node ?loc ?domain cat =
    { empty with fs = G_fs.pst_node ?loc ?domain cat }


  let add_edge g_edge gid_tar t =
    match Massoc_gid.add_opt gid_tar g_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let remove_edge gid_tar label t =
    match Massoc_gid.remove_opt gid_tar label t.next with
    | Some new_next -> Some {t with next = new_next}
    | None -> None

  let update_edge gid_tar old_edge feat_name new_value t =
    let new_edge = G_edge.update feat_name new_value old_edge in
    if new_edge = old_edge
    then None (* the edge is not modified --> not safe *)
    else
      let without_edge = Massoc_gid.remove gid_tar old_edge t.next in
      match Massoc_gid.add_opt gid_tar new_edge without_edge with
      | Some new_next -> Some ({t with next = new_next }, new_edge)
      | None -> (* the produced edge already exists, just remove the old one *)
        Some ({t with next = without_edge }, new_edge)

  let del_edge_feature gid_tar old_edge feat_name t =
    match G_edge.remove feat_name old_edge with
    | None -> None
    | Some new_edge ->
      match Massoc_gid.add_opt gid_tar new_edge (Massoc_gid.remove gid_tar old_edge t.next) with
      | Some new_next -> Some ({t with next = new_next }, new_edge)
      | None -> None

  let remove_key node_id t =
    try {t with next = Massoc_gid.remove_key node_id t.next}
    with Not_found -> t

  let rename mapping n = {n with next = Massoc_gid.rename mapping n.next}

  let append_feats ?loc src tar separator regexp =
    let src_fs = get_fs src in
    let tar_fs = get_fs tar in
    match G_fs.append_feats ?loc src_fs tar_fs separator regexp with
    | Some (new_tar_fs, updated_feats) -> Some (set_fs new_tar_fs tar, updated_feats)
    | None -> None

  let shift user_id delta t =
    { t with
      name = CCOpt.map (fun n -> user_id ^ "_" ^ n) t.name;
      fs = G_fs.set_atom "user" user_id t.fs;
      next = Massoc_gid.map_key ((+) delta) t.next;
      prec = CCOpt.map ((+) delta) t.prec;
      succ = CCOpt.map ((+) delta) t.succ;
    }

  let unshift user_id t =
    match (
      CCOpt.map (fun x -> CCString.chop_prefix ~pre:(user_id^"_") x) t.name,
      G_fs.del_feat "user" t.fs
    ) with
    | (Some name, Some fs) -> { t with name; fs }
    | (Some name, None) -> { t with name }
    | _ -> Error.run "[G_node.unshift] Inconsistent data"

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

  let build_from_ast ?domain lexicons (ast_node, loc) =
    (ast_node.Ast.node_id,
     {
       name = ast_node.Ast.node_id;
       fs = P_fs.build ?domain lexicons ast_node.Ast.fs;
       next = Massoc_pid.empty;
       loc = Some loc;
     } )

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

  let unif_fs fs t = { t with fs = P_fs.unif fs t.fs }

  let add_edge p_edge pid_tar t =
    match Massoc_pid.add_opt pid_tar p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let match_ ?lexicons p_node g_node = P_fs.match_ ?lexicons p_node.fs (G_node.get_fs g_node)

  (* TODO remove: no more position in node features. *)
  (* let match_ ?lexicons p_node g_node =
    match G_node.get_position g_node with
    | None -> P_fs.match_ ?lexicons p_node.fs (G_node.get_fs g_node)
    | Some p ->
      if P_fs.check_position (Some p) p_node.fs
      then P_fs.match_ ?lexicons p_node.fs (G_node.get_fs g_node)
      else raise P_fs.Fail *)

  let compare_pos t1 t2 = Stdlib.compare t1.loc t2.loc
end (* module P_node *)
