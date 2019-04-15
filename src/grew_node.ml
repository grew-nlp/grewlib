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
  type kind = No | Conll_root | Skeleton

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
      kind: kind;
      efs: (string * string) list;
    }

  let shift user_id n t =
    let sh i = i + n in
    { t with
    name = CCOpt.map (fun n -> user_id ^ "_" ^ n) t.name;
    next = Massoc_gid.map_key sh t.next;
    prec = CCOpt.map sh t.prec;
    succ = CCOpt.map sh t.succ;
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

  let empty = { name=None; fs = G_fs.empty; next = Massoc_gid.empty; succ = None; prec = None; position = Unordered 0; kind=No; efs=[] }

  let is_conll_root t = t.kind = Conll_root
  let is_skeleton t = t.kind = Skeleton

  let to_string ?domain t =
    Printf.sprintf "  fs=[%s]\n  next=%s\n"
      (G_fs.to_string t.fs)
      (Massoc_gid.to_string (G_edge.to_string ?domain) t.next)

  let to_gr t = sprintf "[%s] " (G_fs.to_gr t.fs)

  let add_edge g_edge gid_tar t =
    match Massoc_gid.add_opt gid_tar g_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None

  let get_efs n = n.efs

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
    then { empty with
      kind = Conll_root;
      succ;
      position = Ordered 0.;
      name= Some "ROOT";
    }
    else { empty with
      fs = G_fs.of_conll ?loc ?domain line;
      position = Ordered (float_of_conll_id line.Conll.id);
      prec;
      succ;
      efs=line.Conll.efs;
      name = Some (Conll_types.Id.to_string line.Conll.id)
    }

  let pst_leaf ?loc ?domain phon position =
    { empty with fs = G_fs.pst_leaf ?loc ?domain phon; position = Ordered (float position) }
  let pst_node ?loc ?domain cat position =
    { empty with fs = G_fs.pst_node ?loc ?domain cat; position = Ordered (float position) } (* TODO : change to Unordered *)

  let fresh ?prec ?succ pos = { empty with position = Ordered pos; prec; succ; name=Some (Printf.sprintf "W%g" pos) }
  let fresh_unordered () = { empty with position = Unordered (fresh_index ())}

  let remove_opt (id_tar : Gid.t) label t =
    match Massoc_gid.remove_opt id_tar label t.next with
    | Some new_next -> Some {t with next = new_next}
    | None -> None

  let remove_key node_id t =
    try {t with next = Massoc_gid.remove_key node_id t.next} with Not_found -> t

  let rm_out_edges t = {t with next = Massoc_gid.empty}

  (* let build_neighbour t = { empty with position = (get_position t) +. 0.01 }

  let build_new t = { empty with position = (get_position t) +. 0.01 } *)

  let position_comp n1 n2 = Pervasives.compare n1.position n2.position

  let rename mapping n = {n with next = Massoc_gid.rename mapping n.next}

  let skeleton n = { n with name = None; fs=G_fs.empty; next=Massoc_gid.empty; kind = Skeleton; efs=[]; }

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

  let build ?domain lexicons (ast_node, loc) =
    (ast_node.Ast.node_id,
     {
       name = ast_node.Ast.node_id;
       fs = P_fs.build ?domain lexicons ast_node.Ast.fs;
       next = Massoc_pid.empty;
       loc = Some loc;
     } )

  let add_edge p_edge pid_tar t =
    match Massoc_pid.add_opt pid_tar p_edge t.next with
    | Some l -> Some {t with next = l}
    | None -> None


  let match_ ?lexicons p_node g_node =
    match G_node.get_position g_node with
    | G_node.Unordered _ -> P_fs.match_ ?lexicons p_node.fs (G_node.get_fs g_node)
    | G_node.Ordered p ->
      if P_fs.check_position (Some p) p_node.fs
      then P_fs.match_ ?lexicons p_node.fs (G_node.get_fs g_node)
      else raise P_fs.Fail

  let compare_pos t1 t2 = Pervasives.compare t1.loc t2.loc
end (* module P_node *)
