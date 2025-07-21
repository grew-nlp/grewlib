(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Conll

open Grew_utils
open Grew_ast

(* ================================================================================ *)
module G_edge_fs = struct
  (* [G_edge_fs.t] is a feature structure. The list of feature names must be ordered wrt [Stdlib.compare] *)
  type t = (string * Feature_value.t) list

  let build l = (List.sort (fun (x,_) (y,_) -> Stdlib.compare x y) l)

  let to_json = function
    | [("1",s)] -> Feature_value.to_json s
    | fs -> `Assoc (List.map (fun (k,v) -> (k, Feature_value.to_json v)) fs)

  (* returns either [Ok compact_string] or [Error long_string] if no compact representation can be built *)
  let to_string_result ~config fs = fs |> to_json |> Conll_label.of_json |> Conll_label.to_string ~config

  let to_string ~config fs = match to_string_result ~config fs with Ok s | Error s -> s

  let from_string ~config s =
    let open Yojson.Basic.Util in
    s
    |> (Conll_label.of_string ~config)
    |> Conll_label.to_json
    |> to_assoc
    |> List.map
        (fun (f,json_v) ->
          (f, Feature_value.parse f (to_string json_v))
        )
    |> build

  end (* module G_edge_fs *)

  (* ================================================================================ *)
module G_edge = struct
  type t =
    | Fs of G_edge_fs.t
    | Sub  (* dedicated value for subconstituent *)
    | Pred (* dedicated value for precedence (explicit encoding of linear order) *)
    | Succ (* dedicated value for successor (explicit encoding of linear order) *)

  let empty = Fs []
  let sub = Sub
  let pred = Pred
  let succ = Succ

  let is_real_link = function
    | Fs _ | Sub -> true
    | _ -> false

  let is_basic = function
    | Fs fs when not (List.assoc_opt "enhanced" fs = Some (String "yes")) -> true
    | _ -> false

  let from_items l = Fs (G_edge_fs.build l)

  let from_string ~config s = Fs (G_edge_fs.from_string ~config s)

  let get_sub_opt feat_name = function
    | Fs fs -> List_.sort_assoc_opt feat_name fs
    | _ -> Error.run "[get_sub_opt] edge is not fs"

  let update feat_name new_value = function
    | Fs fs -> Fs (List_.sort_update_assoc feat_name new_value fs)
    | _ -> Error.run "[update] edge is not fs"

  let remove_feat_opt feat_name = function
    | Fs fs -> (match List_.sort_remove_assoc_opt feat_name fs with None -> None | Some fs -> Some (Fs fs))
    | _ -> Error.run "[remove_feat_opt] edge is not fs"

  let to_string_opt ~config = function
    | Fs fs -> Some (G_edge_fs.to_string ~config fs)
    | _ -> None

  let to_compact_opt ~config = function
    | Fs fs -> (match G_edge_fs.to_string_result ~config fs with Ok s -> Some s | _ -> None)
    | _ -> None

  let to_string ?config edge =
    match (edge, config) with
    | (Fs fs, Some config) -> G_edge_fs.to_string ~config fs
    | (Fs fs, None) -> String.concat "," (List.map (fun (f,v) -> sprintf "%s=%s" f (Feature_value.to_string v)) fs)
    | (Sub, _) -> "__SUB__"
    | (Pred, _) -> "__PRED__"
    | (Succ, _) -> "__SUCC__"

  let to_json_opt = function
    | Fs fs -> Some (G_edge_fs.to_json fs)
    | _ -> None

  (* WARNING: hardcoded version which subsumes known configs *)
  let to_dep_opt ?(deco=false) ~config = function
    | Fs fs ->
      let styles =
        match List_.sort_assoc_opt "kind" fs with
        | Some (String "deep") -> ["color=blue"; "forecolor=blue"; "bottom"]
        | Some (String "surf") -> ["color=red"; "forecolor=red"]
        | _ ->
        match List_.sort_assoc_opt "enhanced" fs with
        | Some (String "yes") -> ["color=blue"; "forecolor=blue"; "bottom"]
        | _ ->
        match List_.sort_assoc_opt "parseme" fs with
        | Some (String "MWE") -> ["color=#ffa000"; "forecolor=#ffa000"; "bottom"]
        | Some (String "NE") -> ["color=#9900FF"; "forecolor=#9900FF"; "bottom"]
        | _ ->
        match List_.sort_assoc_opt "Cxn" fs with
        | Some (String _) -> ["color=#12cd56"; "forecolor=#12cd56"; "bottom"]
        | _ ->
        match List_.sort_assoc_opt "frsemcor" fs with
        | Some (String _) -> ["color=#12cd56"; "forecolor=#12cd56"; "bottom"]
        | _ ->
        match List_.sort_assoc_opt "span" fs with
        | Some (String _) -> ["color=pink"; "forecolor=pink"; "bottom"]
        | _ ->
        match List_.sort_assoc_opt "Syl" fs with
        | Some (String _) -> ["color=blue"; "forecolor=blue"; "bottom"]
        | _ ->
        match List_.sort_assoc_opt "ExternalOnset" fs with
        | Some (String _) -> ["color=#f57f17"; "forecolor=#f57f17"; "bottom"]
        | _ ->
        match List_.sort_assoc_opt "1" fs with
        | Some (String "RSTR") -> ["bottom"]
        | _ ->
        match List_.sort_assoc_opt "type" fs with
        | Some (String "attach") -> ["color=#12cd56"; "forecolor=#12cd56"; "bottom"]
        (* default *)
        | _ -> [] in
      let styles = if deco then "bgcolor=#8bf56e" :: styles else styles in
      Some (sprintf "{ label = \"%s\"; %s }" (G_edge_fs.to_string ~config fs) (String.concat ";" styles))
    | _ -> None

  (* WARNING: hardcoded version which subsumes known configs *)
  let to_dot_opt ?(deco=false) ~config = function
    | Fs fs ->
      let dot_items =
        match List_.sort_assoc_opt "main_out" fs with
        | Some (String "Yes") -> ["color=red"; "fontcolor=red"]
        | _ -> 
        match List_.sort_assoc_opt "kind" fs with
        | Some (String "deep") -> ["color=blue"; "fontcolor=blue"]
        | Some (String "surf") -> ["color=red"; "fontcolor=red"]
        | _ ->
          match List_.sort_assoc_opt "enhanced" fs with
          | Some (String "yes") -> ["color=blue"; "fontcolor=blue"]
          | _ ->
            match List_.sort_assoc_opt "parseme" fs with
            | Some (String "MWE") -> ["color=#ffa000"; "fontcolor=#ffa000"]
            | Some (String "NE") -> ["color=#9900FF"; "fontcolor=#9900FF"]
            | _ ->
              match List_.sort_assoc_opt "1" fs with
              | Some (String "unscoped") | Some (String "wider") | Some (String "equal") | Some (String "dual") ->
              ["color=\"red\""; "fontcolor=\"red\""]
              | Some (String "in") -> ["style=\"dotted\""] (* PMB link from Box-nodes to Sem-nodes *)
              | _ -> [] in
      let multi_line_label = Str.global_replace (Str.regexp_string ",") "\n" (G_edge_fs.to_string ~config fs) in
      let label =
        match deco with
        | true -> sprintf "<<TABLE BORDER=\"0\" CELLBORDER=\"0\"> <TR> <TD BGCOLOR=\"#8bf56e\">%s</TD> </TR> </TABLE>>" multi_line_label
        | false -> sprintf "\"%s\"" multi_line_label in
      Some (sprintf "[label=%s, %s]" label (String.concat ", " dot_items))
    | _ -> None

  let to_json = function
    | Fs fs -> G_edge_fs.to_json fs
    | _ -> `Null

  let build ~config (ast_edge, loc) =
    match ast_edge.Ast.edge_label_cst with
    | Ast.Pos_list [one] -> from_string ~config one
    | Ast.Atom_list list ->
      let unordered_fs =
        List.map
          (function Ast.Atom_eq (x,[y]) -> (x,Feature_value.parse ~loc x y) | _ -> Error.build ~loc "[G_edge.build] cannot interpret Atom_list")
          list in
      Fs (G_edge_fs.build unordered_fs)
    | Ast.Neg_list _ -> Error.build ~loc "Negative edge spec are forbidden in graphs"
    | Ast.Pos_list _ -> Error.build ~loc "Only atomic edge values are allowed in graphs"
    | Ast.Regexp _ -> Error.build ~loc "Regexp are not allowed in graphs"
    | Ast.Pred -> Error.build ~loc "Pred in graphs"
end (* module G_edge *)


(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst = struct
  type atom_cst =
    (* 1=subj|obj *)
    | Eq of (string * Feature_value.t list)
    (* 1<>subj|obj   2=*  *)
    | Diseq of (string * Feature_value.t list)
    (* !3 *)
    | Absent of string

  type t =
    (* [comp:obj|comp@pass] *)
    | Pos of G_edge_fs.t list
    (* [^comp:obj|comp@pass] *)
    | Neg of G_edge_fs.t list
    (* [RE"aux.*"]  compiled and string version *)
    | Regexp of Regexp.t
    (* [1=subj, 2=*, !3] *)
    | Atom_list of atom_cst list
    | Pred
    | Succ

  let to_string ~config = function
    | Pos fs_list -> (String.concat "|" (List.map (G_edge_fs.to_string ~config) fs_list))
    | Neg fs_list -> "^"^(String.concat "|" (List.map (G_edge_fs.to_string ~config) fs_list))
    | Regexp re -> Regexp.to_string re
    | Atom_list l ->
      String.concat ","
        (List.map
           (function
             | Eq (name,al) -> sprintf "%s=%s" name (String.concat "|" (List.map Feature_value.to_string al))
             | Diseq (name,al) -> sprintf "%s<>%s" name (String.concat "|" (List.map Feature_value.to_string al))
             | Absent name -> sprintf "!%s" name
           ) l
        )
    | Pred -> "__PRED__"
    | Succ -> "__SUCC__"

  let all = Neg []

  let match_atom fs = function
    | Eq (name, l) ->
      begin
        match List_.sort_assoc_opt name fs with
        | None -> false
        | Some v -> List_.sort_mem v l
      end
    | Diseq (name, l) ->
      begin
        match List_.sort_assoc_opt name fs with
        | None -> false
        | Some v -> not (List_.sort_mem v l)
      end
    | Absent name -> not (List_.sort_mem_assoc name fs)

  let match_ ~config cst g_edge =
    match (cst,g_edge) with
    | (Succ, G_edge.Succ) -> true
    | (Pred, G_edge.Pred) -> true
    | (Pos fs_list, G_edge.Fs g_fs) -> List.exists (fun p_fs -> p_fs = g_fs) fs_list
    | (Neg fs_list, G_edge.Fs g_fs) -> not (List.exists (fun p_fs -> p_fs = g_fs) fs_list)
    | (Atom_list l, G_edge.Fs fs) -> List.for_all (match_atom fs) l
    | (Regexp re, g_edge)  ->
      begin
        match G_edge.to_compact_opt ~config g_edge with
        | Some s -> Regexp.re_match re s
        | None -> false
      end
    | _ -> false

  let build_atom ?loc = function
    | Ast.Atom_eq (name, atoms) -> Eq (name, List.map (Feature_value.parse ?loc name) (List.sort Stdlib.compare atoms))
    | Ast.Atom_diseq (name, atoms) -> Diseq (name, List.map (Feature_value.parse ?loc name) (List.sort Stdlib.compare atoms))
    | Ast.Atom_absent name -> Absent name

  let of_ast ?loc ~config = function
    | Ast.Neg_list p_labels -> Neg (List.sort compare (List.map (G_edge_fs.from_string ~config) p_labels))
    | Ast.Pos_list p_labels -> Pos (List.sort compare (List.map (G_edge_fs.from_string ~config) p_labels))
    | Ast.Regexp re -> Regexp re
    | Ast.Atom_list l -> Atom_list (List.map (build_atom ?loc) l)
    | Ast.Pred -> Error.bug "[Label_cst.of_ast]"
end (* module Label_cst *)

(* ================================================================================ *)
module P_edge = struct
  type t = {
    id: string option; (* an identifier for naming under_label in requests *)
    label_cst: Label_cst.t;
  }

  let pred = { id=None; label_cst = Label_cst.Pred}
  let succ = { id=None; label_cst = Label_cst.Succ}

  let get_id_opt t = t.id

  let of_ast ~config (ast_edge, loc) =
    { id = ast_edge.Ast.edge_id;
      label_cst = Label_cst.of_ast ~loc ~config ast_edge.Ast.edge_label_cst
    }

  let to_string ~config t =
    let label = Label_cst.to_string ~config t.label_cst in
    match t.id with
    | None -> label
    | Some id -> sprintf "%s:%s" id label

  let to_id_opt_and_string ~config t =
    let label = Label_cst.to_string ~config t.label_cst in
    match t.id with
    | None -> (None, label)
    | Some id -> (Some id, label)

  type edge_matcher =
    | Fail
    | Pass
    | Binds of string * G_edge.t list

  let match_ ~config p_edge g_edge =
    if Label_cst.match_ ~config p_edge.label_cst g_edge
    then (match p_edge.id with None -> Pass | Some l -> Binds (l, [g_edge]))
    else Fail

  let match_list ~config p_edge g_edge_list =
    match List.filter (fun g_edge -> Label_cst.match_ ~config p_edge.label_cst g_edge) g_edge_list with
    | [] -> Fail
    | list -> (match p_edge.id with None -> Pass | Some l -> Binds (l, list))
end (* module P_edge *)
