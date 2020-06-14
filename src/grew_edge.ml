(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Log
open Printf
open Conllx

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain


(* ================================================================================ *)
module G_edge = struct
  let current_config = ref Conllx_config.default

  let update_config name =
    try current_config := Conllx_config.build name with
      Conllx_error msg -> Error.run "%s" (Yojson.Basic.pretty_to_string msg)

  (* [G_edge.fs] is a feature structure. The list of feature names must be ordered wrt [Stdlib.compare] *)
  type fs = (feature_name * feature_value) list

  let fs_from_items l = (List.sort (fun (x,_) (y,_) -> Stdlib.compare x y) l)

  let fs_to_conllx fs = `Assoc (List.map (fun (k,v) -> (k, `String (string_of_value v))) fs)

  let fs_of_conllx json =
    let open Yojson.Basic.Util in
    json |> to_assoc |> List.map (fun (f,json_v) -> (f, typed_vos f (to_string json_v))) |> fs_from_items

  let fs_to_string_res ?(config = !current_config) fs = fs |> fs_to_conllx |> Conllx_label.of_json |> Conllx_label.to_string ~config

  let fs_to_string ?config fs = match fs_to_string_res ?config fs with Ok s | Error s -> s

  let fs_from_string s = s |> (Conllx_label.of_string ~config:!current_config) |> Conllx_label.to_json |> fs_of_conllx

  type t =
    | Fs of fs
    | Sub
    | Pred
    | Succ

  let empty = Fs []
  let sub = Sub
  let pred = Pred
  let succ = Succ

  let ordering = function
    | Pred | Succ -> true
    | _ -> false

  (* hardcoded edge feature value name "enhanced" *)
  let enhanced = function
    | Fs fs when List.assoc_opt "enhanced" fs = Some (String "yes") -> true
    | _ -> false

  let from_items l = Fs (fs_from_items l)

  let from_string s = Fs (fs_from_string s)

  let get_sub_opt feat_name = function
    | Fs fs -> List_.sort_assoc_opt feat_name fs
    | _ -> Error.run "[get_sub_opt] edge is not fs"

  let rec update feat_name new_value = function
    | Fs fs -> Fs (List_.sort_update_assoc feat_name new_value fs)
    | _ -> Error.run "[update] edge is not fs"

  let remove_feat_opt feat_name = function
    | Fs fs -> (match List_.sort_remove_assoc_opt feat_name fs with None -> None | Some fs -> Some (Fs fs))
    | _ -> Error.run "[remove_feat_opt] edge is not fs"

  let to_string_opt = function
    | Fs fs -> Some (fs_to_string fs)
    | _ -> None

  let to_short_opt = function
    | Fs fs -> (match fs_to_string_res fs with Ok s -> Some s | _ -> None)
    | _ -> None

  let dump = function
    | Fs fs -> fs_to_string fs
    | Sub -> "__SUB__"
    | Pred -> "__PRED__"
    | Succ -> "__SUCC__"

  let to_conllx_opt = function
    | Fs fs -> Some (fs_to_conllx fs)
    | _ -> None

  (* WARNING: hardcoded version which subsumes know configs *)
  let to_dep_opt ?domain ?(deco=false) ?config = function
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
            | Some (String "MWE") -> ["color=#1d7df2"; "forecolor=#1d7df2"; "bottom"]
            | Some (String "NE") -> ["color=#ff760b"; "forecolor=#ff760b"; "bottom"]
            | _ -> [] in
      let styles = if deco then "bgcolor=#8bf56e" :: styles else styles in
      Some (sprintf "{ label = \"%s\"; %s }" (fs_to_string ?config fs) (String.concat ";" styles))
    | _ -> None

  (* WARNING: hardcoded version which subsumes know configs *)
  let to_dot_opt ?domain ?(deco=false) = function
    | Fs fs ->
      let dot_items =
        match List_.sort_assoc_opt "kind" fs with
        | Some (String "deep") -> ["color=blue"; "fontcolor=blue"]
        | Some (String "surf") -> ["color=red"; "fontcolor=red"]
        | _ ->
          match List_.sort_assoc_opt "enhanced" fs with
          | Some (String "yes") -> ["color=blue"; "fontcolor=blue"]
          | _ ->
            match List_.sort_assoc_opt "parseme" fs with
            | Some (String "MWE") -> ["color=#1d7df2"; "fontcolor=#1d7df2"]
            | Some (String "NE") -> ["color=#ff760b"; "fontcolor=#ff760b"]
            | _ -> [] in
      let label = match deco with
        | true -> sprintf "<<TABLE BORDER=\"0\" CELLBORDER=\"0\"> <TR> <TD BGCOLOR=\"#8bf56e\">%s</TD> </TR> </TABLE>>" (fs_to_string fs)
        | false -> sprintf "\"%s\"" (fs_to_string fs) in
      Some (sprintf "[label=%s, %s]" label (String.concat ", " dot_items))
    | _ -> None

  let to_json t = match t with
    | Fs fs -> fs_to_conllx fs
    | _ -> `Null

  let build (ast_edge, loc) =
    match ast_edge.Ast.edge_label_cst with
    | Ast.Pos_list [one] -> from_string one
    | Ast.Atom_list list ->
      let unordered_fs =
        List.map
          (function Ast.Atom_eq (x,[y]) -> (x,typed_vos x y) | _ -> Error.build "[G_edge.build] cannot interpret Atom_list")
          list in
      Fs (fs_from_items unordered_fs)
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
    | Eq of (feature_name * feature_value list)
    (* 1<>subj|obj   2=*  *)
    | Diseq of (feature_name * feature_value list)
    (* !3 *)
    | Absent of feature_name

  type t =
    (* [comp:obj|comp@pass] *)
    | Pos of G_edge.fs list
    (* [^comp:obj|comp@pass] *)
    | Neg of G_edge.fs list
    (* [RE"aux.*"]  compiled and string version *)
    | Regexp of (Str.regexp * string)
    (* [1=subj, 2=*, !3] *)
    | Atom_list of atom_cst list
    | Pred
    | Succ

  let to_string ?domain = function
    | Pos fs_list -> (List_.to_string G_edge.fs_to_string "|" fs_list)
    | Neg fs_list -> "^"^(List_.to_string G_edge.fs_to_string "|" fs_list)
    | Regexp (_,re) -> "re\""^re^"\""
    | Atom_list l ->
      String.concat ","
        (List.map
           (function
             | Eq (name,al) -> sprintf "%s=%s" name (String.concat "|" (List.map string_of_value al))
             | Diseq (name,al) -> sprintf "%s<>%s" name (String.concat "|" (List.map string_of_value al))
             | Absent name -> sprintf "!%s" name
           ) l
        )
    | Pred -> "__PRED__"
    | Succ -> "__SUCC__"

  let to_json ?domain = function
    | Pos l -> `Assoc
                 ["pos",
                  `List (List.map (fun lab -> `String (G_edge.fs_to_string lab)) l)
                 ]
    | Neg l -> `Assoc
                 ["neg",
                  `List (List.map (fun lab -> `String (G_edge.fs_to_string lab)) l)
                 ]
    | Regexp (_,re) -> `Assoc
                         ["regexp", `String re]
    | _ -> failwith "TODO json"

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

  let match_ ?domain cst g_edge = match (cst,g_edge) with
    | (Succ, G_edge.Succ) -> true
    | (Pred, G_edge.Pred) -> true
    | (Pos fs_list, G_edge.Fs g_fs) -> List.exists (fun p_fs -> p_fs = g_fs) fs_list
    | (Neg fs_list, G_edge.Fs g_fs) -> not (List.exists (fun p_fs -> p_fs = g_fs) fs_list)
    | (Atom_list l, G_edge.Fs fs) -> List.for_all (match_atom fs) l
    | (Regexp (re,_), g_edge)  ->
      begin
        match G_edge.to_short_opt g_edge with
        | Some s -> String_.re_match re s
        | None -> false
      end
    | _ -> false

  let build_atom = function
    | Ast.Atom_eq (name, atoms) -> Eq (name, List.map (typed_vos name) (List.sort Stdlib.compare atoms))
    | Ast.Atom_diseq (name, atoms) -> Diseq (name, List.map (typed_vos name) (List.sort Stdlib.compare atoms))
    | Ast.Atom_absent name -> Absent name

  let build ?loc ?domain = function
    | Ast.Neg_list p_labels -> Neg (List.sort compare (List.map G_edge.fs_from_string p_labels))
    | Ast.Pos_list p_labels -> Pos (List.sort compare (List.map G_edge.fs_from_string p_labels))
    | Ast.Regexp re -> Regexp (Str.regexp re, re)
    | Ast.Atom_list l -> Atom_list (List.map build_atom l)
    | Ast.Pred -> Error.bug "[Label_cst.build]"
end (* module Label_cst *)

(* ================================================================================ *)
module P_edge = struct
  type t = {
    id: string option; (* an identifier for naming under_label in patterns *)
    label_cst: Label_cst.t;
  }

  let pred = { id=None; label_cst = Label_cst.Pred}
  let succ = { id=None; label_cst = Label_cst.Succ}
  let cpt = ref 0
  let fresh_name () = incr cpt; Some (sprintf "__e_%d__" !cpt)

  let all = {id=fresh_name (); label_cst=Label_cst.all }

  let get_id_opt t = t.id

  let to_json ?domain t =
    `Assoc (CCList.filter_map CCFun.id
              [
                (match t.id with Some id -> Some ("edge_id", `String id) | None -> None);
                Some ("label_cst", Label_cst.to_json ?domain t.label_cst)
              ])

  let build ?domain (ast_edge, loc) =
    { id = (match ast_edge.Ast.edge_id with Some s -> Some s | None -> fresh_name ());
      label_cst = Label_cst.build ~loc ?domain ast_edge.Ast.edge_label_cst
    }

  let to_string ?domain t =
    let label = Label_cst.to_string ?domain t.label_cst in
    match t.id with
    | None -> label
    | Some id when String.length id > 1 && id.[0] = '_' && id.[1] = '_' -> label
    | Some id -> sprintf "%s:%s" id label

  type edge_matcher =
    | Fail
    | Pass
    | Binds of string * G_edge.t list

  let match_ ?domain p_edge g_edge =
    if Label_cst.match_ ?domain p_edge.label_cst g_edge
    then (match p_edge.id with None -> Pass | Some l -> Binds (l, [g_edge]))
    else Fail

  let match_list ?domain p_edge g_edge_list =
    match List.filter (fun g_edge -> Label_cst.match_ ?domain p_edge.label_cst g_edge) g_edge_list with
    | [] -> Fail
    | list -> (match p_edge.id with None -> Pass | Some l -> Binds (l, list))
end (* module P_edge *)
