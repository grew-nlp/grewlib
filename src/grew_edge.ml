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

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain

(* ================================================================================ *)
module G_edge = struct

  (* [G_edge.fs] is a feature structure. The list of features must be ordered wrt [Stdlib.compare] *)
  type fs = (string * string) list

  let rec update_fs feat_name new_value = function
    | [] -> [(feat_name, new_value)]
    | ((fn,fv)::t) when feat_name < fn -> ((feat_name, new_value) :: (fn,fv) :: t)
    | ((fn,fv)::t) when feat_name = fn -> ((feat_name, new_value) :: t)
    | (x::t) -> (x :: (update_fs feat_name new_value t))

  exception Not_conll
  let fs_to_string fs = 
    let prefix = match List_.sort_assoc "kind" fs with
      | None -> ""
      | Some "surf" -> "S:"
      | Some "deep" -> "D:"
      | Some "enhanced" -> "E:"
      | Some c -> raise Not_conll in
    let suffix = match List_.sort_assoc "deep" fs with
      | Some s -> "@"^s
      | None -> "" in
    let infix_items =
      fs
      |> (List_.sort_remove_assoc "kind")
      |> (List_.sort_remove_assoc "deep") in
    let core = CCList.mapi
        (fun i (n,v) ->
           if string_of_int(i+1) = n
           then v
           else raise Not_conll
        ) infix_items in
    prefix ^ (String.concat ":" core) ^ suffix

  let fs_from_items l = (List.sort (fun (x,_) (y,_) -> Stdlib.compare x y) l)

  (* split ["a"; "b"; "c"] --> [("1","a"); ("2","b"); ("3","c")]  *)
  let split l = CCList.mapi
      (fun i elt -> (string_of_int (i+1), elt)) l

      let fs_from_string str =
    let (init, deep) = match Str.bounded_split (Str.regexp_string "@") str 2 with
      | [i] -> (i, None)
      | [i;d] -> (i, Some ("deep",d))
      | _ -> assert false in
    let before_deep =
      match Str.split (Str.regexp_string ":") init with
      | "S" :: l -> ("kind","surf") :: (split l)
      | "D" :: l -> ("kind","deep") :: (split l)
      | "E" :: l -> ("kind","enhanced") :: (split l)
      | l -> split l in
    fs_from_items (CCList.cons_maybe deep before_deep)


  type t = 
    | Fs of fs
    | Sub
    | Pred
    | Succ

  let from_items l = Fs (fs_from_items l)

  let rec update feat_name new_value = function
    | [] -> [(feat_name, new_value)]
    | (fn,fv)::t when feat_name < fn -> (feat_name, new_value) :: (fn,fv) :: t
    | (fn,fv)::t when feat_name = fn -> (feat_name, new_value) :: t
    | x::t -> x :: (update feat_name new_value t)

  let get_sub feat_name = function
    | Fs fs -> List_.sort_assoc feat_name fs
    | _ -> Error.run "[get_sub] edge is not fs"

  let rec update feat_name new_value = function
    | Fs fs -> Fs (update_fs feat_name new_value fs)
    | _ -> Error.run "[update] edge is not fs"

  let remove_feat_opt feat_name = function
    | Fs fs -> (match List_.sort_remove_assoc_opt feat_name fs with None -> None | Some fs -> Some (Fs fs))
    | _ -> Error.run "[remove_feat_opt] edge is not fs"

  let to_string_long = function
    | Fs fs -> String.concat "," (List.map (fun (x,y) -> x^"="^y) fs)
    | Pred -> "__PRED__"
    | Succ -> "__SUCC__"
    | Sub -> "__SUB__"

  let to_conll = function
    | Fs fs -> fs_to_string fs
    | Pred -> "__PRED__"
    | Succ -> "__SUCC__"
    | Sub -> "__SUB__"

  let to_string edge =
    try to_conll edge
    with Not_conll -> to_string_long edge

  let to_dep ?domain ?(deco=false) t =
    let conll = to_string t in
    Domain.label_to_dep ?domain ~deco conll

  let to_dot ?domain ?(deco=false) t =
    let conll = to_string t in
    Domain.label_to_dot ?domain ~deco conll


  let from_string s = Fs (fs_from_string s)

  let to_json t = `String (to_string t)

  let sub = Sub
  let pred = Pred
  let succ = Succ

  let build (ast_edge, loc) =
    match ast_edge.Ast.edge_label_cst with
    | Ast.Pos_list [one] -> from_string one
    | Ast.Neg_list _ -> Error.build ~loc "Negative edge spec are forbidden in graphs"
    | Ast.Pos_list _ -> Error.build ~loc "Only atomic edge values are allowed in graphs"
    | Ast.Regexp _ -> Error.build ~loc "Regexp are not allowed in graphs"
    | Ast.Atom_list _ -> Error.build ~loc "Non atomic edge are not allowed in graphs"
    | Ast.Pred -> Error.build ~loc "Pred in graphs"
end (* module G_edge *)


(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst = struct
  type atom_cst =
    | Eq of (string * string list)
    | Diseq of (string * string list)
    | Absent of string

  type t =
    | Pos of G_edge.fs list
    | Neg of G_edge.fs list
    | Regexp of (Str.regexp * string)
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
             | Eq (name,al) -> sprintf "%s=%s" name (String.concat "|" al)
             | Diseq (name,al) -> sprintf "%s<>%s" name (String.concat "|" al)
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
        match List_.sort_assoc name fs with
        | None -> false
        | Some v -> List_.sort_mem v l
      end
    | Diseq (name, l) ->
      begin
        match List_.sort_assoc name fs with
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
        try String_.re_match re (G_edge.to_conll g_edge)
        with G_edge.Not_conll -> Error.run "Cannot ckeck for regexp constraint againt a not Conll edge"
      end
    | _ -> false

  let build_atom = function
    | Ast.Atom_eq (name, atoms) -> Eq (name, List.sort Stdlib.compare atoms)
    | Ast.Atom_diseq (name, atoms) -> Diseq (name, List.sort Stdlib.compare atoms)
    | Ast.Atom_absent name -> Absent name

  let build ?loc ?domain = function
    | Ast.Neg_list p_labels -> Neg (List.sort compare (List.map G_edge.fs_from_string p_labels))
    | Ast.Pos_list p_labels -> Pos (List.sort compare (List.map G_edge.fs_from_string p_labels))
    | Ast.Regexp re -> Regexp (Str.regexp re, re)
    | Ast.Atom_list l -> Atom_list (List.map build_atom l)
    | Ast.Pred -> Error.bug "Pred in not a constraint"
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

  let get_id t = t.id

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
