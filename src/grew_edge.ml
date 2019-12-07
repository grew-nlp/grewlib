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
  type t = (string * string) list

  let from_items l = List.sort (fun (x,_) (y,_) -> Stdlib.compare x y) l

  let get_sub = List_.sort_assoc

  exception Not_conll of string

  let to_string_long edge = String.concat "," (List.map (fun (x,y) -> x^"="^y) edge)

  let rec update feat_name new_value = function
    | [] -> [(feat_name, new_value)]
    | (fn,fv)::t when feat_name < fn -> (feat_name, new_value) :: (fn,fv) :: t
    | (fn,fv)::t when feat_name = fn -> (feat_name, new_value) :: t
    | x::t -> x :: (update feat_name new_value t)

  let rec remove feat_name = function
    | [] -> None
    | (fn,fv)::t when feat_name < fn -> None
    | (fn,fv)::t when feat_name = fn -> Some t
    | x::t -> (match remove feat_name t with Some new_t -> Some (x::new_t) | None -> None)

  let to_conll ?domain edge =
    let prefix = match get_sub "kind" edge with
      | None -> ""
      | Some "surf" -> "S:"
      | Some "deep" -> "D:"
      | Some "enhanced" -> "E:"
      | Some c -> raise (Not_conll (sprintf "unknown kind '%s' in edge '%s'" c (to_string_long edge))) in
    let suffix = match get_sub "deep" edge with
      | Some s -> "@"^s
      | None -> "" in
    let infix_items = edge
                      |> (List_.sort_remove_assoc "kind")
                      |> (List_.sort_remove_assoc "deep") in
    let core = CCList.mapi
        (fun i (n,v) ->
           if string_of_int(i+1) = n
           then v
           else raise (Not_conll (to_string_long edge))
        ) infix_items in
    prefix ^ (String.concat ":" core) ^ suffix

  let to_string ?domain edge =
    try to_conll ?domain edge
    with Not_conll s -> sprintf "[%s]" s

  let to_dep ?domain ?(deco=false) t =
    let conll = to_conll t in
    Domain.label_to_dep ?domain ~deco conll

  let to_dot ?domain ?(deco=false) t =
    let conll = to_conll t in
    Domain.label_to_dot ?domain ~deco conll

  let split l = CCList.mapi
      (fun i elt -> (string_of_int (i+1), elt)) l

  let from_string ?loc ?domain str =
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
    List.sort (Stdlib.compare) (CCList.cons_maybe deep before_deep)

  let to_json ?domain t = `String (to_string ?domain t)

  let sub = from_string "__SUB__"

  let build ?domain (ast_edge, loc) =
    match ast_edge.Ast.edge_label_cst with
    | Ast.Pos_list [one] -> from_string ~loc ?domain one
    | Ast.Neg_list _ -> Error.build ~loc "Negative edge spec are forbidden in graphs"
    | Ast.Pos_list _ -> Error.build ~loc "Only atomic edge values are allowed in graphs"
    | Ast.Regexp _ -> Error.build ~loc "Regexp are not allowed in graphs"
    | Ast.Atom_list _ -> Error.build ~loc "Non atomic edge are not allowed in graphs"

end (* module G_edge *)


(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst = struct
  type atom_cst =
    | Eq of (string * string list)
    | Diseq of (string * string list)
    | Absent of string

  type t =
    | Pos of G_edge.t list
    | Neg of G_edge.t list
    | Regexp of (Str.regexp * string)
    | Atom_list of atom_cst list

  let to_string ?domain = function
    | Pos l -> (List_.to_string (G_edge.to_string ?domain) "|" l)
    | Neg l -> "^"^(List_.to_string (G_edge.to_string ?domain) "|" l)
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

  let to_json ?domain = function
    | Pos l -> `Assoc
                 ["pos",
                  `List (List.map (fun lab -> `String (G_edge.to_string ?domain lab)) l)
                 ]
    | Neg l -> `Assoc
                 ["neg",
                  `List (List.map (fun lab -> `String (G_edge.to_string ?domain lab)) l)
                 ]
    | Regexp (_,re) -> `Assoc
                         ["regexp", `String re]
    | Atom_list l -> failwith "TODO json"

  let all = Neg []

  let match_atom g_label = function
    | Eq (name, l) ->
      begin
        match List_.sort_assoc name g_label with
        | None -> false
        | Some v -> List_.sort_mem v l
      end
    | Diseq (name, l) ->
      begin
        match List_.sort_assoc name g_label with
        | None -> false
        | Some v -> not (List_.sort_mem v l)
      end
    | Absent name -> not (List_.sort_mem_assoc name g_label)

  let match_ ?domain cst g_label = match cst with
    | Pos labels -> List.exists (fun p_label -> p_label = g_label) labels
    | Neg labels -> not (List.exists (fun p_label -> p_label = g_label) labels)
    | Atom_list l -> List.for_all (match_atom g_label) l
    | Regexp (re,_) ->
      try String_.re_match re (G_edge.to_conll ?domain g_label)
      with G_edge.Not_conll s ->
        Error.run "Cannot ckeck for regexp constraint on the edge \"%s\"" s

  let build_atom = function
    | Ast.Atom_eq (name, atoms) -> Eq (name, List.sort Stdlib.compare atoms)
    | Ast.Atom_diseq (name, atoms) -> Diseq (name, List.sort Stdlib.compare atoms)
    | Ast.Atom_absent name -> Absent name

  let build ?loc ?domain = function
    | Ast.Neg_list p_labels -> Neg (List.sort compare (List.map (G_edge.from_string ?loc ?domain) p_labels))
    | Ast.Pos_list p_labels -> Pos (List.sort compare (List.map (G_edge.from_string ?loc ?domain) p_labels))
    | Ast.Regexp re -> Regexp (Str.regexp re, re)
    | Ast.Atom_list l -> Atom_list (List.map build_atom l)
end (* module Label_cst *)

(* ================================================================================ *)
module P_edge = struct
  type t = {
    id: string; (* an identifier for naming under_label in patterns *)
    label_cst: Label_cst.t;
  }

  let cpt = ref 0
  let fresh_name () = incr cpt; sprintf "__e_%d__" !cpt

  let all = {id=fresh_name (); label_cst=Label_cst.all }

  let get_id t = t.id

  let to_json ?domain t =
    `Assoc [
      ("edge_id", `String t.id);
      ("label_cst", Label_cst.to_json ?domain t.label_cst)
    ]

  let build ?domain (ast_edge, loc) =
    { id = (match ast_edge.Ast.edge_id with Some s -> s | None -> fresh_name ());
      label_cst = Label_cst.build ~loc ?domain ast_edge.Ast.edge_label_cst
    }

  let to_string ?domain t =
    if String.length t.id > 1 && t.id.[0] = '_' && t.id.[1] = '_'
    then Label_cst.to_string ?domain t.label_cst
    else sprintf "%s:%s" t.id (Label_cst.to_string ?domain t.label_cst)

  type edge_matcher =
    | Fail
    | Binds of string * G_edge.t list

  let match_ ?domain p_edge g_edge =
    match p_edge with
    | {id; label_cst } when Label_cst.match_ ?domain label_cst g_edge -> Binds (id, [g_edge])
    | _ -> Fail

  let match_list ?domain p_edge g_edge_list =
    match p_edge with
    | {id; label_cst } ->
      ( match List.filter (fun g_edge -> Label_cst.match_ ?domain label_cst g_edge) g_edge_list with
        | [] -> Fail
        | list -> Binds (id, list)
      )
end (* module P_edge *)
