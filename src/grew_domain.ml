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
open Log

open Grew_base
open Grew_types
open Grew_ast

(* ================================================================================ *)
module Label_domain = struct
  (** describe the display style of a label *)
  type line = Solid | Dot | Dash
  type style = {
    text: string;
    bottom: bool;
    color: string option;
    bgcolor: string option;
    line: line;
  }

  (** The [default] style value *)
  let default = { text="UNSET"; bottom=false; color=None; bgcolor=None; line=Solid }


  type t = style String_map.t

  let get_style t str =
    match String_map.find_opt str t with
    | Some v -> v
    | None -> {default with text = str}

  let dump label_domain =
  Printf.printf "========= Label domain =========\n";
  String_map.iter (fun label _ -> Printf.printf " - %s\n" label) label_domain;
  Printf.printf "==================================\n%!"

  let to_json label_domain = `List (String_map.fold (fun k _ acc -> (`String k) :: acc) label_domain [])


  (** [decl] is the type for a label declaration: the name and a list of display styles *)
  type decl = string * string list

  let merge l1 l2 =
    List.fold_left
    (fun acc (name, styles) ->
      if List.mem_assoc name acc
      then (Error.warning "duplicate label definition \"%s\"" name; acc)
      else (name, styles) :: acc
    ) l1 l2

  let no_domain_style string_label =
    match Str.bounded_split (Str.regexp ":") string_label 2 with
    | ["MWE"] -> {default with text="MWE"; color=Some "#1d7df2"; bottom=true;}
    | ["1"] -> {default with text="1"; color=Some "#1d7df2"; bottom=true; line=Dot; }
    | ["2"] -> {default with text="2"; color=Some "#1d7df2"; bottom=true; line=Dot; }
    | ["NE"] -> {default with text="NE"; color=Some "#ff760b"; bottom=true;}
    | ["S"; l] -> {default with text=l; color=Some "red"}
    | ["D"; l] | ["E"; l] -> {default with text=l; color=Some "blue"; bottom=true}
    | ["I"; l] -> {default with text=l; color=Some "grey"}
    | _ -> {default with text=string_label}

  (** Computes the style of a label from its options and maybe its shape (like I:...). *)
  let parse_option string_label options =
    let init_style = no_domain_style string_label in
      List.fold_left
        (fun acc_style -> function
            | "@bottom" -> {acc_style with bottom=true}
            | "@dash" -> {acc_style with line=Dash}
            | "@dot" -> {acc_style with line=Dot}
            | s when String.length s > 4 && String.sub s 0 4 = "@bg_" ->
              let color = String.sub s 4 ((String.length s) - 4) in
              {acc_style with bgcolor=Some color}
            | s -> {acc_style with color=Some (String_.rm_first_char s)}
        ) init_style options

  (* [build decl_list] returns a label_domain *)
  let build decl_list =
      List.fold_left
        (fun acc (label, opts) -> String_map.add label (parse_option label opts) acc
        ) String_map.empty decl_list

  let to_dep ?(deco=false) style =
    let dep_items =
      (if style.bottom then ["bottom"] else [])
      @ (match style.color with Some c -> ["color="^c; "forecolor="^c] | None -> [])
      @ (match style.bgcolor with Some c -> ["bgcolor="^c] | None -> [])
      @ (match style.line with
        | Dot -> ["style=dot"]
        | Dash -> ["style=dash"]
        | Solid when deco -> ["bgcolor=#8bf56e"]
        | Solid -> []) in
    sprintf "{ label = \"%s\"; %s}"
      style.text
      (String.concat "; " dep_items)

  let dot_color string =
    match (string.[0], String.length string) with
    | ('#', 4) -> sprintf "\"#%c%c%c%c%c%c\"" string.[1] string.[1] string.[2] string.[2] string.[3] string.[3]
    | ('#', 7) -> sprintf "\"%s\"" string
    | _ -> string

  let to_dot ?(deco=false) style =
    let dot_items =
      (match style.color with Some c -> let d = dot_color c in ["color="^d; "fontcolor="^d] | None -> [])
      @ (match style.line with
        | Dot -> ["style=dotted"]
        | Dash -> ["style=dashed"]
        | Solid -> []) in
      let label = match (deco,style.bgcolor) with
      | (true, _) -> sprintf "<<TABLE BORDER=\"0\" CELLBORDER=\"0\"> <TR> <TD BGCOLOR=\"#00FF00\">%s</TD> </TR> </TABLE>>" style.text
      | (false, Some c) -> sprintf "<<TABLE BORDER=\"0\" CELLBORDER=\"0\"> <TR> <TD BGCOLOR=\"%s\">%s</TD> </TR> </TABLE>>" c style.text
      | _ -> sprintf "\"%s\"" style.text in
      sprintf "[label=%s, %s]" label (String.concat ", " dot_items)

end

(* ================================================================================ *)
module Feature_domain = struct
  type t = {
    decls: Ast.feature_spec list;
  }


  let dump t =
    Printf.printf "========= Feature domain =========\n";
    List.iter (function
      | Ast.Closed (fn, values) -> Printf.printf " %s : %s\n" fn (String.concat ", " values)
      | Ast.Open fn -> Printf.printf " %s is OPEN\n" fn
      | Ast.Num fn -> Printf.printf " %s id NUMERICAL\n" fn
      ) t.decls;
    Printf.printf "==================================\n%!"

  let to_json t =
    `Assoc (
      List.map (function
        | Ast.Closed (fn, values) -> (fn, `List (List.map (fun x -> `String x) values))
        | Ast.Open fn -> (fn, `String "Open")
        | Ast.Num fn -> (fn, `String "Num")
        ) t.decls
      )

  let get_name = function
  | Ast.Closed (fn, _) -> fn
  | Ast.Open fn -> fn
  | Ast.Num fn -> fn

  let is_defined feature_name decls =
    List.exists (fun item -> get_name item = feature_name) decls

  let rec build_decls = function
    | [] -> [Ast.Num "position"]
    | (Ast.Num "position") :: tail -> Log.warning "[Feature_domain] declaration of the feature name \"position\" in useless"; build_decls tail
    | (Ast.Open "position") :: _
    | (Ast.Closed ("position",_)) :: _ ->
      Error.build "[Feature_domain] The feature named \"position\" is reserved and must be types 'integer', you cannot not redefine it"
    | (Ast.Num fn) :: tail | (Ast.Open fn) :: tail | Ast.Closed (fn,_) :: tail when is_defined fn tail ->
      begin
        if fn = "form" || fn = "upos" || fn = "xpos"
        then Error.build "[Feature_domain] The feature named \"%s\" is defined several times (see http://grew.fr/features/note-about-backward-compatibility for details)" fn
        else Error.build "[Feature_domain] The feature named \"%s\" is defined several times" fn

      end
    | x :: tail -> x :: (build_decls tail)

  let build feature_spec_list =
    {decls = build_decls feature_spec_list}

  let feature_names feature_domain =
    List.map (function Ast.Closed (fn, _) | Ast.Open fn | Ast.Num fn -> fn) feature_domain.decls

  let merge list1 list2 =
    List.fold_left
    (fun acc item ->
      match List.filter (fun i -> get_name i = get_name item) acc with
      | [] -> item :: acc
      | [one] ->
        let new_item = match (one, item) with
        | (Ast.Open _, Ast.Open _) | (Ast.Num _, Ast.Num _) -> item
        | (Ast.Open fn, Ast.Closed _) | (Ast.Closed _, Ast.Open fn) ->
          Error.warning "Feature name \"%s\" is declared twice as open and close; it is considered as open" fn; Ast.Open fn
        | (Ast.Closed (fn, l1), (Ast.Closed (_, l2))) -> Ast.Closed (fn , l1 @ l2)
        | _ -> Error.build "Cannot merge numerical ans non numerical features \"%s\"" (get_name item) in
        new_item :: acc
      | _ -> Error.bug "Duplicate in Feature_domain.merge"
       acc)
    list1 list2

  let is_num feature_domain feature_name =
    List.exists (function
      | Ast.Num fn when fn = feature_name -> true
      | _ -> false
    ) feature_domain.decls

  let is_open feature_domain name =
    List.exists (function Ast.Open n when n=name -> true | _ -> false) feature_domain.decls

  (* This function is defined here because it is used by check_feature *)
  let build_disj ?loc ?feature_domain name unsorted_values =
    let values = List.sort Pervasives.compare unsorted_values in
    match (feature_domain, name.[0]) with
      | (None, _)
      | (Some _, '_') -> List.map (fun s -> String s) values (* no check on feat_name starting with '_' *)
      | (Some {decls=dom}, _) ->
        let rec loop = function
          | [] -> Error.build ?loc "[GRS] Unknown feature name '%s'" name
          | ((Ast.Open n)::_) when n = name ->
            List.map (fun s -> String s) values
          | ((Ast.Num n)::_) when n = name ->
            (try List.map (fun s -> Float (String_.to_float s)) values
            with Failure _ -> Error.build ?loc "[GRS] The feature '%s' is of type int" name)
          | ((Ast.Closed (n,vs))::_) when n = name ->
            (match List_.sort_diff values vs with
              | [] -> List.map (fun s -> String s) values
              | l when List.for_all (fun x -> x.[0] = '_') l -> List.map (fun s -> String s) values
              | l -> Error.build ?loc "Unknown feature values '%s' for feature name '%s'"
                  (List_.to_string (fun x->x) ", " l)
          name
            )
          | _::t -> loop t in
        loop dom

  let check_feature ?loc ?feature_domain name value =
    ignore (build_disj ?loc ?feature_domain name [value])
end (* Feature_domain *)


(* ================================================================================ *)
module Domain = struct
  type t =
  | Both of Label_domain.t * Feature_domain.t
  | Label of Label_domain.t
  | Feature of Feature_domain.t

  let dump = function
  | None -> Printf.printf "=================== No domain ===================\n";
  | Some Both (ld,fd) -> Label_domain.dump ld; Feature_domain.dump fd
  | Some Label ld -> Label_domain.dump ld
  | Some Feature fd -> Feature_domain.dump fd

  let to_json = function
  | Both (ld, fd) -> `Assoc [("Label_domain", Label_domain.to_json ld); ("feature_domain", `Null);]
  | Label ld -> `Assoc [("Label_domain", Label_domain.to_json ld)]
  | Feature fd -> `Assoc [("feature_domain", `Null);]

  let build ld fd = Both (ld, fd)

  let build_features_only fd = Feature fd
  let build_labels_only ld = Label ld

  let build_disj ?loc ?domain name unsorted_values =
    match domain with
    | Some (Feature feature_domain) | Some (Both (_, feature_domain)) ->
      Feature_domain.build_disj ?loc ~feature_domain name unsorted_values
    | _ -> Feature_domain.build_disj ?loc name unsorted_values

  let feature_names = function
   | Feature feature_domain | Both (_, feature_domain) -> Feature_domain.feature_names feature_domain
   | _ -> []

  let get_label_style ?domain str = match domain with
  | Some (Both (label_domain,_)) | Some (Label label_domain) -> Some (Label_domain.get_style label_domain str)
  | _ -> None

  let is_open_feature ?domain name = match domain with
  | Some (Feature feature_domain) | Some (Both (_, feature_domain)) -> Feature_domain.is_open feature_domain name
  | _ -> true

  let is_num ?domain name = match domain with
  | Some (Feature feature_domain) | Some (Both (_, feature_domain)) -> Feature_domain.is_num feature_domain name
  | _ -> false

  let check_feature ?loc ?domain name value = match domain with
  | Some (Feature feature_domain) | Some (Both (_, feature_domain)) -> Feature_domain.check_feature ?loc ~feature_domain name value
  | _ -> ()

  let check_feature_name ?loc ?domain name = match domain with
  | Some (Feature feature_domain) | Some (Both (_, feature_domain)) ->
      if not (Feature_domain.is_defined name feature_domain.decls)
      then Error.build ?loc "The feature name \"%s\" in not defined in the domain" name
  | _ -> ()

  let label_to_dot ?domain ?deco label = match domain with
  | Some (Both (label_domain,_))
  | Some (Label label_domain) -> Label_domain.to_dot ?deco (Label_domain.get_style label_domain label)
  | _ -> Label_domain.to_dot ?deco { Label_domain.default with text = label }

  let label_to_dep ?domain ?deco label = match domain with
  | Some (Both (label_domain,_))
  | Some (Label label_domain) -> Label_domain.to_dep ?deco (Label_domain.get_style label_domain label)
  | _ -> Label_domain.to_dep (Label_domain.no_domain_style label)
end
