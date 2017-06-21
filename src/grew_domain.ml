(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
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

  let is_void style = (style.text = "void")

  type t = string array * style array

  (** The [default] style value *)
  let default = { text="UNSET"; bottom=false; color=None; bgcolor=None; line=Solid }

  (** [decl] is the type for a label declaration: the name and a list of display styles *)
  type decl = string * string list

  (** Computes the style of a label from its options and maybe its shape (like I:...). *)
  let parse_option string_label options =
    let init_style = match Str.bounded_split (Str.regexp ":") string_label 2 with
      | ["S"; l] -> {default with text=l; color=Some "red"}
      | ["D"; l] | ["E"; l] -> {default with text=l; color=Some "blue"; bottom=true}
      | ["I"; l] -> {default with text=l; color=Some "grey"}
      | _ -> {default with text=string_label} in
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
    let slist = List.sort (fun (x,_) (y,_) -> compare x y) decl_list in
    let (labels, opts) = List.split slist in
    let labels_array = Array.of_list labels in
    (labels_array,
      Array.mapi (fun i opt -> parse_option labels_array.(i) opt) (Array.of_list opts)
    )

  let to_dep ?(deco=false) style =
    let dep_items =
      (if style.bottom then ["bottom"] else [])
      @ (match style.color with Some c -> ["color="^c; "forecolor="^c] | None -> [])
      @ (match style.bgcolor with Some c -> ["bgcolor="^c] | None -> [])
      @ (match style.line with
        | Dot -> ["style=dot"]
        | Dash -> ["style=dash"]
        | Solid when deco -> ["bgcolor=yellow"]
        | Solid -> []) in
    sprintf "{ label = \"%s\"; %s}" style.text (String.concat "; " dep_items)

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
        | Solid when deco -> ["style=dotted"]
        | Solid -> []) in
      sprintf "[label=\"%s\", %s]" style.text (String.concat ", " dot_items)

end

(* ================================================================================ *)
module Feature_domain = struct
  type t = Ast.feature_spec list

  let is_defined feature_name feature_domain =
    List.exists (function
      | Ast.Closed (fn,_) when fn = feature_name -> true
      | Ast.Open fn when fn = feature_name -> true
      | Ast.Num fn when fn = feature_name -> true
      | _ -> false
    ) feature_domain

  let rec build = function
    | [] -> [Ast.Num "position"]
    | (Ast.Num "position") :: tail -> Log.warning "[Feature_domain] declaration of the feature name \"position\" in useless"; build tail
    | (Ast.Open "position") :: _
    | (Ast.Closed ("position",_)) :: _ ->
      Error.build "[Feature_domain] The feature named \"position\" is reserved and must be types 'integer', you cannot not redefine it"
    | (Ast.Num fn) :: tail | (Ast.Open fn) :: tail | Ast.Closed (fn,_) :: tail when is_defined fn tail ->
      Error.build "[Feature_domain] The feature named \"%s\" is defined several times" fn
    | x :: tail -> x :: (build tail)

  let feature_names feature_domain =
    List.map (function Ast.Closed (fn, _) | Ast.Open fn | Ast.Num fn -> fn) feature_domain

  let get feature_name feature_domain =
    List.find (function
      | Ast.Closed (fn,_) when fn = feature_name -> true
      | Ast.Open fn when fn = feature_name -> true
      | Ast.Num fn when fn = feature_name -> true
      | _ -> false
    ) feature_domain

  let sub feature_domain name1 name2 =
    match (get name1 feature_domain, get name2 feature_domain) with
        | (_, Ast.Open _) -> true
        | (Ast.Closed (_,l1), Ast.Closed (_,l2)) -> List_.sort_include l1 l2
        | (Ast.Num _, Ast.Num _) -> true
        | _ -> false

  let is_open feature_domain name =
    List.exists (function Ast.Open n when n=name -> true | _ -> false) feature_domain

  (* This function is defined here because it is used by check_feature *)
  let build_disj ?loc ?feature_domain name unsorted_values =
    let values = List.sort Pervasives.compare unsorted_values in
    match (feature_domain, name.[0]) with
      | (None, _)
      | (Some _, '_') -> List.map (fun s -> String s) values (* no check on feat_name starting with '_' *)
      | (Some dom, _) ->
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
  type t = Label_domain.t * Feature_domain.t

  let build ld fd = (ld, fd)

  let build_disj ?loc ?domain name unsorted_values =
    match domain with
    | Some (_, feature_domain) -> Feature_domain.build_disj ?loc ~feature_domain name unsorted_values
    | None -> Feature_domain.build_disj ?loc name unsorted_values

  let feature_names (_, feature_domain) = Feature_domain.feature_names feature_domain

  let get_label_name ?domain index = match domain with
  | Some ((names,_),_) -> Some names.(index)
  | None -> None

  let get_label_style ?domain index = match domain with
  | Some ((_,styles),_) -> Some styles.(index)
  | None -> None

  let edge_id_from_string ?loc ?domain str = match domain with
  | Some ((names,_),_) ->
    begin
      try Some (Id.build ?loc str names)
      with Not_found -> Error.build "[Domain.edge_id_from_string] unknown edge label '%s'" str
    end
  | None -> None


  let is_open_feature ?domain name = match domain with
  | Some (_, feature_domain) -> Feature_domain.is_open feature_domain name
  | None -> true

  let check_feature ?loc ?domain name value = match domain with
  | Some (_, feature_domain) -> Feature_domain.check_feature ?loc ~feature_domain name value
  | None -> ()

  let check_feature_name ?loc ?domain name = match domain with
  | None -> ()
  | Some (_, feature_domain) ->
      if not (Feature_domain.is_defined name feature_domain)
      then Error.build ?loc "The feature name \"%s\" in not defined in the domain" name
end
