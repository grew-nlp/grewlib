(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Log
open Printf

open Grew_base

type feature_name = string (* cat, num, ... *)
type feature_atom = string (* V, N, inf, ... *)
type feature_value = string (* V, 4, "free text", ... *)
type suffix = string

type value = String of string | Float of float

let string_of_value = function
  | String s -> Str.global_replace (Str.regexp "\"") "\\\""
    (Str.global_replace (Str.regexp "\\\\") "\\\\\\\\" s)
  | Float i -> String_.of_float i

let conll_string_of_value = function
  | String s -> s
  | Float i -> String_.of_float i

type disjunction = value list

let dot_color string =
  match (string.[0], String.length string) with
  | ('#', 4) -> sprintf "\"#%c%c%c%c%c%c\"" string.[1] string.[1] string.[2] string.[2] string.[3] string.[3]
  | ('#', 7) -> sprintf "\"%s\"" string
  | _ -> string

(* ================================================================================ *)
module Pid = struct
  (* type t = int *)
  type t = Pos of int | Neg of int

  let compare = Pervasives.compare

  let to_id = function
    | Pos i -> sprintf "p_%d" i
    | Neg i -> sprintf "n_%d" i

  let to_string = function
    | Pos i -> sprintf "Pos %d" i
    | Neg i -> sprintf "Neg %d" i
end (* module Pid *)

(* ================================================================================ *)
module Pid_map =
  struct
    include Map.Make (Pid)

    exception True

    let exists fct map =
      try
        iter
          (fun key value ->
            if fct key value
            then raise True
          ) map;
        false
      with True -> true

    (* union of two maps*)
    let union_map m m' = fold (fun k v m'' -> (add k v m'')) m m'
end (* module Pid_map *)

(* ================================================================================ *)
module Pid_set = Set.Make (Pid)

(* ================================================================================ *)
module Gid = struct
  type t =
    | Old of int
    | New of (int * int) (* identifier for "created nodes" *)

  (* a compare function which ensures that new nodes are at the "end" of the graph *)
  let compare t1 t2 = match (t1,t2) with
    | Old o1, Old o2 -> Pervasives.compare o1 o2

    | Old _ , New _ -> -1
    | New _, Old _ -> 1
    | New n1, New n2 -> Pervasives.compare n1 n2

  let to_string = function
    | Old i -> sprintf "%d" i
    | New (i,j) -> sprintf"%d__%d" i j
end (* module Gid *)

(* ================================================================================ *)
module Gid_map = Map.Make (Gid)

(* ================================================================================ *)
module Massoc_gid = Massoc_make (Gid)

(* ================================================================================ *)
module Massoc_pid = Massoc_make (Pid)

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

  type t = string array * style array
  let empty = ([||],[||])

  (** The [default] style value *)
  let default = { text="UNSET"; bottom=false; color=None; bgcolor=None; line=Solid }

  (** [decl] is the type for a label declaration: the name and a list of display styles *)
  type decl = string * string list

  (** Computes the style of a label from its options and maybe its shape (like I:...). *)
  let parse_option string_label options =
    let init_style = match Str.bounded_split (Str.regexp ":") string_label 2 with
      | ["S"; l] -> {default with text=l; color=Some "red"}
      | ["D"; l] -> {default with text=l; color=Some "blue"; bottom=true}
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
  type feature_spec =
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  type t = feature_spec list

  let empty = []

  let is_defined feature_name domain =
    List.exists (function
      | Closed (fn,_) when fn = feature_name -> true
      | Open fn when fn = feature_name -> true
      | Num fn when fn = feature_name -> true
      | _ -> false
    ) domain

  let rec build = function
    | [] -> [Num "position"]
    | (Num "position") :: tail -> Log.warning "[Feature_domain] declaration of the feature name \"position\" in useless"; build tail
    | (Open "position") :: _
    | (Closed ("position",_)) :: _ ->
      Error.build "[Feature_domain] The feature named \"position\" is reserved and must be types 'integer', you cannot not redefine it"
    | (Num fn) :: tail | (Open fn) :: tail | Closed (fn,_) :: tail when is_defined fn tail ->
      Error.build "[Feature_domain] The feature named \"%s\" is defined several times" fn
    | x :: tail -> x :: (build tail)

  let feature_names domain =
    Some (List.map (function Closed (fn, _) | Open fn | Num fn -> fn) domain)

  let get feature_name domain =
    List.find (function
      | Closed (fn,_) when fn = feature_name -> true
      | Open fn when fn = feature_name -> true
      | Num fn when fn = feature_name -> true
      | _ -> false
    ) domain

  let sub domain name1 name2 =
    match (get name1 domain, get name2 domain) with
        | (_, Open _) -> true
        | (Closed (_,l1), Closed (_,l2)) -> List_.sort_include l1 l2
        | (Num _, Num _) -> true
        | _ -> false

  let is_open domain name =
    List.exists (function Open n when n=name -> true | _ -> false) domain

  (* This function is defined here because it is used by check_feature *)
  let build_disj ?loc domain name unsorted_values =
    let values = List.sort Pervasives.compare unsorted_values in
    match name.[0] with
      | '_' -> List.map (fun s -> String s) values (* no check on feat_name starting with '_' *)
      | _ ->
        let rec loop = function
          | [] -> Error.build ?loc "[GRS] Unknown feature name '%s'" name
          | ((Open n)::_) when n = name ->
            List.map (fun s -> String s) values
          | ((Num n)::_) when n = name ->
            (try List.map (fun s -> Float (String_.to_float s)) values
            with Failure _ -> Error.build ?loc "[GRS] The feature '%s' is of type int" name)
          | ((Closed (n,vs))::_) when n = name ->
            (match List_.sort_diff values vs with
              | [] -> List.map (fun s -> String s) values
              | l when List.for_all (fun x -> x.[0] = '_') l -> List.map (fun s -> String s) values
              | l -> Error.build ?loc "Unknown feature values '%s' for feature name '%s'"
                  (List_.to_string (fun x->x) ", " l)
          name
            )
          | _::t -> loop t in
        loop domain

  let build_closed feature_name feature_values =
    let sorted_list = List.sort Pervasives.compare feature_values in
    let without_duplicate =
      let rec loop = function
        | [] -> []
        | x::y::tail when x=y ->
          Log.fwarning "In the declaration of the feature name \"%s\", the value \"%s\" appears more than once" feature_name x;
          loop (y::tail)
        | x::tail -> x:: (loop tail)
      in loop sorted_list in
    Closed (feature_name, without_duplicate)

  let check_feature ?loc domain name value =
    ignore (build_disj ?loc domain name [value])
end (* Feature_domain *)


(* ================================================================================ *)
module Domain = struct
  type t = Label_domain.t * Feature_domain.t

  let build ld fd = (ld, fd)

  let empty = (Label_domain.empty, Feature_domain.empty)

  let feature_names (_, feature_domain) = Feature_domain.feature_names feature_domain

  let is_open_feature (_, feature_domain) name = Feature_domain.is_open feature_domain name

  let check_feature ?loc (_, feature_domain) name value = Feature_domain.check_feature ?loc feature_domain name value 

  let check_feature_name ?loc (_, feature_domain) name =
    if not (Feature_domain.is_defined name feature_domain)
    then Error.build ?loc "The feature name \"%s\" in not defined in the domain" name
end

(* ================================================================================ *)
module Label = struct
  (** Internal representation of labels *)
  type t =
    | Global of int       (* globally defined labels: their names are in the domain *)
    | Local of int        (* locally defined labels: names array should be provided! UNTESTED *)
    | Pattern of string

  let match_ ((table,_),_) p_label g_label = match (p_label, g_label) with
    | (Global p, Global g) when p=g -> true
    | (Pattern p, Global i) when String_.match_star_re p table.(i) -> true
    | _ -> false

  let match_list domain p_label_list g_label =
    List.exists (fun p_label -> match_ domain p_label g_label) p_label_list

  (** [to_string label_domain t] returns a string for the label *)
  let to_string ((table,_),_) ?(locals=[||]) = function
    | Global i -> table.(i)
    | Local i -> fst locals.(i)
    | Pattern s -> s

  let to_int = function
    | Global i -> Some i
    | _ -> None

  let get_style (_,styles) = function
    | Global i -> styles.(i)
    | Local i -> Log.warning "Style of locally defined labels is not implemented"; Label_domain.default
    | Pattern _ -> Label_domain.default

  let to_dep (label_domain,_) ?(deco=false) t =
    let style = get_style label_domain t in
    Label_domain.to_dep ~deco style

  let to_dot (label_domain,_) ?(deco=false) t =
    let style = get_style label_domain t in
    Label_domain.to_dot ~deco style

  let from_string ?loc ((table,_),_) ?(locals=[||]) str =
    if String.contains str '*'
    then Pattern str
    else
      try Global (Id.build ?loc str table)
      with Not_found (* TODO (CANNOT BE RAISED) *) ->
        try Local (Array_.dicho_find_assoc str locals)
        with Not_found -> Error.build "[Label.from_string] unknown edge label '%s'" str
end (* module Label *)


(* ================================================================================ *)
module Feature_value = struct
  let build_disj ?loc (_, feature_domain) name unsorted_values = Feature_domain.build_disj ?loc feature_domain name unsorted_values

  let build_value ?loc domain name value =
    match build_disj ?loc domain name [value] with
      | [x] -> x
      | _ -> Error.bug ?loc "[Feature_value.build_value]"
end (* module Feature_value *)



(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst = struct
  type t =
  | Pos of Label.t list
  | Neg of Label.t list

  let to_string domain = function
    | Pos l -> (List_.to_string (Label.to_string domain) "|" l)
    | Neg l -> "^"^(List_.to_string (Label.to_string domain) "|" l)

  let all = Neg []

  let match_ domain edge = function
    | Pos labels -> Label.match_list domain labels edge
    | Neg labels -> not (Label.match_list domain labels edge)

  let build ?loc domain ?locals = function
  | (edge_labels, true) -> Neg (List.sort compare (List.map (Label.from_string ?loc domain ?locals) edge_labels))
  | (edge_labels, false) -> Pos (List.sort compare (List.map (Label.from_string ?loc domain ?locals) edge_labels))
end (* module Label_cst *)

(* ================================================================================ *)
(* This module defines a type for lexical parameter (i.e. one line in a lexical file) *)
module Lex_par = struct

  type item = string list * string list (* first list: pattern parameters $id , second list command parameters @id *)

  type t = item list

  let size = List.length
  let append = List.append

  let signature = function
    | [] -> Error.bug "[Lex_par.signature] empty data"
    | (pp,cp)::_ -> (List.length pp,List.length cp)

  let dump t =
    printf "[Lex_par.dump] --> size = %d\n" (List.length t);
    List.iter (fun (pp,cp) ->
      printf "%s##%s\n"
        (String.concat "#" pp)
        (String.concat "#" cp)
    ) t

  let parse_line ?loc nb_p nb_c line =
    let line = String_.rm_peripheral_white line in
    if line = "" || line.[0] = '%'
    then None
    else
      let line = Str.global_replace (Str.regexp "\\\\%") "%" line in
      match Str.split (Str.regexp "##") line with
        | [args] when nb_c = 0 ->
          (match Str.split (Str.regexp "#") args with
            | l when List.length l = nb_p -> Some (l,[])
            | _ -> Error.build ?loc
              "Illegal lexical parameter line: \"%s\" doesn't contain %d args"
              line nb_p)
        | [args; values] ->
          (match (Str.split (Str.regexp "#") args, Str.split (Str.regexp "#") values) with
            | (lp,lc) when List.length lp = nb_p && List.length lc = nb_c -> Some (lp,lc)
            | _ -> Error.build ?loc
              "Illegal lexical parameter line: \"%s\" doesn't contain %d args and %d values"
              line nb_p nb_c)
        | _ -> Error.build ?loc "Illegal param line: '%s'" line

  let from_lines ?loc nb_p nb_c lines =
    match List_.opt_map (parse_line ?loc nb_p nb_c) lines with
    | [] -> Error.build ?loc "Empty lexical parameter list"
    | l -> l

  let load ?loc dir nb_p nb_c file =
    try
      let full_file =
        if Filename.is_relative file
        then Filename.concat dir file
        else file in
      let lines = File.read full_file in
      match List_.opt_mapi (fun i line -> parse_line ~loc:(Loc.file_line full_file i) nb_p nb_c line) lines with
      | [] -> Error.build ?loc "Empty lexical parameter file '%s'" file
      | l -> l
    with Sys_error _ -> Error.build ?loc "External lexical file '%s' not found" file

  let select index atom t =
    match
      List_.opt_map
        (fun (p_par, c_par) ->
          let par = List.nth p_par index in
          if atom = par
          then Some (p_par, c_par)
          else None
        ) t
    with
    | [] -> None
    | t -> Some t

  let get_param_value index = function
    | [] -> Error.bug "[Lex_par.get_command_value] empty parameter"
    | (params,_)::_ -> List.nth params index

  let get_command_value index = function
    | [(_,one)] -> List.nth one index
    | [] -> Error.bug "[Lex_par.get_command_value] empty parameter"
    | (_,[sing])::tail when index=0 ->
        Printf.sprintf "%s/%s"
          sing
          (List_.to_string
             (function
               | (_,[s]) -> s
               | _ -> Error.bug "[Lex_par.get_command_value] inconsistent param"
             ) "/" tail
          )
    | l -> Error.run "Lexical parameter are not functionnal"
end (* module Lex_par *)

(* ================================================================================ *)
module Concat_item = struct
  type t =
    | Feat of (Gid.t * feature_name)
    | String of string
end (* module Concat_item *)

