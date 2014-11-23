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
    | Act of (int * string)  (* identifier for "activated nodes" *)

  (* a compare function which ensures that new nodes are at the "end" of the graph *)
  let compare t1 t2 = match (t1,t2) with
    | Old o1, Old o2 -> Pervasives.compare o1 o2

    | Old _ , New _ -> -1
    | New _, Old _ -> 1
    | New n1, New n2 -> Pervasives.compare n1 n2

    | Old _ , Act _ -> -1
    | Act _, Old _ -> 1
    | Act n1, Act n2 -> Pervasives.compare n1 n2

    | Act _ , New _ -> -1
    | New _, Act _ -> 1

  let to_string = function
    | Old i -> sprintf "%d" i
    | New (i,j) -> sprintf"%d__%d" i j
    | Act (i,n) -> sprintf"%d____%s" i n
end (* module Gid *)

(* ================================================================================ *)
module Gid_map = Map.Make (Gid)

(* ================================================================================ *)
module Massoc_gid = Massoc_make (Gid)

(* ================================================================================ *)
module Massoc_pid = Massoc_make (Pid)

(* ================================================================================ *)
module Label = struct
  (** describe the display style of a label *)
  type line = Solid | Dot | Dash
  type style = {
    text: string;
    bottom: bool;
    color: string option;
    bgcolor: string option;
    line: line;
  }

  (** Global names and display styles are recorded in two aligned arrays *)
  let full = ref None
  let styles = ref ([||] : style array)

  (** Internal representation of labels *)
  type t =
    | Global of int       (* globally defined labels: their names are in the [full] array *)
    | Local of int        (* locally defined labels: names array should be provided! UNTESTED *)

  (** [to_string t] returns a string for the label *)
  let to_string ?(locals=[||]) t =
    match (!full, t) with
      | (Some table, Global i) -> table.(i)
      | (Some _, Local i) -> fst locals.(i)
      | _ -> Error.bug "[Label.to_string] labels were not properly initialized"

  let to_int = function
    | Global i -> Some i
    | _ -> None

  (** The [default] style value *)
  let default = { text="UNSET"; bottom=false; color=None; bgcolor=None; line=Solid }

  let get_style = function
    | Global i -> !styles.(i)
    | Local i -> Log.warning "Style of locally defined labels is not implemented"; default

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

  (** [decl] is the type for a label declaration: the name and a list of display styles *)
  type decl = string * string list

  (* [init decl_list] updates global arrays [full] and [styles] *)
  let init decl_list =
    let slist = List.sort (fun (x,_) (y,_) -> compare x y) decl_list in
    let (labels, opts) = List.split slist in
    let labels_array = Array.of_list labels in
    full := Some labels_array;
    styles := Array.mapi (fun i opt -> parse_option labels_array.(i) opt) (Array.of_list opts)

  let to_dep ?(deco=false) t =
    let style = get_style t in
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

  let to_dot ?(deco=false) t =
    let style = get_style t in
    let dot_items =
      (match style.color with Some c -> let d = dot_color c in ["color="^d; "fontcolor="^d] | None -> [])
      @ (match style.line with
        | Dot -> ["style=dotted"]
        | Dash -> ["style=dashed"]
        | Solid when deco -> ["style=dotted"]
        | Solid -> []) in
    sprintf "[label=\"%s\", %s]" style.text (String.concat ", " dot_items)

  let from_string ?loc ?(locals=[||]) string =
    match !full with
      | None -> Error.bug "[Label.from_string] labels were not properly initialized"
      | Some table ->
        try Global (Id.build ?loc string table)
        with Not_found ->
          try Local (Array_.dicho_find_assoc string locals)
          with Not_found -> Error.build "[Label.from_string] unknown edge label '%s'" string
end (* module Label *)

(* ================================================================================ *)
module Domain = struct
  type feature_spec =
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  type t = feature_spec list

  let (current: t option ref) = ref None

  let reset () = current := None

  let is_defined feature_name domain =
    List.exists (function
      | Closed (fn,_) when fn = feature_name -> true
      | Open fn when fn = feature_name -> true
      | Num fn when fn = feature_name -> true
      | _ -> false
    ) domain

  let get feature_name domain =
    List.find (function
      | Closed (fn,_) when fn = feature_name -> true
      | Open fn when fn = feature_name -> true
      | Num fn when fn = feature_name -> true
      | _ -> false
    ) domain

  let check_feature_name ?loc name =
    match !current with
      | None -> ()
      | Some dom when is_defined name dom -> ()
      | _ -> Error.build ?loc "The feature name \"%s\" in not defined in the domain" name

  let is_open name =
      match !current with
      | None -> true
      | Some dom -> List.exists (function Open n when n=name -> true | _ -> false) dom

  let rec normalize_domain = function
    | [] -> [Num "position"]
    | (Num "position") :: tail -> Log.warning "[Domain] declaration of the feature name \"position\" in useless"; normalize_domain tail
    | (Open "position") :: _
    | (Closed ("position",_)) :: _ ->
      Error.build "[Domain] The feature named \"position\" is reserved and must be types 'integer', you cannot not redefine it"
    | (Num fn) :: tail |  (Open fn) :: tail |  Closed (fn,_) :: tail when is_defined fn tail ->
      Error.build "[Domain] The feature named \"%s\" is defined several times" fn
    | x :: tail -> x :: (normalize_domain tail)

  let init domain =
    current := Some (normalize_domain domain)

  let build ?loc name unsorted_values =
    let values = List.sort Pervasives.compare unsorted_values in
    match (name.[0], !current) with
      | ('_', _) (* no check on feat_name starting with '_' *)
      | (_, None) -> List.map (fun s -> String s) values (* no domain defined *)
      | (_, Some dom) ->
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
        loop dom

  let build_one ?loc name value =
    match build ?loc name [value] with
      | [x] -> x
      | _ -> Error.bug ?loc "[Domain.build_one]"

  let check_feature ?loc name value =
    ignore (build ?loc name [value])

  let feature_names () =
    match !current with
      | None -> None
      | Some dom -> Some (List.map (function Closed (fn, _) | Open fn | Num fn -> fn) dom)

  let sub name1 name2 =
    match !current with
      | None -> true
      | Some dom ->
      match (get name1 dom, get name2 dom) with
        | (_, Open _) -> true
        | (Closed (_,l1), Closed (_,l2)) -> List_.sort_include l1 l2
        | (Num _, Num _) -> true
        | _ -> false


end (* Domain *)

(* ================================================================================ *)
module Conll = struct
  type line = {
      line_num: int;
      num: string;
      phon: string;
      lemma: string;
      pos1: string;
      pos2: string;
      morph: (string * string) list;
      deps: (string * string ) list;
    }

  let root = { line_num = -1; num="0"; phon="ROOT"; lemma="__"; pos1="_X"; pos2=""; morph=[]; deps=[] }

  let line_to_string l =
    let (gov_list, lab_list) = List.split l.deps in
    sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
      l.num l.phon l.lemma l.pos1 l.pos2
      (match l.morph with [] -> "_" | list -> String.concat "|" (List.map (fun (f,v) -> sprintf "%s=%s" f v) list))
      (String.concat "|" (gov_list))
      (String.concat "|" (lab_list))

  let parse_morph file_name line_num = function
    | "_" -> []
    | morph ->
      List.map
        (fun feat ->
          match Str.split (Str.regexp "=") feat with
            | [feat_name] -> (feat_name, "true")
            | [feat_name; feat_value] -> (feat_name, feat_value)
            | _ -> Error.build ~loc:(Loc.file_line file_name line_num) "[Conll.load] illegal morphology \n>>>>>%s<<<<<<" morph
        ) (Str.split (Str.regexp "|") morph)

  let underscore s = if s = "" then "_" else s
  let parse_line file_name (line_num, line) =
    match Str.split (Str.regexp "\t") line with
      | [ num; phon; lemma; pos1; pos2; morph; govs; dep_labs; _; _ ] ->
        begin
          try
            let gov_list = if govs = "_" then [] else Str.split (Str.regexp "|") govs
            and lab_list = if dep_labs = "_" then [] else Str.split (Str.regexp "|") dep_labs in
            let deps = List.combine gov_list lab_list in
            {line_num = line_num;
             num = num;
             phon = underscore phon;
             lemma = underscore lemma;
             pos1 = underscore pos1;
             pos2 = underscore pos2;
             morph = parse_morph file_name line_num morph;
             deps = deps;
            }
          with exc -> Error.build ~loc:(Loc.file_line file_name line_num) "[Conll.load] illegal line, exc=%s\n>>>>>%s<<<<<<" (Printexc.to_string exc) line
        end
      | l -> Error.build ~loc:(Loc.file_line file_name line_num) "[Conll.load] illegal line, %d fields (10 are expected)\n>>>>>%s<<<<<<" (List.length l) line

  let load file_name =
    let lines = File.read_ln file_name in
    List.map (parse_line file_name) lines

  let parse file_name lines = List.map (parse_line file_name) lines

    (* We would prefer to compare the float equivalent of l1.num l2.num but this would break the dicho_find function *)
  let compare l1 l2 = Pervasives.compare ((* float_of_string *) l1.num) ((* float_of_string *) l2.num)
end (* module Conll *)

(* ================================================================================ *)
(* This module defines a type for lexical parameter (i.e. one line in a lexical file) *)
module Lex_par = struct

  type item = string list * string list (* first list: pattern parameters $id , second list command parameters @id *)

  type t = item list

  let empty=[]
  let append = List.append

  let dump t =
    printf "[Lex_par.dump] --> size = %d\n" (List.length t);
    List.iter (fun (pp,cp) ->
      printf "%s##%s\n"
        (String.concat "#" pp)
        (String.concat "#" cp)
    ) t

  let rm_peripheral_white s =
    Str.global_replace (Str.regexp "\\( \\|\t\\)*$") ""
    (Str.global_replace (Str.regexp "^\\( \\|\t\\)*") "" s)

  let parse_line ?loc nb_p nb_c line =
    let line = rm_peripheral_white line in
    if line = "" || line.[0] = '%'
    then None
    else
      match Str.split (Str.regexp "##") line with
        | [args] when nb_c = 0 ->
          (match Str.split (Str.regexp "#") args with
            | l when List.length l = nb_p -> Some (l,[])
            | _ -> Error.bug ?loc
              "Illegal lexical parameter line: \"%s\" doesn't contain %d args"
              line nb_p)
        | [args; values] ->
          (match (Str.split (Str.regexp "#") args, Str.split (Str.regexp "#") values) with
            | (lp,lc) when List.length lp = nb_p && List.length lc = nb_c -> Some (lp,lc)
            | _ -> Error.bug ?loc
              "Illegal lexical parameter line: \"%s\" doesn't contain %d args and %d values"
              line nb_p nb_c)
        | _ -> Error.bug ?loc "Illegal param line: '%s'" line

  let from_lines ?loc nb_p nb_c lines = List_.opt_map (parse_line ?loc nb_p nb_c) lines

  let load ?loc dir nb_p nb_c file =
    try
      let full_file =
        if Filename.is_relative file
        then Filename.concat dir file
        else file in
      let lines = File.read full_file in
      List_.opt_mapi (fun i line -> parse_line ~loc:(Loc.file_line full_file i) nb_p nb_c line) lines
    with Sys_error _ -> Error.build ?loc "External lexical file '%s' not found" file

  let sub x y = List.mem x (Str.split (Str.regexp "|") y)

  let filter index atom t =
    match
      List_.opt_map
        (fun (p_par, c_par) ->
          let par = List.nth p_par index in
          if atom=par
          then Some (p_par, c_par)
          else
            if sub atom par (* atom is one of the values of the disjunction par *)
            then Some (List_.set index atom p_par, c_par)
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

