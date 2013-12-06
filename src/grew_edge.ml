open Log
open Printf

open Libgrew_utils
open Grew_ast


let rm_first_char = function "" -> "" | s -> String.sub s 1 ((String.length s) - 1)

(* ================================================================================ *)
module Label = struct
  (** Global names and display styles are recorded in two aligned arrays *)
  let full = ref None

  (** Internal representation of labels *)
  type t =
    | Global of int       (* globally defined labels: their names are in the [full] array *)
    | Local of int        (* locally defined labels: names array should be provided! UNTESTED *)
    | No_domain of string (* out of domain label: name in not constrained *)

  (** [to_string t] returns a string for the label *)
  let to_string ?(locals=[||]) t =
    match (!full, t) with
      | (_, No_domain s) -> s
      | (Some table, Global i) -> table.(i)
      | (Some _, Local i) -> fst locals.(i)
      | _ -> Error.bug "[Label.to_string] inconsistent data"

  let to_int = function
    | Global i -> Some i
    | _ -> None

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

  let styles = ref ([||] : style array)

  let get_style = function
    | Global i -> !styles.(i)
    | Local i -> Log.warning "Style of locally defined labels is not implemented"; default
    | No_domain s -> { default with text=s }

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
            | s -> {acc_style with color=Some (rm_first_char s)}
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
        | Solid when deco -> ["style=dot"]
        | Solid -> []) in
    sprintf "{ label = \"%s\"; %s}" style.text (String.concat "; " dep_items)

  let to_dot ?(deco=false) t =
    let style = get_style t in
    let dot_items =
      (match style.color with Some c -> ["color="^c; "fontcolor="^c] | None -> [])
      @ (match style.line with
        | Dot -> ["style=dotted"]
        | Dash -> ["style=dashed"]
        | Solid when deco -> ["style=dotted"]
        | Solid -> []) in
    sprintf "[label=\"%s\", %s}" style.text (String.concat ", " dot_items)

  let from_string ?loc ?(locals=[||]) string =
    match !full with
      | None -> No_domain string
      | Some table ->
        try Global (Id.build ?loc string table)
        with Not_found ->
          try Local (Array_.dicho_find_assoc string locals)
          with Not_found -> Error.build "[Label.from_string] unknown edge label '%s'" string
end (* module Label *)


(* ================================================================================ *)
module G_edge = struct
  type t = Label.t

  let to_string ?(locals=[||]) t = Label.to_string ~locals t

  let root = Label.No_domain "root"

  let make ?loc ?(locals=[||]) string = Label.from_string ?loc ~locals string

  let build ?locals (ast_edge, loc) =
    match ast_edge.Ast.negative, ast_edge.Ast.edge_labels with
    | (false, [one]) -> Label.from_string ~loc ?locals one
    | (true, _) -> Error.build "Negative edge spec are forbidden in graphs%s" (Loc.to_string loc)
    | (false, _) -> Error.build "Only atomic edge valus are allowed in graphs%s" (Loc.to_string loc)

  let to_dep ?(deco=false) t = Label.to_dep ~deco t
  let to_dot ?(deco=false) t = Label.to_dot ~deco t

  let color_of_option = function
    | [] -> None
    | c::_ -> Some (rm_first_char c)


end (* module G_edge *)


(* ================================================================================ *)
module P_edge = struct
  type u_label =
    | Pos of Label.t list
    | Neg of Label.t list

  type t = {
      id: string option; (* an identifier for naming under_label in patterns *)
      u_label: u_label;
    }

  let all = {id=None; u_label=Neg []}

  let get_id t = t.id

  let make ?loc ?(id=None) ?(neg=false) ?(locals=[||]) = function
    | l when neg -> {id=id; u_label=Neg (List.sort compare (List.map (Label.from_string ?loc ~locals) l))}
    | l -> {id=id; u_label=Pos (List.sort compare (List.map (Label.from_string ?loc ~locals) l))}

  let build ?locals (ast_edge, loc) =
    { id = ast_edge.Ast.edge_id;
      u_label =
      if ast_edge.Ast.negative
      then Neg (List.sort compare (List.map (Label.from_string ~loc ?locals) ast_edge.Ast.edge_labels))
      else Pos (List.sort compare (List.map (Label.from_string ~loc ?locals) ast_edge.Ast.edge_labels))
    }

  let to_string t =
    let pref = match t.id with None -> "" | Some i -> sprintf "%s:" i in
    match t.u_label with
    | Pos l -> pref^(List_.to_string Label.to_string "|" l)
    | Neg l -> pref^"^"^(List_.to_string Label.to_string "|" l)


  let compatible t g_edge = match t.u_label with
  | Pos p -> List_.sort_mem g_edge p
  | Neg n -> not (List_.sort_mem g_edge n)

  type edge_matcher =
    | Fail
    | Ok of Label.t
    | Binds of string * Label.t list

  let match_ pattern_edge graph_label =

    match pattern_edge with
    | {id = Some i; u_label = Pos l} when List.mem graph_label l -> Binds (i, [graph_label])
    | {id = None; u_label = Pos l} when List.mem graph_label l -> Ok graph_label
    | {id = Some i; u_label = Neg l} when not (List.mem graph_label l) -> Binds (i, [graph_label])
    | {id = None; u_label = Neg l} when not (List.mem graph_label l) -> Ok graph_label
    | _ -> Fail

  let match_list pattern_edge graph_edge_list =
    match pattern_edge with
    | {id = None; u_label = Pos l} when List.exists (fun label -> List.mem label l) graph_edge_list ->
        Ok (List.hd graph_edge_list)
    | {id = None; u_label = Neg l} when List.exists (fun label -> not (List.mem label l)) graph_edge_list ->
        Ok (List.hd graph_edge_list)
    | {id = Some i; u_label = Pos l} ->
	(match List.filter (fun label -> List.mem label l) graph_edge_list with
	| [] -> Fail
	| list -> Binds (i, list))
    | {id = Some i; u_label = Neg l} ->
	(match List.filter (fun label -> not (List.mem label l)) graph_edge_list with
	| [] -> Fail
	| list -> Binds (i, list))
    | _ -> Fail

end (* module P_edge *)

