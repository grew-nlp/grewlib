open Log

open Utils
open Ast

module Label = struct
  type decl = string * string option

  type t = int
  
  let full = ref [||]
  let colors = ref [||]
      
  let init string_edge_list = 
    let slist = List.sort (fun (x,_) (y,_) -> compare x y) string_edge_list in
    let (labels, cols) = List.split slist in
    full := Array.of_list labels;
    colors := Array.of_list cols

  let to_string t = !full.(t)
  let to_int t = t

  let from_string ?loc ?(locals=[||]) string = 
    try Id.build ?loc string !full
    with Not_found -> 
      try -1 - (Array_.dicho_find_assoc string locals)
      with Not_found ->	Error.build "[Label.from_string] unknown edge label '%s'" string

  let get_color l = !colors.(l)
end


module Edge = struct

  (* the type of underspecified labels: a positive or negative constraint on a disjunction *)
  type under_label =
    | Pos of int list
    | Neg of int list
	 
  type t = {
      id: string option; (* an identifier for naming under_label in patterns *)
      under_label: under_label;
    }

  let all = {id=None; under_label=Neg []}

  let compare = Pervasives.compare

  let make ?(id=None) ?(neg=false) ?(locals=[||]) = function
    | l when neg -> {id=id; under_label=Neg (List.sort compare (List.map (Label.from_string ~locals) l))}
    | l -> {id=id; under_label=Pos (List.sort compare (List.map (Label.from_string ~locals) l))}

  let build ?locals (ast_edge, loc) =
    { id = ast_edge.Ast.edge_id;
      under_label = 
      if ast_edge.Ast.negative
      then Neg (List.sort compare (List.map (Label.from_string ~loc ?locals) ast_edge.Ast.edge_labels))
      else Pos (List.sort compare (List.map (Label.from_string ~loc ?locals) ast_edge.Ast.edge_labels))
    }


  let from_string string_label =
    match string_label with
    | s when s.[0] = '^' -> 
	let s' = String.sub s 1 ((String.length s) - 1) in
	{id=None; under_label=Neg (List.map Label.from_string (Str.split (Str.regexp "|") s'))}
    | _ -> {id=None; under_label=Pos (List.map Label.from_string (Str.split (Str.regexp "|") string_label))}

  let build_edge line =
    try
      let _ = Str.search_forward(Str.regexp("N\\(.*\\)->")) line 0 in
      let i = int_of_string (Str.matched_group 1 line) in
      let _ = Str.search_forward(Str.regexp("->N\\(.*\\)\\[")) line 0 in
      let j = int_of_string (Str.matched_group 1 line) in
      let _ = Str.search_forward(Str.regexp("label=\"\\([\\^|a-zA-Z0-9_\\-]*\\)\"")) line 0 in
      let string_label = Str.matched_group 1 line in
      (i,j,from_string string_label)
    with Not_found | Invalid_argument _ -> failwith (Printf.sprintf "%s is not an edge declaration" line)

  let to_string t = 
    let pref = match t.id with None -> "" | Some i -> "i:" in
    match t.under_label with
    | Pos l -> pref^(List_.to_string Label.to_string "|" l)
    | Neg l -> pref^"^"^(List_.to_string Label.to_string "|" l)

  let as_label t = match t.under_label with
    | Pos [one] -> one
    | _ -> failwith (Printf.sprintf "[Edge.as_label] edge '%s\' is not a label" (to_string t))

  let of_label l = {id=None; under_label=Pos [l]}

  let to_dot ?(deco=false) x =
    let l = as_label x in 
    match Label.get_color l with
    | None -> Printf.sprintf "[label=\"%s\", color=%s]" (Label.to_string l) (if deco then "red" else "black")
    | Some c -> Printf.sprintf "[label=\"%s\", fontcolor=%s, color=%s]" (Label.to_string l) c (if deco then "red" else "black")

  let to_dep ?(deco=false) x =
    let l = as_label x in 
    match (deco,Label.get_color l) with
    | (false,None) -> Printf.sprintf "{ label = \"%s\"; }" (Label.to_string l)
    | (false,Some c) -> Printf.sprintf "{ label = \"%s\"; forecolor=%s; color=%s; bottom; }" (Label.to_string l) c c
    | (true,None) -> Printf.sprintf "{ label = \"%s\"; color=red}" (Label.to_string l)
    | (true,Some c) -> Printf.sprintf "{ label = \"%s\"; forecolor=%s; color=red; bottom; }" (Label.to_string l) c

  let compatible edge1 edge2 = match (edge1.under_label,edge2.under_label) with
  | Pos l1, Pos l2 -> not (List_.sort_is_empty_inter l1 l2)
  | Pos p, Neg n | Neg n, Pos p -> not (List_.sort_include p n)
  | Neg l1, Neg l2 -> failwith "Cannot compare two negative sets"

  let is_in graph_edge list = 
    List.mem (as_label graph_edge) list 


  type edge_matcher =
    | Fail
    | Ok of Label.t
    | Binds of string * Label.t list

  let match_ pattern_edge graph_edge =
    let graph_label = as_label graph_edge in

    match pattern_edge with
    | {id = Some i; under_label = Pos l} when List.mem graph_label l -> Binds (i, [graph_label])
    | {id = None; under_label = Pos l} when List.mem graph_label l -> Ok graph_label
    | {id = Some i; under_label = Neg l} when not (List.mem graph_label l) -> Binds (i, [graph_label])
    | {id = None; under_label = Neg l} when not (List.mem graph_label l) -> Ok graph_label
    | _ -> Fail

  let match_list pattern_edge graph_edge_list =
    let graph_labels = List.map as_label graph_edge_list in
    match pattern_edge with
    | {id = None; under_label = Pos l} when List.exists (fun label -> List.mem label l) graph_labels -> Ok (List.hd graph_labels)
    | {id = None; under_label = Neg l} when List.exists (fun label -> not (List.mem label l)) graph_labels -> Ok (List.hd graph_labels)
    | {id = Some i; under_label = Pos l} ->
	(match List.filter (fun label -> List.mem label l) graph_labels with
	| [] -> Fail
	| list -> Binds (i, list))
    | {id = Some i; under_label = Neg l} ->
	(match List.filter (fun label -> not (List.mem label l)) graph_labels with
	| [] -> Fail
	| list -> Binds (i, list))
    | _ -> Fail


end
