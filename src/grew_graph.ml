open Printf
open Log

open Grew_utils
open Grew_ast
open Grew_edge
open Grew_fs
open Grew_node
open Grew_command

module Str_map = Map.Make (String)

(* ==================================================================================================== *)
module P_deco = struct
  type t = {
    nodes: Pid.t list;
    edges: (Pid.t * P_edge.t * Pid.t) list;
  }

  let empty = {nodes=[]; edges=[]}
end (* module P_deco *)

(* ==================================================================================================== *)
module P_graph = struct
  type t = P_node.t Pid_map.t

  let empty = Pid_map.empty
  let find = Pid_map.find

  let map_add_edge map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Pid_map.find id_src map with Not_found -> P_node.empty in
    match P_node.add_edge label id_tar node_src with
    | None -> None
    | Some new_node -> Some (Pid_map.add id_src new_node map)

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  (* -------------------------------------------------------------------------------- *)
  let build_filter table (ast_node, loc) =
    let pid = Id.build ~loc ast_node.Ast.node_id table in
    let fs = P_fs.build ast_node.Ast.fs in
    (pid, fs)

  (* -------------------------------------------------------------------------------- *)
  let build ?pat_vars ?(locals=[||]) (full_node_list : Ast.node list) full_edge_list =

    (* NB: insert searches for a previous node with the Same name and uses unification rather than constraint *)
    (* NB: insertion of new node at the end of the list: not efficient but graph building is not the hard part. *)
    let rec insert (ast_node, loc) = function
      | [] -> [P_node.build ?pat_vars (ast_node, loc)]
      | (node_id,fs)::tail when ast_node.Ast.node_id = node_id ->
          (node_id, P_node.unif_fs (P_fs.build ?pat_vars ast_node.Ast.fs) fs) :: tail
      | head :: tail -> head :: (insert (ast_node, loc) tail) in

    let (named_nodes : (Id.name * P_node.t) list) =
      List.fold_left
        (fun acc ast_node -> insert ast_node acc)
        [] full_node_list in

    let sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Pervasives.compare id1 id2) named_nodes in
    let (sorted_ids, node_list) = List.split sorted_nodes in

    (* [pos_table] contains the sorted list of node ids *)
    let pos_table = Array.of_list sorted_ids in

    (* the nodes, in the same order *)
    let map_without_edges = List_.foldi_left
      (fun i acc elt -> Pid_map.add (Pid.Pos i) elt acc)
      Pid_map.empty node_list in

    let (map : t) =
      List.fold_left
	(fun acc (ast_edge, loc) ->
	  let i1 = Id.build ~loc ast_edge.Ast.src pos_table in
	  let i2 = Id.build ~loc ast_edge.Ast.tar pos_table in
	  let edge = P_edge.build ~locals (ast_edge, loc) in
	  (match map_add_edge acc (Pid.Pos i1) edge (Pid.Pos i2) with
	  | Some g -> g
	  | None -> Error.build "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s"
                (P_edge.to_string edge)
                (Loc.to_string loc)
	  )
	) map_without_edges full_edge_list in
    (map, pos_table)


  (* -------------------------------------------------------------------------------- *)
  (* a type for extension of graph: a former graph exists:
     in grew the former is a positive pattern and an extension is a "without" *)
  type extension = {
      ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
      old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *) 	
    }

  (* -------------------------------------------------------------------------------- *)
  let build_extension ?(locals=[||]) pos_table full_node_list full_edge_list =

    let built_nodes = List.map P_node.build full_node_list in

    let (old_nodes, new_nodes) =
      List.partition
        (function (id,_) when Array_.dicho_mem id pos_table -> true | _ -> false)
        built_nodes in
	
    let new_sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Pervasives.compare id1 id2) new_nodes in

    let (new_sorted_ids, new_node_list) = List.split new_sorted_nodes in

    (* table contains the sorted list of node ids *)
    let new_table = Array.of_list new_sorted_ids in

    (* the nodes, in the same order stored with index -1, -2, ... -N *)
    let ext_map_without_edges =
      List_.foldi_left
	(fun i acc elt -> Pid_map.add (Pid.Neg i) elt acc)
	Pid_map.empty
	new_node_list in

    let old_map_without_edges =
      List.fold_left
	(fun acc (id,node) -> Pid_map.add (Pid.Pos (Array_.dicho_find id pos_table)) node acc)
	Pid_map.empty
	old_nodes in

    let ext_map_with_all_edges =
      List.fold_left
	(fun acc (ast_edge, loc) ->
	  let i1 =
	    match Id.build_opt ast_edge.Ast.src pos_table with
              | Some i -> Pid.Pos i
              | None -> Pid.Neg (Id.build ~loc ast_edge.Ast.src new_table) in
	  let i2 =
	    match Id.build_opt ast_edge.Ast.tar pos_table with
              | Some i -> Pid.Pos i
              | None -> Pid.Neg (Id.build ~loc ast_edge.Ast.tar new_table) in
	  let edge = P_edge.build ~locals (ast_edge, loc) in
	  match map_add_edge acc i1 edge i2 with
	  | Some map -> map
	  | None -> Log.fbug "[GRS] [Graph.build_extension] add_edge cannot fail in pattern extension (1)"; exit 2
	) ext_map_without_edges full_edge_list in

    ({ext_map = ext_map_with_all_edges; old_map = old_map_without_edges}, new_table)

  (* [tree_and_roots t] returns:
     - a boolean which is true iff the each node has at most one in-edge
     - the list of "roots" (i.e. nodes without in-edge *)
  let tree_and_roots graph =
    let tree_prop = ref true in
    let not_root =
      Pid_map.fold
	(fun _ node acc ->
	  Massoc_pid.fold
	    (fun acc2 tar _ ->
	      if !tree_prop
	      then
		if Pid_set.mem tar acc2
		then (tree_prop := false; acc2)
		else Pid_set.add tar acc2
	      else Pid_set.add tar acc2
	    ) acc (P_node.get_next node)
	) graph Pid_set.empty in

    let roots =
      Pid_map.fold
	(fun id _ acc ->
	  if Pid_set.mem id not_root
	  then acc
	  else id::acc
	) graph [] in

    (!tree_prop, roots)

  let roots graph = snd (tree_and_roots graph)
end (* module P_graph *)

(* ==================================================================================================== *)
module G_deco = struct
  type t = {
    nodes: Gid.t list;
    edges: (Gid.t * G_edge.t * Gid.t) list;
  }

  let empty = {nodes=[]; edges=[]}
end (* module G_deco *)

(* ==================================================================================================== *)
module Concat_item = struct
  type t =
    | Feat of (Gid.t * string)
    | String of string
end (* module Concat_item *)

(* ==================================================================================================== *)
module G_graph = struct
  type t = {
    meta: (string * string) list;
    map: G_node.t Gid_map.t; (* node description *)
  }

  let empty = {meta=[]; map=Gid_map.empty}

  let find node_id graph = Gid_map.find node_id graph.map

  let equals t t' = Gid_map.equal (fun node1 node2 -> node1 = node2) t.map t'.map

(* Ocaml < 3.12 doesn't have exists function for maps! *)
  exception True
  let node_exists fct t =
    try
      Gid_map.iter (fun _ v -> if fct v then raise True) t.map;
      false
    with True -> true
(* Ocaml < 3.12 doesn't have exists function for maps! *)

  let fold_gid fct t init =
    Gid_map.fold (fun gid _ acc -> fct gid acc) t.map init

  let max_binding t =
    match Gid_map.max_binding t.map with
      | (Gid.Old i,_) -> i
      | _ -> Error.bug "[G_graph.max_binding]"

  let list_num test =
    let rec loop n = function
      | [] -> raise Not_found
      | x::_ when test x -> n
      | _::t -> loop (n+1) t
    in loop 0

  (* is there an edge e out of node i ? *)
  let edge_out graph node_id p_edge =
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> P_edge.compatible p_edge e) (G_node.get_next node)

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Gid_map.find id_src map with Not_found -> G_node.empty in
    match G_node.add_edge label id_tar node_src with
    | None -> None
    | Some new_node -> Some (Gid_map.add id_src new_node map)

  let add_edge graph id_src label id_tar =
    match map_add_edge graph.map id_src label id_tar with
      | Some new_map -> Some {graph with map = new_map }
      | None -> None

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Build functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  let build ?(locals=[||]) gr_ast =
    let full_node_list = gr_ast.Ast.nodes
    and full_edge_list = gr_ast.Ast.edges in

    let named_nodes =
      let rec loop already_bound = function
        | [] -> []
        | (ast_node, loc) :: tail ->
            let tail = loop (ast_node.Ast.node_id :: already_bound) tail in
            if List.mem ast_node.Ast.node_id already_bound
            then Error.build "[GRS] [Graph.build] try to build a graph with twice the same node id '%s'" ast_node.Ast.node_id
            else G_node.build (ast_node, loc) :: tail in
      loop [] full_node_list in

    let sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Pervasives.compare id1 id2) named_nodes in
    let (sorted_ids, node_list) = List.split sorted_nodes in

    (* table contains the sorted list of node ids *)
    let table = Array.of_list sorted_ids in

    (* the nodes, in the same order *)
    let map_without_edges = List_.foldi_left (fun i acc elt -> Gid_map.add (Gid.Old i) elt acc) Gid_map.empty node_list in

    let map =
      List.fold_left
	(fun acc (ast_edge, loc) ->
	  let i1 = Id.build ~loc ast_edge.Ast.src table in
	  let i2 = Id.build ~loc ast_edge.Ast.tar table in
	  let edge = G_edge.build ~locals (ast_edge, loc) in
	  (match map_add_edge acc (Gid.Old i1) edge (Gid.Old i2) with
	  | Some g -> g
	  | None -> Error.build "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s"
                (G_edge.to_string edge)
                (Loc.to_string loc)
	  )
	) map_without_edges full_edge_list in

    {meta=gr_ast.Ast.meta; map=map}

  (* -------------------------------------------------------------------------------- *)
  let of_conll ?loc lines =

    let nodes =
      List.fold_left
        (fun acc line ->
          Gid_map.add (Gid.Old line.Conll.num) (G_node.of_conll line) acc
        ) Gid_map.empty lines in

    let nodes_with_edges =
      List.fold_left
        (fun acc line ->
          (* add line number information in loc *)
          let loc = Loc.opt_set_line line.Conll.line_num loc in

          List.fold_left
            (fun acc2 (gov, dep_lab) ->
              if gov=0
              then acc
              else
                let gov_node =
                  try Gid_map.find (Gid.Old gov) acc
                  with Not_found ->
                    Error.build ?loc "[G_graph.of_conll] the line refers to unknown gov %d" gov in
                match G_node.add_edge (G_edge.make ?loc dep_lab) (Gid.Old line.Conll.num) gov_node with
                  | None -> acc
                  | Some new_node -> Gid_map.add (Gid.Old gov) new_node acc2
            ) acc line.Conll.deps
        ) nodes lines in

    {meta=[]; map=nodes_with_edges}

  (* -------------------------------------------------------------------------------- *)
  let opt_att atts name =
    try Some (List.assoc name atts)
    with Not_found -> None

  (** [of_xml d_xml] loads a graph in the xml format: [d_xml] must be a <D> xml element *)
  let of_xml d_xml =
    match d_xml with
      | Xml.Element ("D", _, t_or_r_list) ->
        let (t_list, r_list) = List.partition (function Xml.Element ("T",_,_) -> true | _ -> false) t_or_r_list in
        let (nodes_without_edges, mapping) =
          List_.foldi_left
            (fun i (acc, acc_map) t_xml ->
              match t_xml with
                | Xml.Element ("T", t_atts, [Xml.PCData phon]) ->
                  let id = List.assoc "id" t_atts in
                  let other_feats = List.filter (fun (n,_) -> not (List.mem n ["id"; "start"; "end"; "label"])) t_atts in
                  let new_fs =
                    List.fold_left
                      (fun acc2 (fn,fv) -> G_fs.set_feat fn fv acc2)
                      G_fs.empty
                      (("phon", phon) :: ("cat", (List.assoc "label" t_atts)) :: other_feats) in
                  let new_node = G_node.set_fs (G_node.set_pos G_node.empty i) new_fs in
                  (Gid_map.add (Gid.Old i) new_node acc, Str_map.add id (Gid.Old i) acc_map)
                | _ -> Log.critical "[G_graph.of_xml] Not a wellformed <T> tag"
            ) (Gid_map.empty, Str_map.empty) t_list in
        let final_map =
          List.fold_left
            (fun acc r_xml ->
              match r_xml with
                | Xml.Element ("R", r_atts, _) ->
                  let src = List.assoc "from" r_atts
                  and tar = List.assoc "to" r_atts
                  and label = List.assoc "label" r_atts in
                  let gid_tar = Str_map.find tar mapping in
                  let gid_src = Str_map.find src mapping in
                  let old_node = Gid_map.find gid_src acc in
                  let new_map =
                    match G_node.add_edge (G_edge.make label) gid_tar old_node with
                      | Some new_node -> Gid_map.add gid_src new_node acc
                      | None -> Log.critical "[G_graph.of_xml] Fail to add edge" in
                  new_map
                | _ -> Log.critical "[G_graph.of_xml] Not a wellformed <R> tag"
            ) nodes_without_edges r_list in
        {meta=[]; map=final_map}
      | _ -> Log.critical "[G_graph.of_xml] Not a <D> tag"

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Update functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  (* -------------------------------------------------------------------------------- *)
  let rename mapping graph =
    {graph with map = 
        Gid_map.fold
          (fun id node acc ->
            let new_id = try List.assoc id mapping with Not_found -> id in
            let new_node = G_node.rename mapping node in
            Gid_map.add new_id new_node acc
          ) graph.map Gid_map.empty
    }

  (* -------------------------------------------------------------------------------- *)
  let del_edge ?edge_ident loc graph id_src label id_tar =
    let node_src =
      try Gid_map.find id_src graph.map
      with Not_found ->
        match edge_ident with
        | None -> Log.fcritical "[RUN] Some edge refers to a dead node, please report"
        | Some id -> Error.run ~loc "[Graph.del_edge] cannot find source node of edge \"%s\"" id in
    try {graph with map = Gid_map.add id_src (G_node.remove id_tar label node_src) graph.map}
    with Not_found -> Error.run ~loc "[Graph.del_edge] cannot find edge '%s'" (G_edge.to_string label)

  (* -------------------------------------------------------------------------------- *)
  let del_node graph node_id =
    {graph with map = 
        Gid_map.fold
          (fun id value acc ->
	    if id = node_id
	    then acc
	    else Gid_map.add id (G_node.remove_key node_id value) acc
          ) graph.map Gid_map.empty
    }

  (* -------------------------------------------------------------------------------- *)
  let add_neighbour loc graph node_id label =
    let index = match node_id with
      | Gid.Old id ->
        (match Label.to_int label with
          | Some label_int -> Gid.New (id, label_int)
          | None -> Error.run ~loc "[Graph.add_neighbour] try to add neighbour with a local label"
        )
      | Gid.New _ -> Error.run ~loc "[Graph.add_neighbour] try to add neighbour node to a neighbour node" in

    if Gid_map.mem index graph.map
    then Error.run ~loc "[Graph.add_neighbour] try to build twice the \"same\" neighbour node (with label '%s')" (Label.to_string label);

    let node = Gid_map.find node_id graph.map in
    (* put the new node on the right of its "parent" *)
    let new_map = Gid_map.add index (G_node.build_neighbour node) graph.map in

    match map_add_edge new_map node_id label index with
    | Some g -> (index, {graph with map = g})
    | None -> Log.bug "[Graph.add_neighbour] add_edge must not fail"; exit 1

  (* -------------------------------------------------------------------------------- *)
  let shift_in loc graph src_gid tar_gid =
    let tar_node = Gid_map.find tar_gid graph.map in

    if Massoc_gid.mem_key src_gid (G_node.get_next tar_node)
    then Error.run ~loc "[Graph.shift_in] dependency from tar to src";

    { graph with map =
        Gid_map.mapi
          (fun node_id node ->
            match G_node.merge_key src_gid tar_gid node with
              | Some new_node -> new_node
              | None -> Error.run ~loc "[Graph.shift_in] create duplicate edge"
          ) graph.map
    }

  (* -------------------------------------------------------------------------------- *)
  (* move all out-edges from id_src are moved to out-edges out off node id_tar *)
  let shift_out loc graph src_gid tar_gid =
    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    if Massoc_gid.mem_key tar_gid (G_node.get_next src_node)
    then Error.run ~loc "[Graph.shift_out] dependency from src to tar";

    {graph with map =
        Gid_map.mapi
          (fun node_id node ->
	    if node_id = src_gid
	    then (* [src_id] becomes without out-edges *)
              G_node.rm_out_edges node
	    else if node_id = tar_gid
	    then
              match G_node.shift_out src_node tar_node with
                | Some n -> n
                | None -> Error.run ~loc "[Graph.shift_out] common successor"
	    else node (* other nodes don't change *)
          ) graph.map
    }

  (* -------------------------------------------------------------------------------- *)
  let shift_edges loc graph src_gid tar_gid =
    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    if Massoc_gid.mem_key tar_gid (G_node.get_next src_node)
    then Error.run ~loc "[Graph.shift_edges] dependency from src (gid=%s) to tar (gid=%s)"
      (Gid.to_string src_gid) (Gid.to_string tar_gid);

    if Massoc_gid.mem_key src_gid (G_node.get_next tar_node)
    then Error.run ~loc "[Graph.shift_edges] dependency from tar (gid=%s) to src (gid=%s)"
      (Gid.to_string tar_gid) (Gid.to_string src_gid);

    let new_map =
      Gid_map.mapi
        (fun node_id node ->
	  if node_id = src_gid
	  then (* [src_id] becomes an isolated node *)
            G_node.rm_out_edges node
	  else if node_id = tar_gid
	  then
            match G_node.shift_out src_node tar_node with
              | Some n -> n
              | None -> Error.run ~loc "[Graph.shift_edges] common successor"
	  else
            match G_node.merge_key src_gid tar_gid node with
              | Some n -> n
              | None -> Error.run ~loc "[Graph.shift_edges] create duplicate edge"
        ) graph.map in
    { graph with map = new_map }

  (* -------------------------------------------------------------------------------- *)
  let merge_node loc graph src_gid tar_gid =
    let se_graph = shift_edges loc graph src_gid tar_gid in

    let src_node = Gid_map.find src_gid se_graph.map in
    let tar_node = Gid_map.find tar_gid se_graph.map in

    match G_fs.unif (G_node.get_fs src_node) (G_node.get_fs tar_node) with
    | Some new_fs ->
      Some {graph with map =
          (Gid_map.add
	     tar_gid
             (G_node.set_fs tar_node new_fs)
	     (Gid_map.remove src_gid se_graph.map)
          )
           }
    | None -> None

  (* -------------------------------------------------------------------------------- *)
  let set_feat ?loc graph node_id feat_name new_value =
    let node = Gid_map.find node_id graph.map in
    let new_fs = G_fs.set_feat ?loc feat_name new_value (G_node.get_fs node) in
    { graph with map = Gid_map.add node_id (G_node.set_fs node new_fs) graph.map }

  (* -------------------------------------------------------------------------------- *)
  let update_feat ?loc graph tar_id tar_feat_name item_list =
    let strings_to_concat =
      List.map
        (function
          | Concat_item.Feat (node_gid, feat_name) ->
              let node = Gid_map.find node_gid graph.map in
              (match G_fs.get_string_atom feat_name (G_node.get_fs node) with
              | Some atom -> atom
              | None -> Error.run ?loc "Cannot update_feat, some feature (named \"%s\") is not defined" feat_name
              )
          | Concat_item.String s -> s
        ) item_list in
    let new_feature_value = List_.to_string (fun s->s) "" strings_to_concat in
    (set_feat ?loc graph tar_id tar_feat_name new_feature_value, new_feature_value)


  (* -------------------------------------------------------------------------------- *)
  let del_feat graph node_id feat_name =
    let node = Gid_map.find node_id graph.map in
    let new_fs = G_fs.del_feat feat_name (G_node.get_fs node) in
    { graph with map = Gid_map.add node_id (G_node.set_fs node new_fs) graph.map }

  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
  (* Output functions *)
  (* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

  (* -------------------------------------------------------------------------------- *)
  let to_gr graph =
    let buff = Buffer.create 32 in

    bprintf buff "graph {\n";

    (* meta data *)
    List.iter
      (fun (name, value) ->
        bprintf buff "  %s = \"%s\";\n" name value
      ) graph.meta;

    (* nodes *)
    let nodes = Gid_map.fold (fun id node acc -> (id,node)::acc) graph.map [] in
    let sorted_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.pos_comp n1 n2) nodes in
    List.iter
      (fun (id,node) ->
        bprintf buff "  N_%s %s;\n" (Gid.to_string id) (G_node.to_gr node)
      ) sorted_nodes;

    (* edges *)
    Gid_map.iter
      (fun id node ->
	Massoc_gid.iter
	  (fun tar edge ->
	    bprintf buff "  N_%s -[%s]-> N_%s;\n" (Gid.to_string id) (G_edge.to_string edge) (Gid.to_string tar)
	  ) (G_node.get_next node)
      ) graph.map;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let to_sentence ?main_feat graph =
    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.pos_comp n1 n2) nodes in

    let words = List.map
      (fun (id, node) -> G_fs.to_word ?main_feat (G_node.get_fs node)
      ) snodes in
    List.fold_left
      (fun acc (regexp,repl) ->
        Str.global_replace (Str.regexp_string regexp) repl acc
      )
      (String.concat " " words)
      [
        " -t-", "-t-";
        "_-_", "-";
        "_", " ";
        "' ", "'";
        " ,", ",";
        " .", ".";
        "( ", "(";
        " )", ")";
        "\\\"", "\"";
      ]

  (* -------------------------------------------------------------------------------- *)
  let to_dep ?filter ?main_feat ?(deco=G_deco.empty) graph =
    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.pos_comp n1 n2) nodes in

    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { opacity=0; scale = 200; fontname=\"Arial\"; }\n";
    bprintf buff "[WORDS] { \n";

    (* nodes *)
    List.iter
      (fun (id, node) ->
        let fs = G_node.get_fs node in
        let dep_fs = G_fs.to_dep ?filter ?main_feat fs in
        let style = match (List.mem id deco.G_deco.nodes, G_fs.get_string_atom "sem" fs) with
          | (true, _) -> "; forecolor=red; subcolor=red; "
          | (false, Some "void") -> "; forecolor=red; subcolor=red; "
          | _ -> "" in
	bprintf buff "N_%s { %s%s }\n" (Gid.to_string id) dep_fs style
      ) snodes;
    bprintf buff "} \n";

    (* edges *)
    bprintf buff "[EDGES] { \n";
    Gid_map.iter
      (fun gid elt ->
	Massoc_gid.iter
	  (fun tar g_edge ->
	    let deco = List.mem (gid,g_edge,tar) deco.G_deco.edges in
	    bprintf buff "N_%s -> N_%s %s\n" (Gid.to_string gid) (Gid.to_string tar) (G_edge.to_dep ~deco g_edge)
	  ) (G_node.get_next elt)
      ) graph.map;

    bprintf buff "} \n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let to_dot ?main_feat ?(deco=G_deco.empty) graph =
    let buff = Buffer.create 32 in

    bprintf buff "digraph G {\n";
    (* bprintf buff "  rankdir=LR;\n"; *)
    bprintf buff "  node [shape=Mrecord];\n";

    (* nodes *)
    Gid_map.iter
      (fun id node ->
	bprintf buff "  N_%s [label=\"%s\", color=%s]\n"
	  (Gid.to_string id)
          (G_fs.to_dot ?main_feat (G_node.get_fs node))
          (if List.mem id deco.G_deco.nodes then "red" else "black")
      ) graph.map;

    (* edges *)
    Gid_map.iter
      (fun id node ->
	Massoc_gid.iter
	  (fun tar g_edge ->
	    let deco = List.mem (id,g_edge,tar) deco.G_deco.edges in
	    bprintf buff "  N_%s -> N_%s%s\n" (Gid.to_string id) (Gid.to_string tar) (G_edge.to_dot ~deco g_edge)
	  ) (G_node.get_next node)
      ) graph.map;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let to_raw graph =

    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.pos_comp n1 n2) nodes in
    let raw_nodes = List.map (fun (gid,node) -> (gid, G_fs.to_raw (G_node.get_fs node))) snodes in

    let get_num gid = list_num (fun (x,_) -> x=gid) raw_nodes in
    let edge_list = ref [] in
    Gid_map.iter
      (fun src_gid node ->
        Massoc_gid.iter
          (fun tar_gid edge ->
            edge_list := (get_num src_gid, G_edge.to_string edge, get_num tar_gid) :: !edge_list
          )
          (G_node.get_next node)
      )
      graph.map;
    (graph.meta, List.map snd raw_nodes, !edge_list)

  (* -------------------------------------------------------------------------------- *)
  let to_conll graph =

    let nodes = Gid_map.fold (fun gid node acc -> (gid,node)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.pos_comp n1 n2) nodes in
    let get_num gid = (list_num (fun (x,_) -> x=gid) snodes) + 1 in

    (* Warning: [govs_labs] maps [gid]s to [num]s *)
    let govs_labs =
      Gid_map.fold
        (fun src_gid node acc ->
          let src_num = get_num src_gid in
          Massoc_gid.fold
            (fun acc2 tar_gid edge  ->
              let old = try Gid_map.find tar_gid acc2 with Not_found -> [] in
              Gid_map.add tar_gid ((string_of_int src_num, G_edge.to_string edge)::old) acc2
            ) acc (G_node.get_next node)
        ) graph.map Gid_map.empty in

    let buff = Buffer.create 32 in
    Gid_map.iter
      (fun gid node ->
        let (govs,labs) = List.split (try Gid_map.find gid govs_labs with Not_found -> ["0","root"]) in
        let fs = G_node.get_fs node in
        bprintf buff "%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t_\t_\n"
          (get_num gid)
          (match G_fs.get_string_atom "phon" fs with Some p -> p | None -> "NO_PHON")
          (match G_fs.get_string_atom "lemma" fs with Some p -> p | None -> "NO_LEMMA")
          (match G_fs.get_string_atom "cat" fs with Some p -> p | None -> "NO_CAT")
          (match G_fs.get_string_atom "pos" fs with Some p -> p | None -> "_")
          (G_fs.to_conll ~exclude: ["phon"; "lemma"; "cat"; "pos"; "position"] fs)
          (String.concat "|" govs)
          (String.concat "|" labs)
      )
      graph.map;
    Buffer.contents buff

end (* module G_graph *)
(* ================================================================================ *)
