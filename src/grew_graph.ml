open Printf
open Log

open Grew_utils
open Grew_ast
open Grew_edge
open Grew_fs
open Grew_node
open Grew_command


(* ================================================================================ *)
module P_deco = struct
  type t =
      { nodes: Pid.t list;
	edges: (Pid.t * P_edge.t * Pid.t) list;
      }

  let empty = {nodes=[]; edges=[]}
end
(* ================================================================================ *)

(* ================================================================================ *)
module G_deco = struct
  type t =
      { nodes: Gid.t list;
	edges: (Gid.t * G_edge.t * Gid.t) list;
      }

  let empty = {nodes=[]; edges=[]}
end
(* ================================================================================ *)

(* ================================================================================ *)
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

  let build_filter table (ast_node, loc) =
    let pid = Id.build ~loc ast_node.Ast.node_id table in
    let fs = P_fs.build ast_node.Ast.fs in
    (pid, fs)

  let build ?pat_vars ?(locals=[||]) (full_node_list : Ast.node list) full_edge_list =
    (* let (named_nodes, constraints) =  *)
    (*   let rec loop already_bound = function *)
    (*     | [] -> ([],[]) *)
    (*     | (ast_node, loc) :: tail -> *)
    (*         let (tail_nodes, tail_const) = loop (ast_node.Ast.node_id :: already_bound) tail in *)
    (*         if List.mem ast_node.Ast.node_id already_bound *)
    (*         then (tail_nodes, (ast_node, loc)::tail_const) *)
    (*         else (P_node.build ?pat_vars (ast_node, loc) :: tail_nodes, tail_const) in *)
    (*   loop [] full_node_list in *)

    let rec insert (ast_node, loc) = function
      | [] -> [P_node.build ?pat_vars (ast_node, loc)]
      | (n,h)::t when ast_node.Ast.node_id = n ->
          (n, P_node.unif_fs (P_fs.build ?pat_vars ast_node.Ast.fs) h) :: t
      | h::t -> h :: (insert (ast_node, loc) t) in

    let (named_nodes : (Id.name * P_node.t) list) =
      let rec loop = function
        | [] -> []
        | ast_node :: tail ->
            let tail_nodes = loop tail in
            insert ast_node tail_nodes in
            (* let old_node = List.find (fun n -> P_node.get_name) *)
            (* if List.mem ast_node.Ast.node_id already_bound *)
            (* then (tail_nodes, (ast_node, loc)::tail_const) *)
            (* else (P_node.build ?pat_vars (ast_node, loc) :: tail_nodes, tail_const) in *)
      loop full_node_list in

    let sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Pervasives.compare id1 id2) named_nodes in
    let (sorted_ids, node_list) = List.split sorted_nodes in

    (* table contains the sorted list of node ids *)
    let table = Array.of_list sorted_ids in

    (* the nodes, in the same order *)
    let map_without_edges = List_.foldi_left (fun i acc elt -> Pid_map.add i elt acc) Pid_map.empty node_list in

    let (map : t) =
      List.fold_left
	(fun acc (ast_edge, loc) ->
	  let i1 = Id.build ~loc ast_edge.Ast.src table in
	  let i2 = Id.build ~loc ast_edge.Ast.tar table in
	  let edge = P_edge.build ~locals (ast_edge, loc) in
	  (match map_add_edge acc i1 edge i2 with
	  | Some g -> g
	  | None -> Error.build "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s"
                (P_edge.to_string edge)
                (Loc.to_string loc)
	  )
	) map_without_edges full_edge_list in
    (map, table, [](* List.map (build_filter table) constraints *))



  (* a type for extension of graph: a former graph exists:
     in grew the former is a positive pattern and an extension is a "without" *)
  type extension = {
      ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
      old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *) 	
    }

  let build_extension ?(locals=[||]) old_table full_node_list full_edge_list =

    let built_nodes = List.map P_node.build full_node_list in

    let (old_nodes, new_nodes) =
      List.partition
        (function (id,_) when Array_.dicho_mem id old_table -> true | _ -> false)
        built_nodes in
	
    let new_sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Pervasives.compare id1 id2) new_nodes in

    let (new_sorted_ids, new_node_list) = List.split new_sorted_nodes in

    (* table contains the sorted list of node ids *)
    let new_table = Array.of_list new_sorted_ids in

    (* the nodes, in the same order stored with index -1, -2, ... -N *)
    let ext_map_without_edges =
      List_.foldi_left
	(fun i acc elt -> Pid_map.add (-i-1) elt acc)
	Pid_map.empty
	new_node_list in

    let old_map_without_edges =
      List.fold_left
	(fun acc (id,node) -> Pid_map.add (Array_.dicho_find id old_table) node acc)
	Pid_map.empty
	old_nodes in

    let ext_map_with_all_edges =
      List.fold_left
	(fun acc (ast_edge, loc) ->
	  let i1 =
	    match Id.build_opt ast_edge.Ast.src old_table
	    with Some i -> i | None -> -1-(Id.build ~loc ast_edge.Ast.src new_table) in
	  let i2 =
	    match Id.build_opt ast_edge.Ast.tar old_table
	    with Some i -> i | None -> -1-(Id.build ~loc ast_edge.Ast.tar new_table) in
	  let edge = P_edge.build ~locals (ast_edge, loc) in
	  match map_add_edge acc i1 edge i2 with
	  | Some map -> map
	  | None -> Log.fbug "[GRS] [Graph.build_extension] add_edge cannot fail in pattern extension (1)"; exit 2
	) ext_map_without_edges full_edge_list in

    ({ext_map = ext_map_with_all_edges; old_map = old_map_without_edges}, new_table)

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Topology functions *)
  (* ---------------------------------------------------------------------------------------------------- *)

  (* [tree_and_roots t] returns:
     - a boolean which is true iff the each node has at most one in-edge
     - the list of "roots" (i.e. nodes without in-edge *)
  let tree_and_roots graph =
    let tree_prop = ref true in
    let not_root =
      Pid_map.fold
	(fun _ node acc ->
	  Massoc.fold_left
	    (fun acc2 tar _ ->
	      if !tree_prop
	      then
		if IntSet.mem tar acc2
		then (tree_prop := false; acc2)
		else IntSet.add tar acc2
	      else IntSet.add tar acc2
	    ) acc (P_node.get_next node)
	) graph IntSet.empty in

    let roots =
      Pid_map.fold
	(fun id _ acc ->
	  if IntSet.mem id not_root
	  then acc
	  else id::acc
	) graph [] in

    (!tree_prop, roots)

  let roots graph = snd (tree_and_roots graph)
end (* module P_graph *)
(* ================================================================================ *)



(* ================================================================================ *)
module G_graph = struct
  type t = {
      map: G_node.t Gid_map.t; (* node description *)
      lub: int;                (* least upper bound *)
    }

  let empty = {map = Gid_map.empty; lub = 0}

  let find node_id graph = Gid_map.find node_id graph.map

  type concat_item =
    | Feat of (Gid.t * string)
    | String of string

  let map_add_edge map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Gid_map.find id_src map with Not_found -> G_node.empty in
    match G_node.add_edge label id_tar node_src with
    | None -> None
    | Some new_node -> Some (Gid_map.add id_src new_node map)

  let build ?(locals=[||]) full_node_list full_edge_list =

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

    {map=map;lub=Array.length table}


  let of_conll ?loc lines =

    let nodes =
      List.fold_left
        (fun acc line ->
          Gid_map.add (Gid.Old line.Conll.num) (G_node.of_conll line) acc)
        Gid_map.empty lines in

    let nodes_with_edges =
      List.fold_left
        (fun acc line ->
          (* add line number information in loc *)
          let loc = Loc.opt_set_line line.Conll.line_num loc in

          if line.Conll.gov=0
          then acc
          else
            let gov_node =
              try Gid_map.find (Gid.Old line.Conll.gov) acc
              with Not_found ->
                Error.build ?loc "[G_graph.of_conll] the line refers to unknown gov %d" line.Conll.gov in
            match G_node.add_edge (G_edge.make ?loc line.Conll.dep_lab) (Gid.Old line.Conll.num) gov_node with
            | None -> acc
            | Some new_node -> Gid_map.add (Gid.Old line.Conll.gov) new_node acc
        ) nodes lines in

        {map = nodes_with_edges; lub=1+(Gid_map.fold (fun _ _ acc -> acc+1) nodes_with_edges 0)}


  (* ---------------------------------------------------- *)
  (* Update functions *)
  (* ---------------------------------------------------- *)



  (* [add_edge graph id_src label id_tar] tries to add an edge grom [id_src] to [id_tar] with [label] to [graph].
     if it succeeds, [Some new_graph] is returned
     if it fails (the edge already exists), [None] is returned
   *)	
  let add_edge graph id_src label id_tar =
    match map_add_edge graph.map id_src label id_tar with
    | Some new_map -> Some {graph with map = new_map}
    | None -> None


  (* remove (id_src -[label]-> id_tar) from graph.
     Log.critical if the edge is not in graph *)
  let del_edge ?edge_ident loc graph id_src label id_tar =
    let node_src =
      try Gid_map.find id_src graph.map
      with Not_found ->
        match edge_ident with
        | None -> Log.fcritical "[RUN] Some edge refers to a dead node, please report"
        | Some id -> Error.run ~loc "[Graph.del_edge] cannot find source node of edge \"%s\"" id in

    try {graph with map =
	 (* Gid_map.add id_src {node_src with Node.next = Massoc.remove id_tar label node_src.Node.next} graph.map *)
	 Gid_map.add id_src (G_node.remove id_tar label node_src) graph.map
       }
    with Not_found -> Error.run ~loc "[Graph.del_edge] cannot find edge '%s'" (G_edge.to_string label)

  (* remove node i from graph, with all its incoming and outcoming edges *)
  (* [graph] is unchanged if the node is not in it *)
  let del_node graph node_id =
    let new_map =
      Gid_map.fold
	(fun id value acc ->
	  if id = node_id
	  then acc
	  (* else Gid_map.add id {value with Node.next = try Massoc.remove_key node_id value.Node.next with Not_found -> value.Node.next} acc *)
	  else Gid_map.add id (G_node.remove_key node_id value) acc
	) graph.map Gid_map.empty in
    {graph with map = new_map}

  let add_neighbour loc graph node_id label =

    (* index is a new number (higher then lub and uniquely defined by (node_id,label) *)
    (* let index = graph.lub + ((Label.to_int label) * graph.lub) + node_id in *)

    let index = match node_id with
      | Gid.Old id -> Gid.New (id, Label.to_int label)
      | Gid.New _ -> Error.run ~loc "[Graph.add_neighbour] try to add neighbour node to a neighbour node" in

    if Gid_map.mem index graph.map
    then Error.run ~loc "[Graph.add_neighbour] try to build twice the \"same\" neighbour node (with label '%s')" (Label.to_string label);

    let node = Gid_map.find node_id graph.map in
    (* put the new node on the right of its "parent" *)
    let new_graph = {graph with map = Gid_map.add index (G_node.build_neighbour node) graph.map} in
    match add_edge new_graph node_id label index with
    | Some g -> (index, g)
    | None -> Log.bug "[Graph.add_neighbour] add_edge must not fail"; exit 1



  (* move all in arcs to id_src are moved to in arcs on node id_tar from graph, with all its incoming edges *)
  let shift_in loc graph src_gid tar_gid =
    let tar_node = Gid_map.find tar_gid graph.map in

    if Massoc_gid.mem_key src_gid (G_node.get_next tar_node)
    then Error.run ~loc "[Graph.shift_in] dependency from tar to src";

    let new_map =
      Gid_map.mapi
	(fun node_id node ->
          match G_node.merge_key src_gid tar_gid node with
          | Some new_node -> new_node
          | None -> Error.run ~loc "[Graph.shift_in] create duplicate edge"
	) graph.map

    in {graph with map = new_map}

  (* move all out-edges from id_src are moved to out-edges out off node id_tar *)
  let shift_out loc graph src_gid tar_gid =
    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    if Massoc_gid.mem_key tar_gid (G_node.get_next src_node)
    then Error.run ~loc "[Graph.shift_out] dependency from src to tar";

    let new_map =
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
    in {graph with map = new_map}

  (* move all incident arcs from/to id_src are moved to incident arcs on node id_tar from graph, with all its incoming and outcoming edges *)
  let shift_edges loc graph src_gid tar_gid =
    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    if Massoc_gid.mem_key tar_gid (G_node.get_next src_node)
    then Error.run ~loc "[Graph.shift_edges] dependency from src to tar";

    if Massoc_gid.mem_key src_gid (G_node.get_next tar_node)
    then Error.run ~loc "[Graph.shift_edges] dependency from tar to src";

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
	) graph.map

    in {graph with map = new_map}

  let merge_node loc graph src_gid tar_gid =
    let se_graph = shift_edges loc graph src_gid tar_gid in

    let src_node = Gid_map.find src_gid se_graph.map in
    let tar_node = Gid_map.find tar_gid se_graph.map in

    match G_fs.unif (G_node.get_fs src_node) (G_node.get_fs tar_node) with
    | Some new_fs ->
	let new_map =
	  Gid_map.add
	    tar_gid
            (G_node.set_fs tar_node new_fs)
	    (Gid_map.remove src_gid se_graph.map) in
	Some {se_graph with map = new_map}
    | None -> None

  let set_feat ?loc graph node_id feat_name new_value =
    let node = Gid_map.find node_id graph.map in
    let new_fs = G_fs.set_feat ?loc feat_name new_value (G_node.get_fs node) in
    {graph with map = Gid_map.add node_id (G_node.set_fs node new_fs) graph.map}

  let update_feat ?loc graph tar_id tar_feat_name item_list =
    let strings_to_concat =
      List.map
        (function
          | Feat (node_gid, feat_name) ->
              let node = Gid_map.find node_gid graph.map in
              (match G_fs.get_atom feat_name (G_node.get_fs node) with
              | Some atom -> atom
              | None -> Error.run ?loc "Some feature (named \"%s\") is not defined" feat_name
              )
          | String s -> s
        ) item_list in
    let new_feature_value = List_.to_string (fun s->s) "" strings_to_concat in
    (set_feat ?loc graph tar_id tar_feat_name new_feature_value, new_feature_value)



      (** [del_feat graph node_id feat_name] returns [graph] where the feat [feat_name] of [node_id] is deleted
	  If the feature is not present, [graph] is returned. *)
  let del_feat graph node_id feat_name =
    let node =  Gid_map.find node_id graph.map in
    let new_fs = G_fs.del_feat feat_name (G_node.get_fs node) in
    {graph with map = Gid_map.add node_id (* {node with Node.fs = new_fs} *) (G_node.set_fs node new_fs) graph.map}

  let to_gr graph =
    let buff = Buffer.create 32 in

    bprintf buff "graph {\n";

    (* list of the nodes *)
    Gid_map.iter
      (fun id node ->
        bprintf buff "N_%s %s;\n" (Gid.to_string id) (G_node.to_gr node)
      ) graph.map;
    (* list of the edges *)
    Gid_map.iter
      (fun id node ->
	Massoc_gid.iter
	  (fun tar edge ->
	    bprintf buff "N_%s -[%s]-> N_%s;\n" (Gid.to_string id) (G_edge.to_string edge) (Gid.to_string tar)
	  ) (G_node.get_next node)
      ) graph.map;

    bprintf buff "}\n";
    Buffer.contents buff

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

  let to_dep ?main_feat ?(deco=G_deco.empty) graph =
    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { opacity=0; scale = 200; fontname=\"Arial\"; }\n";

    bprintf buff "[WORDS] { \n";

    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.pos_comp n1 n2) nodes in

    List.iter
      (fun (id, node) ->
	if List.mem id deco.G_deco.nodes
	then bprintf buff
            "N_%s { %sforecolor=red; subcolor=red; }\n" (Gid.to_string id) (G_fs.to_dep ?main_feat (G_node.get_fs node))
	else bprintf buff
            "N_%s { %s }\n" (Gid.to_string id) (G_fs.to_dep ?main_feat (G_node.get_fs node))
      ) snodes;
    bprintf buff "} \n";

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

  let to_dot ?main_feat ?(deco=G_deco.empty) graph =
    let buff = Buffer.create 32 in

    bprintf buff "digraph G {\n";
    (* bprintf buff "  rankdir=LR;\n"; *)
    bprintf buff "  node [shape=Mrecord];\n";

    (* list of the nodes *)
    Gid_map.iter
      (fun id node ->
	bprintf buff "  N_%s [label=\"%s\", color=%s]\n"
	  (Gid.to_string id)
          (G_fs.to_dot ?main_feat (G_node.get_fs node))
          (if List.mem id deco.G_deco.nodes then "red" else "black")
      ) graph.map;
    (* list of the edges *)
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

  let equals t t' = Gid_map.equal (fun node1 node2 -> node1 = node2) t.map t'.map

  (* is there an edge e out of node i ? *)
  let edge_out graph node_id p_edge =
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> P_edge.compatible p_edge e) (G_node.get_next node)

end (* module G_graph *)
(* ================================================================================ *)
