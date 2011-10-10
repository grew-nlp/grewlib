open Printf
open Log

open Utils
open Ast
open Grew_edge
open Grew_fs
open Grew_node
open Command


module Deco = struct
  type t =
      { nodes: int list;
	edges: (int * Label.t * int) list;
      }

  let empty = {nodes=[]; edges=[]}
end

module Graph = struct
  type t = {
      map: Node.t IntMap.t; (* node description *)
      lub: int;             (* least upper bound *)
    }

  let empty = {map = IntMap.empty; lub = 0}

  type gid = int

  type concat_item =
    | Feat of (gid * string)
    | String of string

  let find node_id graph = IntMap.find node_id graph.map

  let map_add_edge map id_src label id_tar =
    let node_src = 
      (* Not found can be raised when adding an edge from pos to neg *)
      try IntMap.find id_src map with Not_found -> Node.empty in
    match Massoc.add id_tar label node_src.Node.next with
    | Some l -> Some (IntMap.add id_src {node_src with Node.next = l} map)
    | None -> None

  (* [add_edge graph id_src label id_tar] tries to add an edge grom [id_src] to [id_tar] with [label] to [graph].
     if it succeeds, [Some new_graph] is returned
     if it fails (the edge already exists), [None] is returned
   *)	  
  let add_edge graph id_src label id_tar =
    match map_add_edge graph.map id_src label id_tar with
    | Some new_map -> Some {graph with map = new_map}
    | None -> None

  let build_filter ?domain table (ast_node, loc) = 
    let pid = Id.build ~loc ast_node.Ast.node_id table in
    let fs = Feature_structure.build ?domain ast_node.Ast.fs in
    (pid, fs)


  let build ?domain ?(locals=[||]) full_node_list full_edge_list = 

    let (named_nodes, constraints) = 
      let rec loop already_bound = function
        | [] -> ([],[])
        | (ast_node, loc) :: tail ->
            let (tail_nodes, tail_const) = loop (ast_node.Ast.node_id :: already_bound) tail in
            if List.mem ast_node.Ast.node_id already_bound
            then (tail_nodes, (ast_node, loc)::tail_const)
            else (Node.build ?domain (ast_node, loc) :: tail_nodes, tail_const) in
      loop [] full_node_list in

    (* let named_nodes = List.map (Node.build ?domain) full_node_list in *)

    let sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Pervasives.compare id1 id2) named_nodes in
    let (sorted_ids, node_list) = List.split sorted_nodes in

    (* table contains the sorted list of node ids *)
    let table = Array.of_list sorted_ids in
    
    (* the nodes, in the same order *) 
    let map_without_edges = List_.foldi_left (fun i acc elt -> IntMap.add i elt acc) IntMap.empty node_list in
    
    let map =
      List.fold_left
	(fun acc (ast_edge, loc) ->
	  let i1 = Id.build ~loc ast_edge.Ast.src table in
	  let i2 = Id.build ~loc ast_edge.Ast.tar table in
	  let edge = Edge.build ~locals (ast_edge, loc) in
	  (match map_add_edge acc i1 edge i2 with
	  | Some g -> g
	  | None -> Log.fcritical "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s" 
                (Edge.to_string edge)
                (Loc.to_string loc)
	  )
	) map_without_edges full_edge_list in
    
    ({map=map;lub=Array.length table}, table, List.map (build_filter ?domain table) constraints)
      
  (* a type for extension of graph: a former graph exists: 
     in grew the former is a positive pattern and an extension is a "without" *)
  type extention = {
      ext_map: Node.t IntMap.t; (* node description for new nodes and for edge "Old -> New"  *)
      old_map: Node.t IntMap.t; (* a partial map for new constraints on old nodes "Old [...]" *) 	
    }

  let build_extention ?domain ?(locals=[||]) old_table full_node_list full_edge_list = 

    let built_nodes = List.map (Node.build ?domain) full_node_list in

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
	(fun i acc elt -> IntMap.add (-i-1) elt acc) 
	IntMap.empty 
	new_node_list in
    
    let old_map_without_edges = 
      List.fold_left 
	(fun acc (id,node) -> IntMap.add (Array_.dicho_find id old_table) node acc) 
	IntMap.empty 
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
	  let edge = Edge.build ~locals (ast_edge, loc) in
	  match map_add_edge acc i1 edge i2 with
	  | Some map -> map
	  | None -> Log.fbug "[GRS] [Graph.build_extention] add_edge cannot fail in pattern extention (1)"; exit 2
	) ext_map_without_edges full_edge_list in
    
    ({ext_map = ext_map_with_all_edges; old_map = old_map_without_edges}, new_table)
      
  (* ---------------------------------------------------------------------------------------------------- *)
  (* Output functions *)
  (* ---------------------------------------------------------------------------------------------------- *)
  let to_gr graph =
    let buff = Buffer.create 32 in
    
    bprintf buff "graph {\n";

    (* list of the nodes *)
    IntMap.iter
      (fun id node ->
	bprintf buff "N%d %s [%s];\n" 
	  id 
          (match node.Node.pos with Some i -> sprintf "(%d)" i | None -> "")
          (Feature_structure.to_gr node.Node.fs)
      ) graph.map;
    (* list of the edges *)
    IntMap.iter
      (fun id node ->
	Massoc.iter
	  (fun tar edge -> 
	    bprintf buff "N%d -[%s]-> N%d;\n" id (Edge.to_string edge) tar
	  ) node.Node.next
      ) graph.map;
    
    bprintf buff "}\n";
    Buffer.contents buff


  let to_dot ?(deco=Deco.empty) graph = 
    let buff = Buffer.create 32 in
    
    bprintf buff "digraph G {\n";

    (* list of the nodes *)
    IntMap.iter
      (fun id node ->
	bprintf buff "N%d[shape=Mrecord, label=\"{%s}\", color=%s]\n" 
	  id (Feature_structure.to_string node.Node.fs) (if List.mem id deco.Deco.nodes then "red" else "black")
      ) graph.map;
    (* list of the edges *)
    IntMap.iter
      (fun id node ->
	Massoc.iter
	  (fun tar edge -> 
	    let deco = List.mem (id,Edge.as_label edge,tar) deco.Deco.edges in
	    bprintf buff "N%d->N%d%s\n" id tar (Edge.to_dot ~deco edge)
	  ) node.Node.next
      ) graph.map;
    
    bprintf buff "}\n";
    Buffer.contents buff

  let to_dep ?main_feat ?(deco=Deco.empty) graph =
    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { opacity=0; scale = 200; fontname=\"Arial\"; }\n";

    bprintf buff "[WORDS] { \n";

    let nodes = IntMap.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> Pervasives.compare n1.Node.pos n2.Node.pos) nodes in

    List.iter
      (fun (id, node) -> 
	if List.mem id deco.Deco.nodes
	then bprintf buff "N%d { %sforecolor=red; subcolor=red; }\n" id (Feature_structure.to_dep ?main_feat node.Node.fs)
	else bprintf buff "N%d { %s }\n" id (Feature_structure.to_dep ?main_feat node.Node.fs)
      ) snodes;
    bprintf buff "} \n";
    
    bprintf buff "[EDGES] { \n";
    IntMap.iter 
      (fun id elt ->
	Massoc.iter
	  (fun tar edge -> 
	    let deco = List.mem (id,Edge.as_label edge,tar) deco.Deco.edges in
	    bprintf buff "N%d -> N%d %s\n" id tar (Edge.to_dep ~deco edge)
	  ) elt.Node.next
      ) graph.map;
      bprintf buff "} \n";
    Buffer.contents buff

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Topology functions *)
  (* ---------------------------------------------------------------------------------------------------- *)
  
  (* [tree_and_roots t] returns:
     - a boolean which is true iff the each node has at most one in-edge
     - the list of "roots" (i.e. nodes without in-edge *)
  let tree_and_roots graph =
    let tree_prop = ref true in
    let not_root =
      IntMap.fold 
	(fun _ node acc -> 
	  Massoc.fold_left 
	    (fun acc2 tar _ ->
	      if !tree_prop
	      then 
		if IntSet.mem tar acc2
		then (tree_prop := false; acc2)
		else IntSet.add tar acc2
	      else IntSet.add tar acc2
	    ) acc node.Node.next 
	) graph.map IntSet.empty in

    let roots =
      IntMap.fold 
	(fun id _ acc -> 
	  if IntSet.mem id not_root
	  then acc 
	  else id::acc
	) graph.map [] in
    
    (!tree_prop, roots)

  let roots graph = snd (tree_and_roots graph)

  (* ---------------------------------------------------------------------------------------------------- *)
  (* Update functions *)
  (* ---------------------------------------------------------------------------------------------------- *)

  (* remove (id_src -[label]-> id_tar) from graph.
     Log.critical if the edge is not in graph *)
  let del_edge ?edge_ident loc graph id_src label id_tar = 
    let node_src = 
      try IntMap.find id_src graph.map 
      with Not_found -> 
        match edge_ident with
        | None -> Log.fcritical "[RUN] Some edge refers to a dead node, please report"
        | Some id -> Error.run ~loc "[Graph.del_edge] cannot find source node of edge \"%s\"" id in
    
    try {graph with map =  
	 IntMap.add id_src {node_src with Node.next = Massoc.remove id_tar label node_src.Node.next} graph.map
       }
    with Not_found -> Error.run ~loc "[Graph.del_edge] cannot find edge '%s'" (Edge.to_string label)

  (* remove node i from graph, with all its incoming and outcoming edges *) 
  (* [graph] is unchanged if the node is not in it *)
  let del_node graph node_id = 
    let new_map = 
      IntMap.fold 
	(fun id value acc ->
	  if id = node_id 
	  then acc
	  else IntMap.add id {value with Node.next = try Massoc.remove_key node_id value.Node.next with Not_found -> value.Node.next} acc
	) graph.map IntMap.empty in
    {graph with map = new_map}

  let add_neighbour loc graph node_id edge = 
    let label = Edge.as_label edge in

    (* index is a new number (higher then lub and uniquely defined by (node_id,label) *)
    let index = graph.lub + ((Grew_edge.Label.to_int label) * graph.lub) + node_id in

    if IntMap.mem index graph.map
    then Error.run ~loc "[Graph.add_neighbour] try to build twice the \"same\" neighbour node (with label '%s')" (Label.to_string label);

    
    let node = IntMap.find node_id graph.map in
    (* put the new node on the right of its "parent" *)
    let new_node = {Node.empty with Node.pos = match node.Node.pos with Some x -> Some (x+1) | None -> None} in
    let new_graph = {graph with map = IntMap.add index new_node graph.map} in  
    match add_edge new_graph node_id (Edge.of_label label) index with
    | Some g -> (index, g)
    | None -> Log.bug "[Graph.add_neighbour] add_edge must not fail"; exit 1



  (* move all in arcs to id_src are moved to in arcs on node id_tar from graph, with all its incoming edges *) 
  let shift_in loc graph src_gid tar_gid =
    let src_node = IntMap.find src_gid graph.map in
    let tar_node = IntMap.find tar_gid graph.map in
    
    if Massoc.mem_key src_gid tar_node.Node.next 
    then Error.run ~loc "[Graph.shift_in] dependency from tar to src";

    let new_map = 
      IntMap.mapi
	(fun node_id node ->
	  try {node with Node.next = Massoc.merge_key src_gid tar_gid node.Node.next}
	  with Massoc.Duplicate -> Error.run ~loc "[Graph.shift_edges] create duplicate edge"
	) graph.map

    in {graph with map = new_map}

  (* move all out-edges from id_src are moved to out-edges out off node id_tar *) 
  let shift_out loc graph src_gid tar_gid =
    let src_node = IntMap.find src_gid graph.map in
    let tar_node = IntMap.find tar_gid graph.map in
    
    if Massoc.mem_key tar_gid src_node.Node.next 
    then Error.run ~loc "[Graph.shift_edges] dependency from src to tar";

    let new_map = 
      IntMap.mapi
	(fun node_id node ->
	  if node_id = src_gid 
	  then (* [src_id] becomes without out-edges *) 
	    {node with Node.next = Massoc.empty}
	  else if node_id = tar_gid 
	  then
	    try {node with Node.next = Massoc.disjoint_union src_node.Node.next tar_node.Node.next}
	    with Massoc.Not_disjoint -> Error.run ~loc "[Graph.shift_edges] common successor"
	  else node (* other nodes don't change *)
	) graph.map
    in {graph with map = new_map}

  (* move all incident arcs from/to id_src are moved to incident arcs on node id_tar from graph, with all its incoming and outcoming edges *) 
  let shift_edges loc graph src_gid tar_gid =
    let src_node = IntMap.find src_gid graph.map in
    let tar_node = IntMap.find tar_gid graph.map in
    
    if Massoc.mem_key tar_gid src_node.Node.next 
    then Error.run ~loc "[Graph.shift_edges] dependency from src to tar";

    if Massoc.mem_key src_gid tar_node.Node.next 
    then Error.run ~loc "[Graph.shift_edges] dependency from tar to src";

    let new_map = 
      IntMap.mapi
	(fun node_id node ->
	  if node_id = src_gid 
	  then (* [src_id] becomes an isolated node *) 
	    {node with Node.next = Massoc.empty}
	  else if node_id = tar_gid 
	  then
	    try {node with Node.next = Massoc.disjoint_union src_node.Node.next tar_node.Node.next}
	    with Massoc.Not_disjoint -> Error.run ~loc "[Graph.shift_edges] common successor"
	  else 
	    try {node with Node.next = Massoc.merge_key src_gid tar_gid node.Node.next}
	    with Massoc.Duplicate -> Error.run ~loc "[Graph.shift_edges] create duplicate edge"
	) graph.map

    in {graph with map = new_map}















  let merge_node loc graph src_gid tar_gid =
    let se_graph = shift_edges loc graph src_gid tar_gid in

    let src_node = IntMap.find src_gid se_graph.map in
    let tar_node = IntMap.find tar_gid se_graph.map in
    
    match Feature_structure.unif src_node.Node.fs tar_node.Node.fs with
    | Some new_fs -> 
	let new_map =
	  IntMap.add
	    tar_gid 
	    {tar_node with Node.fs = new_fs}
	    (IntMap.remove src_gid se_graph.map) in
	Some {se_graph with map = new_map}
    | None -> None 

  (* FIXME: check consistency wrt the domain *)      
  let update_feat graph tar_id tar_feat_name item_list =
    let tar = IntMap.find tar_id graph.map in
    let strings_to_concat =
      List.map
        (function
          | Feat (node_gid, feat_name) ->
              let node = IntMap.find node_gid graph.map in
              (try 
                match Feature_structure.get_atom feat_name node.Node.fs with
                | Some atom -> atom
                | None -> Log.fcritical "[BUG] [Graph.update_feat] Feature not atomic"
              with Not_found -> 
                Log.fcritical "[RUN] [Graph.update_feat] no feature \"%s\" in node \"%s\"" 
                  feat_name (Node.to_string node))
          | String s -> s
        ) item_list in
    let new_feature_value = List_.to_string (fun s->s) "" strings_to_concat in
    let new_f = Feature_structure.set_feat tar_feat_name [new_feature_value] tar.Node.fs in
    ({graph with map = IntMap.add tar_id {tar with Node.fs = new_f} graph.map}, new_feature_value)
      
      (** [del_feat graph node_id feat_name] returns [graph] where the feat [feat_name] of [node_id] is deleted
	  If the feature is not present, [graph] is returned. *)
  let del_feat graph node_id feat_name = 
    let node =  IntMap.find node_id graph.map in
    let new_fs = Feature_structure.del_feat feat_name node.Node.fs in
    {graph with map = IntMap.add node_id {node with Node.fs = new_fs} graph.map}

  let equals t t' = IntMap.equal (fun node1 node2 -> node1 = node2) t.map t'.map

  (* is there an edge e out of node i ? *)
  let edge_out graph node_id edge = 
    let node = IntMap.find node_id graph.map in
    Massoc.exists (fun _ e -> Edge.compatible e edge) node.Node.next
  	
end (* module Graph *)

