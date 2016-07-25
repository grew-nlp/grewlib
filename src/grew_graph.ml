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
open Conll

open Grew_base
open Grew_ast
open Grew_types

open Grew_edge
open Grew_fs
open Grew_node

(* ================================================================================ *)
module P_deco = struct
  type t = {
    nodes: Pid.t list;
    edges: (Pid.t * P_edge.t * Pid.t) list;
  }

  let empty = {nodes=[]; edges=[]}
end (* module P_deco *)

(* ================================================================================ *)
module P_graph = struct
  type t = P_node.t Pid_map.t

  let empty = Pid_map.empty

  let find = Pid_map.find

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Pid_map.find id_src map with Not_found -> P_node.empty in
    match P_node.add_edge label id_tar node_src with
      | None -> None
      | Some new_node -> Some (Pid_map.add id_src new_node map)

  (* -------------------------------------------------------------------------------- *)
  let build_filter domain table (ast_node, loc) =
    let pid = Id.build ~loc ast_node.Ast.node_id table in
    let fs = P_fs.build domain ast_node.Ast.fs in
    (pid, fs)

  (* -------------------------------------------------------------------------------- *)
  let build domain ?pat_vars ?(locals=[||]) (full_node_list : Ast.node list) full_edge_list =

    (* NB: insert searches for a previous node with the Same name and uses unification rather than constraint *)
    (* NB: insertion of new node at the end of the list: not efficient but graph building is not the hard part. *)
    let rec insert (ast_node, loc) = function
      | [] -> [P_node.build domain ?pat_vars (ast_node, loc)]
      | (node_id,fs)::tail when ast_node.Ast.node_id = node_id ->
        begin
          try (node_id, P_node.unif_fs (P_fs.build domain ?pat_vars ast_node.Ast.fs) fs) :: tail
          with Error.Build (msg,_) -> raise (Error.Build (msg,Some loc))
        end
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
          let edge = P_edge.build domain (ast_edge, loc) in
          (match map_add_edge acc (Pid.Pos i1) edge (Pid.Pos i2) with
            | Some g -> g
            | None -> Error.build "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s"
              (P_edge.to_string domain edge)
              (Loc.to_string loc)
          )
        ) map_without_edges full_edge_list in
    (map, pos_table)


  (* -------------------------------------------------------------------------------- *)
  (* a type for extension of graph (a former graph exists):
     in grew the former is a positive basic and an extension is a negative basic ("without") *)
  type extension = {
    ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
    old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *)
  }

  (* -------------------------------------------------------------------------------- *)
  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_extension domain ?pat_vars ?(locals=[||]) pos_table full_node_list full_edge_list =

    let built_nodes = List.map (P_node.build domain ?pat_vars) full_node_list in

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
        (fun acc (id,node) ->
          let pid_pos = Pid.Pos (Array_.dicho_find id pos_table) in
          try
            let old = Pid_map.find pid_pos acc in
            Pid_map.add pid_pos (P_node.unif_fs (P_node.get_fs node) old) acc
          with Not_found -> Pid_map.add pid_pos node acc
        ) Pid_map.empty old_nodes in

    let ext_map_with_all_edges =
      List.fold_left
        (fun acc (ast_edge, loc) ->
          let src = ast_edge.Ast.src
          and tar = ast_edge.Ast.tar in
          let i1 =
            match Id.build_opt src pos_table with
              | Some i -> Pid.Pos i
              | None -> Pid.Neg (Id.build ~loc src new_table) in
          let i2 =
            match Id.build_opt tar pos_table with
              | Some i -> Pid.Pos i
              | None -> Pid.Neg (Id.build ~loc tar new_table) in
          let edge = P_edge.build domain (ast_edge, loc) in
          match map_add_edge acc i1 edge i2 with
            | Some map -> map
            | None -> Log.fbug "[GRS] [Graph.build_extension] add_edge cannot fail in pattern extension"; exit 2
        ) ext_map_without_edges full_edge_list in
    ({ext_map = ext_map_with_all_edges; old_map = old_map_without_edges}, new_table)

  (* -------------------------------------------------------------------------------- *)
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

  (* -------------------------------------------------------------------------------- *)
  let roots graph = snd (tree_and_roots graph)
end (* module P_graph *)

(* ================================================================================ *)
module G_deco = struct
  type t = {
    nodes: (Gid.t * (string * string list)) list;  (* a list of (node, (pattern_id, features of nodes implied in the step)) *)
    edges: (Gid.t * G_edge.t * Gid.t) list;        (* an edge list *)
  }

  let empty = {nodes=[]; edges=[]}
end (* module G_deco *)

(* ================================================================================ *)
module G_graph = struct
  type t = {
    meta: string list;                       (* meta-informations *)
    map: G_node.t Gid_map.t;                 (* node description *)
    fusion: (Gid.t * (Gid.t * string)) list; (* the list of fusion word considered in UD conll *)
    highest_index: int;                      (* the next free interger index *)
  }

  let empty = {meta=[]; map=Gid_map.empty; fusion=[]; highest_index=0; }

  (* ---------------------------------------------------------------------- *)
  let rename mapping graph =
    {graph with map =
        Gid_map.fold
          (fun id node acc ->
            let new_id = try List.assoc id mapping with Not_found -> id in
            let new_node = G_node.rename mapping node in
            Gid_map.add new_id new_node acc
          ) graph.map Gid_map.empty
    }

  let get_highest g = g.highest_index

  let find node_id graph = Gid_map.find node_id graph.map

  let equals t t' = Gid_map.equal (fun node1 node2 -> node1 = node2) t.map t'.map

  let node_exists fct t = Gid_map.exists (fun _ node -> fct node) t.map

  let fold_gid fct t init =
    Gid_map.fold (fun gid _ acc -> fct gid acc) t.map init

  (* is there an edge e out of node i ? *)
  let edge_out domain graph node_id label_cst =
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> Label_cst.match_ domain label_cst e) (G_node.get_next node)

  let get_annot_info graph =
    let annot_info =
      Gid_map.fold
        (fun _ node acc ->
          match (G_node.get_annot_info node, acc) with
            | (None,_) -> acc
            | (Some f, None) -> Some (f,G_node.get_position node)
            | (Some _, Some _) -> Error.build "[G_node.get_annot_info] Two nodes with annot info"
        ) graph.map None in
    match annot_info with
      | Some x -> x
      | None -> Error.build "[G_node.get_annot_info] No nodes with annot info"

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Gid_map.find id_src map with Not_found -> G_node.empty in
    match G_node.add_edge label id_tar node_src with
      | None -> None
      | Some new_node -> Some (Gid_map.add id_src new_node map)

  (* -------------------------------------------------------------------------------- *)
  let add_edge graph id_src label id_tar =
    match map_add_edge graph.map id_src label id_tar with
      | Some new_map -> Some {graph with map = new_map }
      | None -> None

  (* -------------------------------------------------------------------------------- *)
  let build domain ?(locals=[||]) gr_ast =
    let full_node_list = gr_ast.Ast.nodes
    and full_edge_list = gr_ast.Ast.edges in

    let rec loop already_bound index prec = function
      | [] -> (Gid_map.empty,[])

      | (ast_node, loc)::tail ->
        let node_id = ast_node.Ast.node_id in
        if List.mem node_id already_bound
        then Error.build ~loc "[GRS] [Graph.build] try to build a graph with twice the same node id '%s'" node_id
        else
          let (new_tail, table) = loop (node_id :: already_bound) (index+1) (Some index) tail in
          let succ = if tail = [] then None else Some (index+1) in
          let (_,new_node) = G_node.build domain ?prec ?succ index (ast_node, loc) in
            (
              Gid_map.add index new_node new_tail,
              (node_id,index)::table
            ) in

    let (map_without_edges, table) = loop [] 0 None full_node_list in

    let map =
      List.fold_left
        (fun acc (ast_edge, loc) ->
          let i1 = List.assoc ast_edge.Ast.src table in
          let i2 = List.assoc ast_edge.Ast.tar table in
          let edge = G_edge.build domain (ast_edge, loc) in
          (match map_add_edge acc i1 edge i2 with
            | Some g -> g
            | None -> Error.build "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s"
              (G_edge.to_string domain edge)
              (Loc.to_string loc)
          )
        ) map_without_edges full_edge_list in

    {meta=gr_ast.Ast.meta; map=map; fusion = []; highest_index = (List.length full_node_list) -1}

  (* -------------------------------------------------------------------------------- *)
  let of_conll domain conll =

    let sorted_lines = Conll.root :: (List.sort Conll.compare conll.Conll.lines) in

    let gtable = (Array.of_list (List.map (fun line -> line.Conll.id) sorted_lines), string_of_int) in

    let rec loop index prec = function
      | [] -> Gid_map.empty
      | [last] ->
        let loc = Loc.file_opt_line conll.Conll.file last.Conll.line_num in
        Gid_map.add index (G_node.of_conll domain ~loc ?prec last) Gid_map.empty
      | line::tail ->
        let loc = Loc.file_opt_line conll.Conll.file line.Conll.line_num in
        Gid_map.add index (G_node.of_conll domain ~loc ?prec ~succ:(index+1) line)
          (loop (index+1) (Some index) tail) in

    let map_without_edges = loop 0 None sorted_lines in

    let map_with_edges =
      List.fold_left
        (fun acc line ->
          let loc = Loc.file_opt_line conll.Conll.file line.Conll.line_num in
          let dep_id = Id.gbuild ~loc line.Conll.id gtable in
          List.fold_left
            (fun acc2 (gov, dep_lab) ->
              let gov_id = Id.gbuild ~loc gov gtable in
              let edge = G_edge.make domain ~loc dep_lab in
              (match map_add_edge acc2 gov_id edge dep_id with
                | Some g -> g
                | None -> Error.build "[GRS] [Graph.of_conll] try to build a graph with twice the same edge %s %s"
                  (G_edge.to_string domain edge)
                  (Loc.to_string loc)
              )
            ) acc line.Conll.deps
        ) map_without_edges conll.Conll.lines in

      let fusion =
        List.map
          (fun {Conll.first; last; fusion; mw_line_num} ->
              let loc = Loc.file_opt_line conll.Conll.file mw_line_num in
              (
                Id.gbuild ~loc first gtable,
                (
                  Id.gbuild ~loc last gtable,
                  fusion
                )
              )
          ) conll.Conll.multiwords in

    {meta = conll.Conll.meta; map=map_with_edges; fusion; highest_index= (List.length sorted_lines) -1 }

  (* -------------------------------------------------------------------------------- *)
  (** input : "Le/DET/le petit/ADJ/petit chat/NC/chat dort/V/dormir ./PONCT/." *)
  let of_brown domain ?sentid brown =
    let units = Str.split (Str.regexp " ") brown in
      let conll_lines = List.mapi
      (fun i item -> match Str.full_split (Str.regexp "/[A-Z'+'']+/") item with
        | [Str.Text form; Str.Delim pos; Str.Text lemma] ->
        let pos = String.sub pos 1 ((String.length pos)-2) in
        let feats = match (i,sentid) with
          | (0,Some id) -> [("sentid", id)]
          | _ -> [] in
        Conll.build_line ~id:(i+1) ~form ~lemma ~xpos:pos ~feats ~deps:([(i, "SUC")]) ()
        | _ -> Error.build "[Graph.of_brown] Cannot parse Brown item >>>%s<<< (expected \"phon/POS/lemma\") in >>>%s<<<" item brown
      ) units in 
    of_conll domain { Conll.file=None; meta=[]; lines=conll_lines; multiwords=[] }

  (* -------------------------------------------------------------------------------- *)
  let opt_att atts name =
    try Some (List.assoc name atts)
    with Not_found -> None

  (* -------------------------------------------------------------------------------- *)
  (** [of_xml d_xml] loads a graph in the xml format: [d_xml] must be a <D> xml element *)
  let of_xml domain d_xml = failwith "of_xml not available"

  (* -------------------------------------------------------------------------------- *)
  let del_edge domain ?edge_ident loc graph id_src label id_tar =
    let node_src =
      try Gid_map.find id_src graph.map
      with Not_found ->
        match edge_ident with
          | None -> Log.fcritical "[RUN] Some edge refers to a dead node, please report"
          | Some id -> Error.run ~loc "[Graph.del_edge] cannot find source node of edge \"%s\"" id in
    try {graph with map = Gid_map.add id_src (G_node.remove id_tar label node_src) graph.map}
    with Not_found -> Error.run ~loc "[Graph.del_edge] cannot find edge '%s'" (G_edge.to_string domain label)

  (* -------------------------------------------------------------------------------- *)
  let del_node graph node_id =
    let map_wo_node =
      Gid_map.fold
        (fun id value acc ->
          if id = node_id
          then acc
          else Gid_map.add id (G_node.remove_key node_id value) acc
        ) graph.map Gid_map.empty in
    let node = Gid_map.find node_id graph.map in
    let new_map =
      match (G_node.get_prec node, G_node.get_succ node) with
      | (Some id_prec, Some id_succ) ->
        begin
          let prec = Gid_map.find id_prec map_wo_node
          and succ = Gid_map.find id_succ map_wo_node in
          map_wo_node
          |> (Gid_map.add id_prec (G_node.set_succ id_succ prec))
          |> (Gid_map.add id_succ (G_node.set_prec id_prec succ))
        end
      | (Some id_prec, None) ->
        begin
          let prec = Gid_map.find id_prec map_wo_node in
          map_wo_node
          |> (Gid_map.add id_prec (G_node.remove_succ prec))
        end
      | (None, Some id_succ) ->
        begin
          let succ = Gid_map.find id_succ map_wo_node in
          map_wo_node
          |> (Gid_map.add id_succ (G_node.remove_prec succ))
        end
      | (None, None) -> map_wo_node in
    { graph with map = new_map }

  (* -------------------------------------------------------------------------------- *)
  let add_neighbour loc domain graph node_id label = failwith "no more add_neighbour"

  (* -------------------------------------------------------------------------------- *)
  let insert domain id1 id2 graph =
    let node1 = Gid_map.find id1 graph.map in
    let node2 = Gid_map.find id2 graph.map in
    let pos1 = G_node.get_position node1 in
    let pos2 = G_node.get_position node2 in
    let new_pos= (pos1 +. pos2) /. 2. in
    let new_gid = graph.highest_index + 1 in
    let map = graph.map
      |> (Gid_map.add new_gid (G_node.fresh domain ~prec:id1 ~succ:id2 new_pos))
      |> (Gid_map.add id1 (G_node.set_succ new_gid node1))
      |> (Gid_map.add id2 (G_node.set_prec new_gid node2)) in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let append domain id graph =
    let node = Gid_map.find id graph.map in
    let pos = G_node.get_position node in
    let new_pos= pos +. 1. in
    let new_gid = graph.highest_index + 1 in
    let map = graph.map
      |> (Gid_map.add new_gid (G_node.fresh domain ~prec:id new_pos))
      |> (Gid_map.add id (G_node.set_succ new_gid node)) in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let prepend domain id graph =
    let node = Gid_map.find id graph.map in
    let pos = G_node.get_position node in
    let new_pos= pos -. 1. in
    let new_gid = graph.highest_index + 1 in
    let map = graph.map
      |> (Gid_map.add new_gid (G_node.fresh domain ~succ:id new_pos))
      |> (Gid_map.add id (G_node.set_prec new_gid node)) in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let add_after loc domain node_id graph =
    let node = Gid_map.find node_id graph.map in
    match G_node.get_succ node with
    | Some gid_succ -> insert domain node_id gid_succ graph
    | None -> append domain node_id graph

  (* -------------------------------------------------------------------------------- *)
  let add_before loc domain node_id graph =
    let node = Gid_map.find node_id graph.map in
    match G_node.get_prec node with
    | Some gid_prec -> insert domain gid_prec node_id graph
    | None -> prepend domain node_id graph

  (* -------------------------------------------------------------------------------- *)
  (* move out-edges (which respect cst [labels,neg]) from id_src are moved to out-edges out off node id_tar *)
  let shift_out loc domain src_gid tar_gid label_cst graph =
    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    let src_next = G_node.get_next src_node in
    let tar_next = G_node.get_next tar_node in

    (* Error if a loop is created by the shift_out *)
    let src_tar_edges = Massoc_gid.assoc tar_gid src_next in
    let _ =
      try
        let loop_edge = List.find (fun edge -> Label_cst.match_ domain label_cst edge) src_tar_edges in
        Error.run ~loc "The shfit_out command tries to build a loop (with label %s)" (Label.to_string domain loop_edge)
      with Not_found -> () in

    let (new_src_next,new_tar_next) =
    Massoc_gid.fold
      (fun (acc_src_next,acc_tar_next) next_gid edge ->
        if Label_cst.match_ domain label_cst edge
        then
          match Massoc_gid.add next_gid edge acc_tar_next with
          | Some new_acc_tar_next -> (Massoc_gid.remove next_gid edge acc_src_next, new_acc_tar_next)
          | None -> Error.run ~loc "The [shift_out] command tries to build a duplicate edge (with label \"%s\")" (Label.to_string domain edge)

        else (acc_src_next,acc_tar_next)
      )
      (src_next, tar_next) src_next in

    { graph with map =
      graph.map
      |> (Gid_map.add src_gid (G_node.set_next new_src_next src_node))
      |> (Gid_map.add tar_gid (G_node.set_next new_tar_next tar_node))
    }

  (* -------------------------------------------------------------------------------- *)
  let shift_in loc domain src_gid tar_gid label_cst graph =
    let tar_node = Gid_map.find tar_gid graph.map in
    let tar_next = G_node.get_next tar_node in

    (* Error if a loop is created by the shift_in *)
    let tar_src_edges = Massoc_gid.assoc src_gid tar_next in
    let _ =
      try
        let loop_edge = List.find (fun edge -> Label_cst.match_ domain label_cst edge) tar_src_edges in
        Error.run ~loc "The [shift_in] command tries to build a loop (with label \"%s\")" (Label.to_string domain loop_edge)
      with Not_found -> () in

    { graph with map =
        Gid_map.mapi
          (fun node_id node ->
            let node_next = G_node.get_next node in
            match Massoc_gid.assoc src_gid node_next with
            | [] -> node (* no edges from node to src *)
            | node_src_edges ->
              let node_tar_edges = Massoc_gid.assoc tar_gid node_next in
              let (new_node_src_edges, new_node_tar_edges) =
              List.fold_left
              (fun (acc_node_src_edges,acc_node_tar_edges) edge ->
                if Label_cst.match_ domain label_cst edge
                then
                  match List_.usort_insert edge acc_node_tar_edges with
                  | None -> Error.run ~loc "The [shift_in] command tries to build a duplicate edge (with label \"%s\")" (Label.to_string domain edge)
                  | Some l -> (List_.usort_remove edge acc_node_src_edges, l)
                else (acc_node_src_edges,acc_node_tar_edges)
              )
              (node_src_edges, node_tar_edges) node_src_edges in
              let new_next =
                node_next
                |> (Massoc_gid.replace src_gid new_node_src_edges)
                |> (Massoc_gid.replace tar_gid new_node_tar_edges) in
              G_node.set_next new_next node
          ) graph.map
    }

  (* -------------------------------------------------------------------------------- *)
  let shift_edges loc domain src_gid tar_gid label_cst graph =
    graph
    |> (shift_in loc domain src_gid tar_gid label_cst)
    |> (shift_out loc domain src_gid tar_gid label_cst)

  (* -------------------------------------------------------------------------------- *)
  let merge_node loc domain graph src_gid tar_gid =
    let se_graph = shift_edges loc domain src_gid tar_gid Label_cst.all graph in

    let src_node = Gid_map.find src_gid se_graph.map in
    let tar_node = Gid_map.find tar_gid se_graph.map in

    match G_fs.unif (G_node.get_fs src_node) (G_node.get_fs tar_node) with
      | Some new_fs ->
        Some {graph with map =
            (Gid_map.add
               tar_gid
               (G_node.set_fs new_fs tar_node)
               (Gid_map.remove src_gid se_graph.map)
            )
             }
      | None -> None

  (* -------------------------------------------------------------------------------- *)
  let set_feat ?loc domain graph node_id feat_name new_value =
    let node = Gid_map.find node_id graph.map in
    let new_node =
      match feat_name with
        | "position" -> G_node.set_position (float_of_string new_value) node
        | _ ->
          let new_fs = G_fs.set_feat ?loc domain feat_name new_value (G_node.get_fs node) in
          (G_node.set_fs new_fs node) in
    { graph with map = Gid_map.add node_id new_node graph.map }

  (* -------------------------------------------------------------------------------- *)
  let update_feat ?loc domain graph tar_id tar_feat_name item_list =
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
    (set_feat ?loc domain graph tar_id tar_feat_name new_feature_value, new_feature_value)

  (* -------------------------------------------------------------------------------- *)
  let del_feat graph node_id feat_name =
    let node = Gid_map.find node_id graph.map in
    let new_fs = G_fs.del_feat feat_name (G_node.get_fs node) in
    { graph with map = Gid_map.add node_id (G_node.set_fs new_fs node) graph.map }

  (* -------------------------------------------------------------------------------- *)
  let to_gr domain graph =
    let buff = Buffer.create 32 in

    bprintf buff "graph {\n";

    (* meta data *)
    List.iter
      (fun (s) ->
        bprintf buff "  %s;\n" s
      ) graph.meta;

    (* nodes *)
    let nodes = Gid_map.fold
      (fun id node acc ->
        if G_node.is_conll_root node
        then acc
        else (id,node)::acc
      ) graph.map [] in

    let sorted_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in
    List.iter
      (fun (id,node) ->
        bprintf buff "  N_%s %s;\n" (Gid.to_string id) (G_node.to_gr node)
      ) sorted_nodes;

    (* edges *)
    List.iter
      (fun (id,node) ->
        Massoc_gid.iter
          (fun tar edge ->
            bprintf buff "  N_%s -[%s]-> N_%s;\n" (Gid.to_string id) (G_edge.to_string domain edge) (Gid.to_string tar)
          ) (G_node.get_next node)
      ) sorted_nodes;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let to_sentence ?main_feat graph =
    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in

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
  let to_dep domain ?filter ?main_feat ?(deco=G_deco.empty) graph =
    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in

    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { opacity=0; scale = 200; fontname=\"Arial\"; }\n";
    bprintf buff "[WORDS] { \n";

    (* nodes *)
    List.iter
      (fun (id, node) ->
        let decorated_feat = try List.assoc id deco.G_deco.nodes with Not_found -> ("",[]) in
        let fs = G_node.get_fs node in
        let dep_fs = G_fs.to_dep ~decorated_feat ~position:(G_node.get_position node) ?filter ?main_feat fs in

        let style = match G_fs.get_string_atom "void" fs with
          | Some "y" -> "; forecolor=red; subcolor=red; "
          | _ -> "" in

        bprintf buff "N_%s { %s%s }\n"
          (Gid.to_string id)
          dep_fs
          style
      ) snodes;
    bprintf buff "} \n";

    (* edges *)
    bprintf buff "[EDGES] { \n";

    if !Global.debug
    then
      List.iter
        (fun (id, node) ->
          begin
            match G_node.get_prec node with
            | None -> ()
            | Some p -> bprintf buff "N_%s -> N_%s { label=\"__PREC__\"; bottom; style=dot; color=lightblue; forecolor=lightblue; }\n" (Gid.to_string id) (Gid.to_string p)
          end;
          begin
            match G_node.get_succ node with
            | None -> ()
            | Some s -> bprintf buff "N_%s -> N_%s { label=\"__SUCC__\"; bottom; style=dot; color=lightblue; forecolor=lightblue; }\n" (Gid.to_string id) (Gid.to_string s)
          end
        ) snodes;

    Gid_map.iter
      (fun gid elt ->
        Massoc_gid.iter
          (fun tar g_edge ->
            if not (G_edge.is_void domain g_edge)
            then
              let deco = List.mem (gid,g_edge,tar) deco.G_deco.edges in
              bprintf buff "N_%s -> N_%s %s\n" (Gid.to_string gid) (Gid.to_string tar) (G_edge.to_dep domain ~deco g_edge)
          ) (G_node.get_next elt)
      ) graph.map;

    bprintf buff "} \n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let to_dot domain ?main_feat ?(deco=G_deco.empty) graph =
    let buff = Buffer.create 32 in

    bprintf buff "digraph G {\n";
    (* bprintf buff "  rankdir=LR;\n"; *)
    bprintf buff "  node [shape=Mrecord];\n";

    (* nodes *)
    Gid_map.iter
      (fun id node ->
        let decorated_feat =
          try List.assoc id deco.G_deco.nodes
          with Not_found -> ("",[]) in
        bprintf buff "  N_%s [label=<%s>, color=%s]\n"
          (Gid.to_string id)
          (G_fs.to_dot ~decorated_feat ?main_feat (G_node.get_fs node))
          (* TODO: add bgcolor in dot output *)
          (if List.mem_assoc id deco.G_deco.nodes then "red" else "black")
      ) graph.map;

    (* edges *)
    Gid_map.iter
      (fun id node ->
        Massoc_gid.iter
          (fun tar g_edge ->
            let deco = List.mem (id,g_edge,tar) deco.G_deco.edges in
            bprintf buff "  N_%s -> N_%s%s\n" (Gid.to_string id) (Gid.to_string tar) (G_edge.to_dot domain ~deco g_edge)
          ) (G_node.get_next node)
      ) graph.map;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let list_num test =
    let rec loop n = function
      | [] -> raise Not_found
      | x::_ when test x -> n
      | _::t -> loop (n+1) t
    in loop 0

  let to_raw domain graph =
    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in
    let raw_nodes = List.map (fun (gid,node) -> (gid, G_fs.to_raw (G_node.get_fs node))) snodes in

    let get_num gid = list_num (fun (x,_) -> x=gid) raw_nodes in
    let edge_list = ref [] in
    Gid_map.iter
      (fun src_gid node ->
        Massoc_gid.iter
          (fun tar_gid edge ->
            edge_list := (get_num src_gid, G_edge.to_string domain edge, get_num tar_gid) :: !edge_list
          )
          (G_node.get_next node)
      )
      graph.map;
    (graph.meta, List.map snd raw_nodes, !edge_list)

  (* -------------------------------------------------------------------------------- *)
  let to_conll_string domain graph =
    let nodes = Gid_map.fold
      (fun gid node acc -> (gid,node)::acc)
      graph.map [] in

    (* sort nodes wrt position *)
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in

    (* renumbering of nodes to have a consecutive sequence of int 1 --> n, in case of node deletion or addition *)
    let snodes = List.mapi 
      (fun i (gid,node) -> (gid, G_node.set_position (float i) node)
      ) snodes in

    let get_num gid =
      let gnode = List.assoc gid snodes in
      if G_node.is_conll_root gnode
      then 0.
      else G_node.get_position (List.assoc gid snodes) in

    (* Warning: [govs_labs] maps [gid]s to [num]s *)
    let govs_labs =
      Gid_map.fold
        (fun src_gid node acc ->
          let src_num = get_num src_gid in
          Massoc_gid.fold
            (fun acc2 tar_gid edge  ->
              let old = try Gid_map.find tar_gid acc2 with Not_found -> [] in
              Gid_map.add tar_gid ((sprintf "%g" src_num, G_edge.to_string domain edge)::old) acc2
            ) acc (G_node.get_next node)
        ) graph.map Gid_map.empty in

    let buff = Buffer.create 32 in
    List.iter (fun v -> bprintf buff "%s\n" v) graph.meta;
    List.iter
      (fun (gid, node) ->
        begin
          try
            let (gid_last,fusion) = List.assoc gid graph.fusion in
            bprintf buff "%g-%g\t%s\t_\t_\t_\t_\t_\t_\t_\t_\n"
            (get_num gid) (get_num gid_last) fusion
          with
          | Not_found -> ()
        end;

        if not (G_node.is_conll_root node)
        then
          let gov_labs = try Gid_map.find gid govs_labs with Not_found -> [] in

          let sorted_gov_labs =
            List.sort
              (fun (g1,l1) (g2,l2) ->
                if l1 <> "" && l1.[0] <> 'I' && l1.[0] <> 'D'
                then -1
                else if l2 <> "" && l2.[0] <> 'I' && l2.[0] <> 'D'
                then 1
                else
                  match compare (String_.to_float g1) (String_.to_float g2) with
                    | 0 -> compare l1 l2
                    | x -> x
              ) gov_labs in
          let (govs,labs) = List.split sorted_gov_labs in
          let fs = G_node.get_fs node in
          bprintf buff "%g\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t_\t_\n"
            (get_num gid)
            (match G_fs.get_string_atom "phon" fs with Some p -> p | None -> "_")
            (match G_fs.get_string_atom "lemma" fs with Some p -> p | None -> "_")
            (match G_fs.get_string_atom "cat" fs with Some p -> p | None -> "_")
            (match G_fs.get_string_atom "pos" fs with Some p -> p | None -> "_")
            (G_fs.to_conll ~exclude: ["phon"; "lemma"; "cat"; "pos"; "position"] fs)
            (match govs with [] -> "_" | _ -> String.concat "|" govs)
            (match labs with [] -> "_" | _ -> String.concat "|" labs)
      )
      snodes;
    Buffer.contents buff
end (* module G_graph *)
