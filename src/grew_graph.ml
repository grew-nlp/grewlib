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
        begin
          try (node_id, P_node.unif_fs (P_fs.build ?pat_vars ast_node.Ast.fs) fs) :: tail
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
  (* a type for extension of graph (a former graph exists):
     in grew the former is a positive basic and an extension is a negative basic ("without") *)
  type extension = {
    ext_map: P_node.t Pid_map.t; (* node description for new nodes and for edge "Old -> New"  *)
    old_map: P_node.t Pid_map.t; (* a partial map for new constraints on old nodes "Old [...]" *)
  }

  (* -------------------------------------------------------------------------------- *)
  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_extension ?pat_vars ?(locals=[||]) pos_table full_node_list full_edge_list =

    let built_nodes = List.map (P_node.build ?pat_vars) full_node_list in

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
          let edge = P_edge.build ~locals (ast_edge, loc) in
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
    meta: (string * string) list; (* meta-informations *)
    map: G_node.t Gid_map.t;      (* node description *)
  }

  let empty = {meta=[]; map=Gid_map.empty}

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

  (* ---------------------------------------------------------------------- *)
  (* [normalize g] changes all graphs keys to Old _ (used when entering a new module) *)
  let normalize t =
    let (_, mapping) =
      Gid_map.fold
        (fun key value (max_binding, mapping) ->
          match key with
          | Gid.Old n -> (n, mapping)
          | Gid.New _ -> (max_binding, mapping)
          | Gid.Act (n,suffix) -> (max_binding+1, (key, (Gid.Old (max_binding+1)))::mapping)
        ) t.map (0, []) in
        rename mapping t


  let find node_id graph = Gid_map.find node_id graph.map

  let equals t t' = Gid_map.equal (fun node1 node2 -> node1 = node2) t.map t'.map

  let node_exists fct t = Gid_map.exists (fun _ node -> fct node) t.map

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
  let edge_out graph node_id label_cst =
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> Label_cst.match_ e label_cst) (G_node.get_next node)

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
  let build ?(locals=[||]) gr_ast =
    let full_node_list = gr_ast.Ast.nodes
    and full_edge_list = gr_ast.Ast.edges in

    let next_free_position = ref 1. in

    let named_nodes =
      let rec loop already_bound = function
        | [] -> []
        | (ast_node, loc) :: tail ->
          let node_id = ast_node.Ast.node_id in
          if List.mem node_id already_bound
          then Error.build "[GRS] [Graph.build] try to build a graph with twice the same node id '%s'" node_id
          else
            let (new_id,new_node) = G_node.build ~def_position:!next_free_position (ast_node, loc) in
            next_free_position := 1. +. (max !next_free_position (G_node.get_position new_node));
            let new_tail = loop (node_id :: already_bound) tail in
            (new_id,new_node) :: new_tail in
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
    let sorted_lines = Conll.root :: (List.sort Conll.compare lines) in

    let table = Array.of_list (List.map (fun line -> line.Conll.num) sorted_lines) in

    let map_without_edges =
      List_.foldi_left
        (fun i acc line ->
          let loc = Loc.opt_set_line i loc in
          Gid_map.add (Gid.Old i) (G_node.of_conll ?loc line) acc)
        Gid_map.empty sorted_lines in
    let map_with_edges =
      List.fold_left
        (fun acc line ->
          (* add line number information in loc *)
          let loc = Loc.opt_set_line line.Conll.line_num loc in
          let dep_id = Id.build ?loc line.Conll.num table in
          List.fold_left
            (fun acc2 (gov, dep_lab) ->
              let gov_id = Id.build ?loc gov table in
              let edge = G_edge.make ?loc dep_lab in
              (match map_add_edge acc2 (Gid.Old gov_id) edge (Gid.Old dep_id) with
                | Some g -> g
                | None -> Error.build "[GRS] [Graph.of_conll] try to build a graph with twice the same edge %s %s"
                  (G_edge.to_string edge)
                  (match loc with Some l -> Loc.to_string l | None -> "")
              )
            ) acc line.Conll.deps
        ) map_without_edges lines in
    {meta=[]; map=map_with_edges}

  (* -------------------------------------------------------------------------------- *)
  (** input : "Le/DET/le petit/ADJ/petit chat/NC/chat dort/V/dormir ./PONCT/." *)
  let of_brown ?sentid brown =
    let units = Str.split (Str.regexp " ") brown in
      let conll_lines = List.mapi
      (fun i item -> match Str.full_split (Str.regexp "/[A-Z'+'']+/") item with
        | [Str.Text phon; Str.Delim pos; Str.Text lemma] ->
        let pos = String.sub pos 1 ((String.length pos)-2) in
        let morph = match (i,sentid) with
        | (0,Some id) -> [("sentid", id)]
        | _ -> [] in
        {
          Conll.line_num=0;
          num = sprintf "%d" (i+1);
          phon;
          lemma;
          pos1 = "_";
          pos2 = pos;
          morph;
          deps = [(sprintf "%d" i, "SUC")]
          }
        | _ -> Error.build "[Graph.of_brown] Cannot parse Brown item >>>%s<<< (expected \"phon/POS/lemma\")" item
      ) units in 
    of_conll conll_lines

  (* -------------------------------------------------------------------------------- *)
  let opt_att atts name =
    try Some (List.assoc name atts)
    with Not_found -> None

 (* -------------------------------------------------------------------------------- *)
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
                  let new_node = G_node.set_fs new_fs (G_node.set_position (float i) G_node.empty) in
                  (Gid_map.add (Gid.Old i) new_node acc, String_map.add id (Gid.Old i) acc_map)
                | _ -> Log.critical "[G_graph.of_xml] Not a wellformed <T> tag"
            ) (Gid_map.empty, String_map.empty) t_list in
        let final_map =
          List.fold_left
            (fun acc r_xml ->
              match r_xml with
                | Xml.Element ("R", r_atts, _) ->
                  let src = List.assoc "from" r_atts
                  and tar = List.assoc "to" r_atts
                  and label = List.assoc "label" r_atts in
                  let gid_tar = String_map.find tar mapping in
                  let gid_src = String_map.find src mapping in
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
  let activate loc node_id suffix graph =
    let index = match node_id with
      | Gid.Old id -> Gid.Act (id, suffix)
      | _ -> Error.run ~loc "[Graph.activate] is possible only from a \"ground\" node" in

    if Gid_map.mem index graph.map
    then Error.run ~loc "[Graph.activate] try to activate twice the \"same\" node (with suffix '%s')" suffix;

    let node = Gid_map.find node_id graph.map in
    let new_map = Gid_map.add index (G_node.build_new node) graph.map in
    (index, {graph with map = new_map})

  (* -------------------------------------------------------------------------------- *)
  let add_neighbour loc graph node_id label =
    let index = match node_id with
      | Gid.Old id ->
        (match Label.to_int label with
          | Some label_int -> Gid.New (id, label_int)
          | None -> Error.run ~loc "[Graph.add_neighbour] try to add neighbour with a local label"
        )
      | Gid.New _ | Gid.Act _ -> Error.run ~loc "[Graph.add_neighbour] try to add neighbour node to a neighbour node" in

    if Gid_map.mem index graph.map
    then Error.run ~loc "[Graph.add_neighbour] try to build twice the \"same\" neighbour node (with label '%s')" (Label.to_string label);

    let node = Gid_map.find node_id graph.map in
    (* put the new node on the right of its "parent" *)
    let new_map = Gid_map.add index (G_node.build_neighbour node) graph.map in

    match map_add_edge new_map node_id label index with
      | Some g -> (index, {graph with map = g})
      | None -> Log.bug "[Graph.add_neighbour] add_edge must not fail"; exit 1

  (* -------------------------------------------------------------------------------- *)
  (* move out-edges (which respect cst [labels,neg]) from id_src are moved to out-edges out off node id_tar *)
  let shift_out loc src_gid tar_gid label_cst graph =
    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    let src_next = G_node.get_next src_node in
    let tar_next = G_node.get_next tar_node in

    (* Error if a loop is created by the shift_out *)
    let src_tar_edges = Massoc_gid.assoc tar_gid src_next in
    let _ =
      try
        let loop_edge = List.find (fun edge -> Label_cst.match_ edge label_cst) src_tar_edges in
        Error.run ~loc "The shfit_out command tries to build a loop (with label %s)" (Label.to_string loop_edge)
      with Not_found -> () in

    let (new_src_next,new_tar_next) =
    Massoc_gid.fold
      (fun (acc_src_next,acc_tar_next) next_gid edge ->
        if Label_cst.match_ edge label_cst
        then
          match Massoc_gid.add next_gid edge acc_tar_next with
          | Some new_acc_tar_next -> (Massoc_gid.remove next_gid edge acc_src_next, new_acc_tar_next)
          | None -> Error.run ~loc "The [shift_out] command tries to build a duplicate edge (with label \"%s\")" (Label.to_string edge)

        else (acc_src_next,acc_tar_next)
      )
      (src_next, tar_next) src_next in

    { graph with map =
      graph.map
      |> (Gid_map.add src_gid (G_node.set_next new_src_next src_node))
      |> (Gid_map.add tar_gid (G_node.set_next new_tar_next tar_node))
    }

  (* -------------------------------------------------------------------------------- *)
  let shift_in loc src_gid tar_gid label_cst graph =
    let tar_node = Gid_map.find tar_gid graph.map in
    let tar_next = G_node.get_next tar_node in

    (* Error if a loop is created by the shift_in *)
    let tar_src_edges = Massoc_gid.assoc src_gid tar_next in
    let _ =
      try
        let loop_edge = List.find (fun edge -> Label_cst.match_ edge label_cst) tar_src_edges in
        Error.run ~loc "The [shift_in] command tries to build a loop (with label \"%s\")" (Label.to_string loop_edge)
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
                if Label_cst.match_ edge label_cst
                then
                  match List_.usort_insert edge acc_node_tar_edges with
                  | None -> Error.run ~loc "The [shift_in] command tries to build a duplicate edge (with label \"%s\")" (Label.to_string edge)
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
  let shift_edges loc src_gid tar_gid label_cst graph =
    graph
    |> (shift_in loc src_gid tar_gid label_cst)
    |> (shift_out loc src_gid tar_gid label_cst)

  (* -------------------------------------------------------------------------------- *)
  let merge_node loc graph src_gid tar_gid =
    let se_graph = shift_edges loc src_gid tar_gid Label_cst.all graph in

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
  let set_feat ?loc graph node_id feat_name new_value =
    let node = Gid_map.find node_id graph.map in
    let new_node =
      match feat_name with
        | "position" -> G_node.set_position (float_of_string new_value) node
        | _ ->
          let new_fs = G_fs.set_feat ?loc feat_name new_value (G_node.get_fs node) in
          (G_node.set_fs new_fs node) in
    { graph with map = Gid_map.add node_id new_node graph.map }

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
    { graph with map = Gid_map.add node_id (G_node.set_fs new_fs node) graph.map }

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
            bprintf buff "  N_%s -[%s]-> N_%s;\n" (Gid.to_string id) (G_edge.to_string edge) (Gid.to_string tar)
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
  let to_dep ?filter ?main_feat ?(deco=G_deco.empty) graph =
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
            bprintf buff "  N_%s -> N_%s%s\n" (Gid.to_string id) (Gid.to_string tar) (G_edge.to_dot ~deco g_edge)
          ) (G_node.get_next node)
      ) graph.map;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let to_raw graph =
    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in
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
              Gid_map.add tar_gid ((sprintf "%g" src_num, G_edge.to_string edge)::old) acc2
            ) acc (G_node.get_next node)
        ) graph.map Gid_map.empty in

    let buff = Buffer.create 32 in
    List.iter
      (fun (gid, node) ->
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
            (match G_fs.get_string_atom "phon" fs with Some p -> p | None -> "_e_")
            (match G_fs.get_string_atom "lemma" fs with Some p -> p | None -> "_e_")
            (match G_fs.get_string_atom "cat" fs with Some p -> p | None -> "_")
            (match G_fs.get_string_atom "pos" fs with Some p -> p | None -> "_")
            (G_fs.to_conll ~exclude: ["phon"; "lemma"; "cat"; "pos"; "position"] fs)
            (String.concat "|" govs)
            (String.concat "|" labs)
      )
      snodes;
    Buffer.contents buff
end (* module G_graph *)
