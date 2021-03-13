(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
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
module P_graph = struct
  type t = P_node.t Pid_map.t

  let empty = Pid_map.empty

  let find = Pid_map.find

  let pid_name_list t = Pid_map.fold (fun _ node acc -> (P_node.get_name node)::acc) t []

  let to_json_python ~config t =
    `List (
      Pid_map.fold
        (fun pid p_node acc ->
           (`Assoc [
               ("id", `String (Pid.to_string pid));
               ("node", P_node.to_json_python ~config p_node)
             ]) :: acc
        ) t []
    )

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge_opt src_pid label tar_pid map =
    let src_node =
      match Pid_map.find_opt src_pid map with
      | None -> P_node.empty (* adding an edge from ker to ext *)
      | Some p_node -> p_node in
    match P_node.add_edge_opt label tar_pid src_node with
    | None -> None
    | Some new_node -> Some (Pid_map.add src_pid new_node map)

  (* -------------------------------------------------------------------------------- *)
  let of_ast ~config lexicons basic_ast =
    let full_node_list = basic_ast.Ast.pat_nodes
    and full_edge_list = basic_ast.Ast.pat_edges in

    let nodes_id = List.map (fun ({Ast.node_id},_) -> node_id) full_node_list in
    let edges_id = CCList.filter_map (fun ({Ast.edge_id},_) -> edge_id) full_edge_list in

    begin
      match List_.intersect nodes_id edges_id with
      | [] -> ()
      | id::_ ->
        let loc = snd (List.find (fun ({Ast.node_id},_) -> node_id = id) full_node_list) in
        Error.build ~loc "The same identifier `%s` cannot be used both as a node identifier and as an edge identifier" id
    end;

    (* NB: insert searches for a previous node with the same name and uses unification rather than constraint *)
    (* NB: insertion of new node at the end of the list: not efficient but graph building is not the hard part. *)
    let rec insert (ast_node, loc) = function
      | [] -> [P_node.of_ast lexicons (ast_node, loc)]
      | (node_id,fs)::tail when ast_node.Ast.node_id = node_id ->
        begin
          try (node_id, P_node.unif_fs (P_fs.of_ast lexicons ast_node.Ast.fs) fs) :: tail
          with Error.Build (msg,_) -> raise (Error.Build (msg,Some loc))
        end
      | head :: tail -> head :: (insert (ast_node, loc) tail) in

    let named_nodes =
      List.fold_left
        (fun acc ast_node -> insert ast_node acc)
        [] full_node_list in

    let sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Stdlib.compare id1 id2) named_nodes in
    let (sorted_ids, node_list) = List.split sorted_nodes in

    (* [ker_table] contains the sorted list of node ids *)
    let ker_table = Array.of_list sorted_ids in

    (* the nodes, in the same order *)
    let map_without_edges = List_.foldi_left
        (fun i acc elt -> Pid_map.add (Pid.Ker i) elt acc)
        Pid_map.empty node_list in

    let (map,edge_ids) =
      List.fold_left
        (fun (acc_map,acc_edge_ids) (ast_edge, loc) ->
           let src_pid = Pid.Ker (Id.build ~loc ast_edge.Ast.src ker_table) in
           let tar_pid = Pid.Ker (Id.build ~loc ast_edge.Ast.tar ker_table) in
           match ast_edge.Ast.edge_label_cst with
           | Ast.Pred -> (* X < Y *)
             begin
               match map_add_edge_opt src_pid P_edge.succ tar_pid acc_map with
               | None -> Error.build ~loc "[P_graph.build] try to build a graph with twice the order edge"
               | Some acc2 ->
                 begin
                   match map_add_edge_opt tar_pid P_edge.pred src_pid acc2 with
                   | None -> Error.build ~loc "[P_graph.build] try to build a graph with twice the order edge"
                   | Some m -> (m, acc_edge_ids)
                 end
             end
           | _ ->
             let edge = P_edge.of_ast ~config (ast_edge, loc) in
             begin
               match map_add_edge_opt src_pid edge tar_pid acc_map with
               | Some m -> (m, match ast_edge.Ast.edge_id with Some id -> id::acc_edge_ids | None -> acc_edge_ids)
               | None -> Error.build ~loc "[P_graph.build] try to build a graph with twice the same edge %s" (P_edge.to_string ~config edge)
             end
        ) (map_without_edges,[]) full_edge_list in

    (map, ker_table, edge_ids)

  (* -------------------------------------------------------------------------------- *)

  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let of_ast_extension ~config lexicons ker_table edge_ids full_node_list full_edge_list =

    let built_nodes = List.map (P_node.of_ast lexicons) full_node_list in

    let (old_nodes, new_nodes) =
      List.partition
        (function (id,_) when Array_.dicho_mem id ker_table -> true | _ -> false)
        built_nodes in

    let new_sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Stdlib.compare id1 id2) new_nodes in

    let (new_sorted_ids, new_node_list) = List.split new_sorted_nodes in

    (* table contains the sorted list of node ids *)
    let ext_table = Array.of_list new_sorted_ids in

    (* the nodes, in the same order stored with index -1, -2, ... -N TODO check ?? *)
    let ext_map_without_edges =
      List_.foldi_left
        (fun i acc elt -> Pid_map.add (Pid.Ext i) elt acc)
        Pid_map.empty
        new_node_list in

    (* let old_map_without_edges = *)
    let filter_on_old_edges =
      List.fold_left
        (fun acc (id,node) ->
           let ker_pid = Pid.Ker (Array_.dicho_find id ker_table) in
           let p_fs = P_node.get_fs node in
           match Pid_map.find_opt ker_pid acc with
           | None -> Pid_map.add ker_pid p_fs acc
           | Some old_p_fs -> Pid_map.add ker_pid (P_fs.unif old_p_fs p_fs) acc
        ) Pid_map.empty old_nodes in

    let (ext_map_with_all_edges, new_edge_ids) =
      List.fold_left
        (fun (acc_map, acc_edge_ids) (ast_edge, loc) ->
           let src = ast_edge.Ast.src
           and tar = ast_edge.Ast.tar in
           let src_pid =
             match Id.build_opt src ker_table with
             | Some i -> Pid.Ker i
             | None -> Pid.Ext (Id.build ~loc src ext_table) in
           let tar_pid =
             match Id.build_opt tar ker_table with
             | Some i -> Pid.Ker i
             | None -> Pid.Ext (Id.build ~loc tar ext_table) in

           match ast_edge.Ast.edge_label_cst with
           | Ast.Pred ->
             (match map_add_edge_opt src_pid P_edge.succ tar_pid acc_map with
              | Some acc2 -> (match map_add_edge_opt tar_pid P_edge.pred src_pid acc_map with
                  | Some m -> (m, acc_edge_ids)
                  | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the order edge"
                )
              | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the order edge"
             )
           | _ ->
             let edge = P_edge.of_ast ~config (ast_edge, loc) in
             (match map_add_edge_opt src_pid edge tar_pid acc_map with
              | Some m -> (m, match ast_edge.Ast.edge_id with Some id -> id::acc_edge_ids | None -> acc_edge_ids)
              | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the same edge %s"
                          (P_edge.to_string ~config edge)
             )
        ) (ext_map_without_edges, edge_ids) full_edge_list in

    (ext_map_with_all_edges, filter_on_old_edges, ext_table, new_edge_ids)

  (* -------------------------------------------------------------------------------- *)

  (* return the list of [pid] of nodes without in edges (Pred & Succ are taken into account) *)
  let roots (p_graph: t) =
    let not_root_set =
      Pid_map.fold
        (fun _ p_node acc ->
           Massoc_pid.fold
             (fun acc2 tar_pid _ -> Pid_set.add tar_pid acc2
             ) acc (P_node.get_next p_node)
        ) p_graph Pid_set.empty in
    Pid_map.fold
      (fun pid _ acc ->
         if Pid_set.mem pid not_root_set
         then acc
         else pid::acc
      ) p_graph []
end (* module P_graph *)

(* ================================================================================ *)
module G_deco = struct
  (* value is (f, Some g) for combined request "f=v/g=u" and (j, None) else *)
  type highlighted_feat = string * string option

  type t = {
    (* a list of (node, (pattern_id, features of nodes implied in the step)) *)
    nodes: (Gid.t * (string * highlighted_feat list)) list;
    (* an edge list *)
    edges: (Gid.t * G_edge.t * Gid.t) list;
  }

  let empty = {nodes=[]; edges=[]; }

  let merge deco1 deco2 =
    {
      nodes =
        List.fold_left
          (fun acc ((gid2, (_,hf_list2)) as item2) ->
             match List.assoc_opt gid2 acc with
             | None -> item2 :: acc
             | Some (pid1,hf_list1) -> (gid2, (pid1, hf_list1 @ hf_list2)) :: (List.remove_assoc gid2 acc)
          ) deco1.nodes deco2.nodes;
      edges = deco1.edges @ deco2.edges;
    }
end (* module G_deco *)

(* ================================================================================ *)
module G_graph = struct

  type trace_item = G_deco.t * (string * int) * G_deco.t * t

  and t = {
    meta: (string * string) list; (* meta-informations *)
    map: G_node.t Gid_map.t;      (* node description *)
    highest_index: int;           (* the next free integer index *)
    rules: int String_map.t;
    trace: trace_item option;     (* if the rewriting history is kept *)
    impact: G_deco.t;
  }

  let get_meta_opt key t = List.assoc_opt key t.meta

  let get_meta_list t = t.meta

  let set_meta key value t = {t with meta = (key,value) :: List.remove_assoc key t.meta}

  let empty = { meta=[]; map=Gid_map.empty; highest_index=0; rules=String_map.empty; trace=None; impact=G_deco.empty}

  let is_empty t = Gid_map.is_empty t.map

  let is_initial g = String_map.is_empty g.rules

  let size t = Gid_map.cardinal (t.map)

  let find node_id graph = Gid_map.find node_id graph.map
  let find_opt node_id graph = Gid_map.find_opt node_id graph.map

  let node_exists fct t = Gid_map.exists (fun _ node -> fct node) t.map

  let fold_gid fct t init =
    Gid_map.fold (fun gid _ acc -> fct gid acc) t.map init

  let track_rules (rule_name,_) t =
    if !Global.track_rules
    then
      let old = try String_map.find rule_name t.rules with Not_found -> 0 in
      { t with rules = String_map.add rule_name (old+1) t.rules }
    else t

  let track_history up rule_info down previous_graph t =
    if !Global.track_history
    then { t with trace = Some (up, rule_info, down, previous_graph) }
    else t

  let track_impact down t =
    if !Global.track_impact
    then { t with impact = G_deco.merge t.impact down }
    else t

  let track up rule_info down previous_graph t =
    t
    |> track_rules rule_info
    |> track_history up rule_info down previous_graph
    |> track_impact down

  let get_history graph =
    let rec loop g =
      match g.trace with
      | None -> []
      | Some (u,r,d,g') -> (u,r,d,g') :: (loop g') in
    List.rev (loop graph)

  let rec trace_depth graph =
    match graph.trace with
    | None -> 0
    | Some (_,_,_,g) -> 1 + (trace_depth g)

  let string_rules t =
    String_map.fold
      (fun k v acc ->
         sprintf "%s:%d; %s" k v acc
      ) t.rules ""

  let clear_rules t = { t with rules = String_map.empty }

  (* is there an edge e out of node i ? *)
  let edge_out ~config graph node_id label_cst =
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> Label_cst.match_ ~config label_cst e) (G_node.get_next node)

  let covered node (gid1, _, gid2) graph =
    let node1 = find gid1 graph in
    let node2 = find gid2 graph in
    match (G_node.get_position_opt node, G_node.get_position_opt node1, G_node.get_position_opt node2) with
    | (Some p, Some p1, Some p2) -> (min p1 p2) < p && (max p1 p2) > p
    | _ -> false

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge_opt map src_gid label tar_gid =
    let src_node = Gid_map.find src_gid map in
    match G_node.add_edge_opt label tar_gid src_node with
    | None -> None
    | Some new_node -> Some (Gid_map.add src_gid new_node map)

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge src_gid label tar_gid map =
    let src_node = Gid_map.find src_gid map in
    match G_node.add_edge_opt label tar_gid src_node with
    | Some new_node -> Gid_map.add src_gid new_node map
    | None -> Error.bug "[Graph.map_add_edge] duplicate edge"

  (* -------------------------------------------------------------------------------- *)
  let add_edge_opt src_gid label tar_gid graph =
    match map_add_edge_opt graph.map src_gid label tar_gid with
    | Some new_map -> Some {graph with map = new_map }
    | None -> None

  (* -------------------------------------------------------------------------------- *)
  let map_del_edge_opt ?loc src_gid label tar_gid map =
    match Gid_map.find_opt src_gid map with
    | None -> Error.bug ?loc "[Graph.map_del_edge_opt] Some edge refers to a dead node"
    | Some src_node ->
      match G_node.remove_edge_opt tar_gid label src_node with
      | None -> None
      | Some new_node -> Some (Gid_map.add src_gid new_node map)

  (* -------------------------------------------------------------------------------- *)

  (* return input map if edge not found *)
  let map_del_edge ?loc src_gid label tar_gid map =
    match Gid_map.find_opt src_gid map with
    | None -> map
    | Some src_node ->
      match G_node.remove_edge_opt tar_gid label src_node with
      | None -> map
      | Some new_node -> Gid_map.add src_gid new_node map

  (* -------------------------------------------------------------------------------- *)
  let del_edge_opt ?loc src_gid label tar_gid graph =
    match map_del_edge_opt ?loc src_gid label tar_gid graph.map with
    | None -> None
    | Some new_map -> Some {graph with map = new_map }

  (* -------------------------------------------------------------------------------- *)
  let map_add_pred_succ gid1 gid2 map =
    map
    |> (map_add_edge gid1 G_edge.succ gid2)
    |> (map_add_edge gid2 G_edge.pred gid1)

  (* -------------------------------------------------------------------------------- *)
  let map_del_pred_succ gid1 gid2 map =
    map
    |> (map_del_edge gid1 G_edge.succ gid2)
    |> (map_del_edge gid2 G_edge.pred gid1)

  (* -------------------------------------------------------------------------------- *)
  let edge_length_opt (src_gid,_,tar_gid) graph =
    match (G_node.get_position_opt (find src_gid graph), G_node.get_position_opt (find tar_gid graph)) with
    | (Some src, Some tar) -> Some (abs (tar-src))
    | _ -> None

  (* -------------------------------------------------------------------------------- *)
  let edge_delta_opt (src_gid,_,tar_gid) graph =
    match (G_node.get_position_opt (find src_gid graph), G_node.get_position_opt (find tar_gid graph)) with
    | (Some src, Some tar) -> Some (tar-src)
    | _ -> None

  (* -------------------------------------------------------------------------------- *)
  let of_ast ~config gr_ast =
    let (ordered_nodes, unordered_nodes) =
      List.fold_left
        (fun (orderd_acc, unordered_acc) (node,loc) ->
           match Id.get_pos_opt node.Ast.node_id with
           | Some p -> ((p,(node,loc)) :: orderd_acc, unordered_acc)
           | None -> (orderd_acc, (node,loc) :: unordered_acc)
        ) ([],[]) gr_ast.Ast.nodes in

    let sorted_nodes = List.sort (fun (p1,_) (p2,_) -> Stdlib.compare p1 p2) ordered_nodes in

    let rec loop already_bound index = function
      | [] -> (Gid_map.empty,[])
      | (_, (ast_node, loc))::tail ->
        let node_id = ast_node.Ast.node_id in
        if List.mem node_id already_bound
        then Error.build ~loc "[GRS] [G_graph.of_ast] try to build a graph with twice the same node id '%s'" node_id
        else
          let (new_tail, table) = loop (node_id :: already_bound) (index+1) tail in
          let new_node = G_node.of_ast ~position:index (ast_node, loc) in
          (
            Gid_map.add index new_node new_tail,
            (node_id,index)::table
          ) in

    let rec loop_succ_pred acc_map = function
      | [] | [_] -> acc_map
      | x::y::t ->
        loop_succ_pred acc_map (y::t)
        |> map_add_pred_succ (snd x) (snd y)
    in

    let (map_with_ordered_nodes, table_ordered) = loop [] 0 sorted_nodes in

    let map_with_ordered_nodes_succ_pred = loop_succ_pred map_with_ordered_nodes table_ordered in

    let (map_without_edges, table, final_index) =
      List.fold_left
        (fun (acc_map, acc_table, acc_index) (ast_node,loc) ->
           let node_id = ast_node.Ast.node_id in
           let new_node = G_node.of_ast (ast_node,loc) in
           (
             Gid_map.add acc_index new_node acc_map,
             (node_id,acc_index)::acc_table,
             acc_index + 1
           )
        ) (map_with_ordered_nodes_succ_pred, table_ordered, List.length sorted_nodes) unordered_nodes in


    let map =
      List.fold_left
        (fun acc (ast_edge, loc) ->
           let i1 = List.assoc ast_edge.Ast.src table in
           let i2 = List.assoc ast_edge.Ast.tar table in
           let edge = G_edge.build ~config (ast_edge, loc) in
           (match map_add_edge_opt acc i1 edge i2 with
            | Some g -> g
            | None -> Error.build ~loc "[G_graph.of_ast] try to build a graph with twice the same edge %s"
                        (G_edge.dump ~config edge)
           )
        ) map_without_edges gr_ast.Ast.edges in

    { empty with
      meta=gr_ast.Ast.meta;
      map;
      highest_index = final_index - 1;
    }

  (* -------------------------------------------------------------------------------- *)
  let of_json (json: Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    let meta =
      try json |> member "meta" |> to_assoc |> List.map (fun (k,v) -> (k, v |> to_string))
      with Type_error _ -> [] in

    let nodes =
      match json |> member "nodes" with
      | `Null -> [] (* no nodes field *)
      | json_nodes ->
        try
          json_nodes
          |> to_assoc
          |> List.map
            (fun (id,json_node) ->
               let fs =
                 try [("label", json_node |> to_string)]
                 with Type_error _ ->
                   json_node
                   |> to_assoc
                   |> List.map (fun (feat_name,json_value) -> (feat_name, json_value |> to_string)) in
               (id,fs)
            )
        with Type_error _ ->
          Error.build
            "[G_graph.of_json] Cannot parse field `nodes` (See https://grew.fr/doc/json):\n%s"
            (Yojson.Basic.pretty_to_string json_nodes) in

    let json_edges = try json |> member "edges" |> to_list with Type_error _ -> [] in

    let edges =
      List.map
        (fun json_edge ->
           let fs =
             (* if [json_edge] is of type string, it is interpreted as [1=value] *)
             try [("1", typed_vos "1" (json_edge |> member "label" |> to_string))]
             with Type_error _ ->
               json_edge
               |> member "label"
               |> to_assoc
               |> List.map (fun (x,y) -> (x,typed_vos x (to_string y))) in
           (json_edge |> member "src" |> to_string, fs, json_edge |> member "tar" |> to_string)
        ) json_edges in

    let (map_without_edges, table, final_index) =
      List.fold_left
        (fun (acc_map, acc_table, acc_index) (node_id, fs_items) ->
           let fs = G_fs.of_items fs_items in
           let new_node = G_node.set_name node_id (G_node.set_fs fs G_node.empty) in
           (
             Gid_map.add acc_index new_node acc_map,
             String_map.add node_id acc_index acc_table,
             acc_index + 1
           )
        ) (Gid_map.empty, String_map.empty, 0) nodes in

    let order =
      try json |> member "order" |> to_list |> List.map (fun x ->String_map.find (x |> to_string) table)
      with Type_error _ -> [] in

    let rec loop_order (acc_map, acc_position) = function
      | [] -> acc_map
      | gid::tail ->
        let node = Gid_map.find gid acc_map in
        let map_with_position = Gid_map.add gid (G_node.set_position acc_position node) acc_map in
        let map_with_succ_prec =
          match tail with
          | [] -> map_with_position
          | gid_next :: _ -> map_add_pred_succ gid gid_next map_with_position in
        loop_order (map_with_succ_prec, acc_position+1) tail in

    let maps_with_order = loop_order (map_without_edges, 0) order in

    let map =
      List.fold_left
        (fun acc (id_src, edge_items, id_tar) ->
           match (String_map.find_opt id_src table, String_map.find_opt id_tar table) with
           | (Some gid_1, Some gid_2) ->
             let edge = G_edge.from_items edge_items in
             (match map_add_edge_opt acc gid_1 edge gid_2 with
              | Some g -> g
              | None -> Error.build "[G_graph.of_json] try to build a graph with twice the same edge %s" (G_edge.dump edge)
             )
           | (None, _) -> Error.build "[G_graph.of_json] undefined node id `%s` used as `src` in edges" id_src
           | (_, None) -> Error.build "[G_graph.of_json] undefined node id `%s` used as `tar` in edges" id_tar
        ) maps_with_order edges in

    { empty with
      meta;
      map;
      highest_index = final_index - 1;
    }

  (* -------------------------------------------------------------------------------- *)
  let to_json graph =
    let meta = `Assoc (List.map (fun (k,v) -> (k, `String v)) graph.meta) in

    let (nodes_rev, gid_position_list) =
      Gid_map.fold
        (fun gid node (acc_nodes, acc_gpl) ->
           let item = (Gid.to_string gid, G_fs.to_json (G_node.get_fs node)) in
           match G_node.get_position_opt node with
           | None -> (item :: acc_nodes, acc_gpl)
           | Some p -> (item :: acc_nodes, (gid,p) :: acc_gpl)
        ) graph.map ([], []) in

    let nodes = List.rev nodes_rev in

    let edges =
      Gid_map.fold
        (fun src_gid node acc ->
           let src = `String (Gid.to_string src_gid) in
           Massoc_gid.fold
             (fun acc2 tar_gid edge ->
                match G_edge.to_json_opt edge with
                | None -> acc2
                | Some js ->
                  (`Assoc [
                      ("src", src);
                      ("label", js);
                      ("tar", `String (Gid.to_string tar_gid));
                    ]) :: acc2
             ) acc (G_node.get_next node)
        ) graph.map [] in

    let order =
      List.map
        (fun (gid,_) -> `String (Gid.to_string gid))
        (List.sort (fun (_,p1) (_,p2) -> Stdlib.compare p1 p2) gid_position_list) in

    let modified_nodes =
      List.map
        (fun (gid,(_,hf_list)) ->
           `Assoc [("id", `String (Gid.to_string gid)); ("features", `List (List.map (fun (fn, _) -> `String fn) hf_list))]
        ) graph.impact.nodes in

    let modified_edges =
      List.map
        (fun (gid1,edge,gid2) ->
           `Assoc [("src", `String (Gid.to_string gid1)); ("edge", G_edge.to_json edge); ("tar", `String (Gid.to_string gid2))]
        ) graph.impact.edges in

    let full_assoc_list = [
      ("meta", meta);
      ("nodes", `Assoc nodes);
      ("edges", `List edges);
      ("order", `List order);
      ("modified_nodes", `List modified_nodes);
      ("modified_edges", `List modified_edges);
    ] in

    (* remove fields with empty data *)
    `Assoc (
      List.filter (function
          | (_,`List []) -> false
          | (_,`Assoc []) -> false
          | _ -> true
        ) full_assoc_list
    )


  (* -------------------------------------------------------------------------------- *)
  let of_json_python ~config json =
    match json with
    | `Assoc (l : (string * Yojson.Basic.t) list) ->
      let (ast_node_list, ast_edge_list) = List.fold_left
          (fun (acc_node, acc_edge) -> function
             | (id, `List [json_fs; `List succ]) ->
               let fs = match json_fs with
                 | `Assoc feat_json_list ->
                   List.map (function
                       | (feat_name, `String value) -> ({Ast.name= feat_name; kind = Ast.Equality [value]}, Loc.empty)
                       | (feat_name, json) -> Error.build "[Graph.of_json_python] invalid feature structure. feat_name=`%s` json=`%s`" feat_name (Yojson.Basic.pretty_to_string json)
                     ) feat_json_list
                 |  `String one -> [({Ast.name= "form"; kind = Ast.Equality [one]}, Loc.empty)]
                 | _ -> Error.build "[Graph.of_json_python] invalid fs" in
               let new_edges = List.map
                   (function
                     | `List [`String rel; `String tar] -> ({Ast.edge_id=None; edge_label_cst=Ast.Pos_list [rel]; src=id; tar},Loc.empty)
                     | _ -> Error.build "[Graph.of_json_python] invalid succ list"
                   ) succ in
               (
                 ({ Ast.node_id=id; fs}, Loc.empty) :: acc_node,
                 new_edges @ acc_edge
               )
             | (_,x) -> Error.build "[Graph.of_json_python] <1> ill formed graph\n%s" (Yojson.Basic.pretty_to_string x)
          ) ([],[]) l in
      let graph_ast = { Ast.meta=[]; nodes=ast_node_list; edges=ast_edge_list}
      in of_ast ~config graph_ast
    | x -> Error.build "[Graph.of_json_python]  <2> ill formed graph\n%s" (Yojson.Basic.pretty_to_string x)


  (* -------------------------------------------------------------------------------- *)
  (** input : "Le/DET/le petit/ADJ/petit chat/NC/chat dort/V/dormir ./PONCT/." *)

  let re = Str.regexp "/\\(ADJ\\|ADJWH\\|ADV\\|ADVWH\\|CC\\|CLO\\|CLR\\|CLS\\|CS\\|DET\\|DETWH\\|ET\\|I\\|NC\\|NPP\\|P\\|P\\+D\\|P\\+PRO\\|PONCT\\|PREF\\|PRO\\|PROREL\\|PROWH\\|V\\|VIMP\\|VINF\\|VPP\\|VPR\\|VS\\)/"

  let of_brown ?sentid ~config brown =
    let units = Str.split (Str.regexp " ") brown in
    let json_nodes =
      ("0", `Assoc [("form", `String "__0__")]) ::
      List.mapi (
        fun i item -> match Str.full_split re item with
          | [Str.Text form; Str.Delim xpos_string; Str.Text lemma] ->
            let xpos = String.sub xpos_string 1 ((String.length xpos_string)-2) in
            (string_of_int (i+1), `Assoc [("form", `String form); ("xpos", `String xpos); ("lemma", `String lemma)])
          | _ -> Error.build "[Graph.of_brown] Cannot parse Brown item >>>%s<<< (expected \"phon/POS/lemma\") in >>>%s<<<" item brown
      ) units in
    let order = List.map (fun (id,_) -> `String id) json_nodes in
    of_json (`Assoc [("nodes", `Assoc json_nodes); ("order", `List order)])

  (* -------------------------------------------------------------------------------- *)
  let of_pst pst =
    let cpt = ref 0 in
    let fresh_id () = incr cpt; !cpt - 1 in

    let leaf_list = ref [] in

    let rec loop nodes = function
      | Ast.Leaf (loc, phon) ->
        let fid = fresh_id () in
        let node = G_node.build_pst_leaf ~loc phon in
        leaf_list := fid :: ! leaf_list;
        (fid, Gid_map.add fid node nodes)

      | Ast.T (loc, cat, daughters) ->
        let fid = fresh_id () in
        let new_node = G_node.build_pst_node ~loc cat in
        let with_mother = Gid_map.add fid new_node nodes in
        let new_nodes = List.fold_left
            (fun map daughter ->
               let (daughter_id, new_map) = loop map daughter in
               map_add_edge fid G_edge.sub daughter_id new_map
            ) with_mother daughters in
        (fid, new_nodes) in

    let (_,map) = loop Gid_map.empty pst in

    let rec prec_loop position map = function
      | [] | [_] -> map
      | gid1 :: gid2 :: tail ->
        let new_map = prec_loop (position + 1) map (gid2 :: tail) in
        let node1 = Gid_map.find gid1 new_map in
        let new_node1 = G_node.set_position position node1 in
        new_map
        |> (map_add_pred_succ gid1 gid2)
        |> (Gid_map.add gid1 new_node1) in
    { empty with
      map=prec_loop 1 map (List.rev !leaf_list);
      highest_index = !cpt;
    }

  let del_edge_feature_opt ?loc edge_id feat_name (src_gid,edge,tar_gid) graph =
    match Gid_map.find_opt src_gid graph.map with
    | None -> Error.run ?loc "[Graph.del_edge_feature_opt] cannot find source node of edge \"%s\"" edge_id
    | Some src_node ->
      match G_node.del_edge_feature_opt tar_gid edge feat_name src_node with
      | Some (new_node, new_edge) -> Some ({graph with map = Gid_map.add src_gid new_node graph.map}, new_edge)
      | None -> None

  (* -------------------------------------------------------------------------------- *)
  (* [shift_position delta gid map] applies a position shift of [delta] to all nodes
     of the [map] that are successors of [gid] *)
  let rec shift_position delta gid map =
    let node = Gid_map.find gid map in
    match G_node.get_position_opt node with
    | None -> Error.run "[G_node.shift_position] unordered node"
    | Some p ->
      let new_node = G_node.set_position (p + delta) node in
      let next_map = Gid_map.add gid new_node map in
      match G_node.get_succ_opt node with
      | None -> next_map
      | Some next_gid -> shift_position delta next_gid next_map

  (* -------------------------------------------------------------------------------- *)
  let add_before gid graph =
    let node = Gid_map.find gid graph.map in
    let position = match G_node.get_position_opt node with
      | None -> Error.run "[G_node.insert_before] unordered node"
      | Some p -> p in
    let shifted_map = shift_position 1 gid graph.map in
    let new_gid = graph.highest_index + 1 in
    let new_node = G_node.set_position position G_node.empty in
    let new_map = match G_node.get_pred_opt node with
      | None ->
        shifted_map
        |> (Gid_map.add new_gid new_node)
        |> (map_add_pred_succ new_gid gid)
      | Some prec_gid ->
        shifted_map
        |> (Gid_map.add new_gid new_node)
        |> (map_del_pred_succ prec_gid gid)
        |> (map_add_pred_succ new_gid gid)
        |> (map_add_pred_succ prec_gid new_gid) in
    (new_gid, { graph with map=new_map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  (* WARNING: use only if [last_gid] is the last ordered node in graph! *)
  let append last_gid graph =
    let last_node = Gid_map.find last_gid graph.map in
    let position = match G_node.get_position_opt last_node with
      | None -> Error.run "[G_node.append] unordered nodes"
      | Some p -> p + 1 in
    let new_gid = graph.highest_index + 1 in
    let new_node = G_node.set_position position G_node.empty in
    let new_map =
      graph.map
      |> (Gid_map.add new_gid new_node)
      |> (map_add_pred_succ last_gid new_gid) in
    (new_gid, { graph with map=new_map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let add_after gid graph =
    match G_node.get_succ_opt (Gid_map.find gid graph.map) with
    | Some gid_succ -> add_before gid_succ graph
    | None -> append gid graph

  (* -------------------------------------------------------------------------------- *)
  let add_unordered graph =
    let new_gid = graph.highest_index + 1 in
    let map = Gid_map.add new_gid G_node.empty graph.map in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  (* build a new map when one node is remove from the list of ordered nodes:
     1) shift position of nodes after [node]
     2) update pred and succ edges
  *)
  let map_unorder node_id map =
    let node = Gid_map.find node_id map in
    match (G_node.get_pred_opt node, G_node.get_succ_opt node) with
    | (Some pred_id, Some succ_id) ->
      map
      |> (shift_position (-1) succ_id)
      |> (map_del_pred_succ pred_id node_id)
      |> (map_del_pred_succ node_id succ_id)
      |> (map_add_pred_succ pred_id succ_id)
    | (Some pred_id, None) ->
      map
      |> (map_del_pred_succ pred_id node_id)
    | (None, Some succ_id) ->
      map
      |> (shift_position (-1) succ_id)
      |> (map_del_pred_succ node_id succ_id)
    | (None, None) ->
      map

  (* -------------------------------------------------------------------------------- *)
  let unorder_opt node_id graph =
    match Gid_map.find_opt node_id graph.map with
    | None -> None
    | Some node ->
      match G_node.get_position_opt node with
      | None -> None
      | Some _ ->
        let new_map =
          graph.map
          |> (Gid_map.add node_id (G_node.unset_position node))
          |> (map_unorder node_id) in
        Some { graph with map = new_map }

  (* -------------------------------------------------------------------------------- *)
  let del_node_opt node_id graph =
    match Gid_map.find_opt node_id graph.map with
    | None -> None
    | Some node ->
      let new_map =
        Gid_map.fold
          (fun id value acc ->
             if id = node_id
             then acc
             else Gid_map.add id (G_node.remove_key node_id value) acc
          ) (map_unorder node_id graph.map) Gid_map.empty in
      Some { graph with map = new_map }

  (* -------------------------------------------------------------------------------- *)
  (* move out-edges (which respect [label_csl]) from [src_gid] are moved to out-edges out off node [tar_gid] *)
  let shift_out ~config loc src_gid tar_gid is_gid_local label_cst graph =
    let del_edges = ref [] and add_edges = ref [] in

    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    let src_next = G_node.get_next src_node in
    let tar_next = G_node.get_next tar_node in

    let (new_src_next, new_tar_next) =
      Massoc_gid.fold
        (fun (acc_src_next,acc_tar_next) next_gid edge ->
           if Label_cst.match_ ~config label_cst edge && not (is_gid_local next_gid)
           then
             match Massoc_gid.add_opt next_gid edge acc_tar_next with
             | None when !Global.safe_commands -> Error.run ~loc "The [shift_out] command tries to build a duplicate edge (with label \"%s\")" (G_edge.dump ~config  edge)
             | None ->
               del_edges := (src_gid,edge,next_gid) :: !del_edges;
               (Massoc_gid.remove next_gid edge acc_src_next, acc_tar_next)
             | Some new_acc_tar_next ->
               del_edges := (src_gid,edge,next_gid) :: !del_edges;
               add_edges := (tar_gid,edge,next_gid) :: !add_edges;
               (Massoc_gid.remove next_gid edge acc_src_next, new_acc_tar_next)
           else (acc_src_next,acc_tar_next)
        )
        (src_next, tar_next) src_next in

    let new_map =
      graph.map
      |> (Gid_map.add src_gid (G_node.set_next new_src_next src_node))
      |> (Gid_map.add tar_gid (G_node.set_next new_tar_next tar_node)) in

    ( { graph with map = new_map }, !del_edges, !add_edges )

  (* -------------------------------------------------------------------------------- *)
  let shift_in ~config loc src_gid tar_gid is_gid_local label_cst graph =
    let del_edges = ref [] and add_edges = ref [] in
    let new_map =
      Gid_map.mapi
        (fun node_id node ->
           if is_gid_local node_id (* shift does not move pattern edges *)
           then node
           else
             let node_next = G_node.get_next node in
             match Massoc_gid.assoc src_gid node_next with
             | [] -> node (* no edges from node to src *)
             | node_src_edges ->
               let node_tar_edges = Massoc_gid.assoc tar_gid node_next in
               let (new_node_src_edges, new_node_tar_edges) =
                 List.fold_left
                   (fun (acc_node_src_edges,acc_node_tar_edges) edge ->
                      if Label_cst.match_ ~config label_cst edge
                      then
                        match List_.usort_insert_opt edge acc_node_tar_edges with
                        | None when !Global.safe_commands ->
                          Error.run ~loc "The [shift_in] command tries to build a duplicate edge (with label \"%s\")" (G_edge.dump ~config edge)
                        | None ->
                          del_edges := (node_id,edge,src_gid) :: !del_edges;
                          (List_.usort_remove edge acc_node_src_edges, acc_node_tar_edges)
                        | Some l ->
                          del_edges := (node_id,edge,src_gid) :: !del_edges;
                          add_edges := (node_id,edge,tar_gid) :: !add_edges;
                          (List_.usort_remove edge acc_node_src_edges, l)
                      else (acc_node_src_edges,acc_node_tar_edges)
                   )
                   (node_src_edges, node_tar_edges) node_src_edges in
               let new_next =
                 node_next
                 |> (Massoc_gid.replace src_gid new_node_src_edges)
                 |> (Massoc_gid.replace tar_gid new_node_tar_edges) in
               G_node.set_next new_next node
        ) graph.map in
    ( { graph with map = new_map },
      !del_edges,
      !add_edges
    )

  (* -------------------------------------------------------------------------------- *)
  let shift_edges ~config loc src_gid tar_gid is_gid_local label_cst graph =
    let (g1,de1,ae1) = shift_out ~config loc src_gid tar_gid is_gid_local label_cst graph in
    let (g2,de2,ae2) = shift_in ~config loc src_gid tar_gid is_gid_local label_cst g1 in
    (g2, de1 @ de2, ae1 @ ae2)

  (* -------------------------------------------------------------------------------- *)
  let update_feat ?loc graph node_id feat_name new_value =
    let node = Gid_map.find node_id graph.map in
    let new_fs = G_fs.set_value ?loc feat_name new_value (G_node.get_fs node) in
    let new_node = G_node.set_fs new_fs node in
    { graph with map = Gid_map.add node_id new_node graph.map }

  (* -------------------------------------------------------------------------------- *)
  let append_feats_opt ?loc graph src_id tar_id separator regexp =
    let src_node = Gid_map.find src_id graph.map in
    let tar_node = Gid_map.find tar_id graph.map in
    match G_node.append_feats_opt ?loc src_node tar_node separator regexp with
    | Some (new_tar_node, updated_feats) ->
      Some ({ graph with map = Gid_map.add tar_id new_tar_node graph.map }, updated_feats)
    | None -> None

  (* -------------------------------------------------------------------------------- *)
  let del_feat_opt graph node_id feat_name =
    let node = Gid_map.find node_id graph.map in
    match G_fs.del_feat_opt feat_name (G_node.get_fs node) with
    | Some new_fs -> Some { graph with map = Gid_map.add node_id (G_node.set_fs new_fs node) graph.map }
    | None -> None

  (* -------------------------------------------------------------------------------- *)
  let to_json_python ~config graph =
    let gr_id id = G_node.get_name id (Gid_map.find id graph.map) in

    let nodes = Gid_map.fold
        (fun id node acc ->
           let node_id = gr_id id
           and fs = G_node.get_fs node
           and succ =
             Massoc_gid.fold
               (fun acc tar edge ->
                  match G_edge.to_string_opt ~config edge with
                  | None -> acc
                  | Some s -> (`List [`String s; `String (gr_id tar)]) :: acc
               ) [] (G_node.get_next node) in
           (node_id,`List [G_fs.to_json_python fs; `List succ])::acc
        ) graph.map [] in

    `Assoc nodes

  (* -------------------------------------------------------------------------------- *)
  let to_gr ~config graph =
    let buff = Buffer.create 32 in
    bprintf buff "graph {\n";

    (* meta data *)
    List.iter (fun (key, value) -> bprintf buff "# %s = \"%s\";\n" key value) graph.meta;

    (* node_list *)
    let (orderd_nodes, non_ordered_nodes) =
      Gid_map.fold
        (fun gid node (acc_on, acc_non) ->
           match G_node.get_position_opt node with
           | None -> (acc_on, (gid,node)::acc_non)
           | Some _ -> ((gid,node)::acc_on, acc_non)
        ) graph.map ([],[]) in
    let sorted_ordered_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) orderd_nodes in

    let id_of_gid gid =
      match CCList.find_idx (fun (x,_) -> x=gid) sorted_ordered_nodes with
      | Some (i,_) -> sprintf "W%d" i
      | None -> sprintf "N%d" gid in

    let nodes = Gid_map.fold (fun gid node acc -> (gid,node)::acc) graph.map [] in
    let sorted_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) nodes in
    List.iter
      (fun (gid,node) ->
         bprintf buff "  %s %s;\n" (id_of_gid gid) (G_node.to_gr node)
      ) sorted_nodes;

    (* edges *)
    List.iter
      (fun (src_gid,node) ->
         Massoc_gid.iter
           (fun tar_gid edge ->
              match G_edge.to_string_opt ~config edge with
              | Some s -> bprintf buff "  %s -[%s]-> %s;\n" (id_of_gid src_gid) s (id_of_gid tar_gid)
              | None -> ()
           ) (G_node.get_next node)
      ) sorted_nodes;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let space_after node =
    match G_fs.get_value_opt "SpaceAfter" (G_node.get_fs node) with
    | Some (String "No") -> false
    | _ -> true

  let to_sentence ?pivot ?(deco=G_deco.empty) graph =
    let high_list = match pivot with
      | None -> List.map fst deco.nodes
      | Some pivot ->
        match List.find_opt (fun (gid, (pattern_id,_)) -> pattern_id = pivot) deco.nodes with
        | None -> Error.run "Undefined pivot %s" pivot
        | Some (gid,_) -> [gid] in

    let is_highlighted_gid gid = List.mem gid high_list in

    let exception Find of Gid.t in

    let init_gid_opt =
      try Gid_map.iter
            (fun gid node ->
               match (G_node.get_pred_opt node, G_node.get_succ_opt node) with
               | (None, Some _) -> raise (Find gid)
               | _ -> ()
            ) graph.map; None
      with Find node -> Some node in

    match init_gid_opt with
    | None ->
      begin
        match get_meta_opt "text" graph with
        | Some t -> t
        | None -> "*** Cannot find text metadata ***"
      end
    | Some init_gid ->
      let buff = Buffer.create 32 in
      let rec loop current_form flag_highlight flag_sa gid =
        let to_buff = function
          | (None, _, _) -> ()
          | (Some s, false, false) -> bprintf buff "%s" s
          | (Some s, false, true) -> bprintf buff "%s " s
          | (Some s, true, false) -> bprintf buff "<span class=\"highlight\">%s</span>" s
          | (Some s, true, true) -> bprintf buff "<span class=\"highlight\">%s</span> " s
        in
        let node = Gid_map.find gid graph.map in
        let fs = G_node.get_fs node in
        let (new_current_form, new_flag_highlight, new_flag_sa) =
          match (G_fs.get_value_opt "textform" fs, G_fs.get_value_opt "form" fs) with
          | (Some (String "_"),_) -> (current_form, flag_highlight || (is_highlighted_gid gid), space_after node)
          | (None, Some (String "__0__")) -> (current_form, flag_highlight, false)
          | (Some (String form), _)
          | (None, Some (String form)) ->
            let form = match form with "UNDERSCORE" -> "_" | x -> x in (* '_' is escaped in textform to avoid ambiguity with textform=_ in multi-word tokens *)
            to_buff (current_form, flag_highlight, flag_sa); (Some form, is_highlighted_gid gid, space_after node)
          | _ -> (current_form, flag_highlight, space_after node) in
        match G_node.get_succ_opt node with
        | Some succ_gid -> loop new_current_form new_flag_highlight new_flag_sa succ_gid
        | None -> to_buff (new_current_form, new_flag_highlight, new_flag_sa) in
      loop None false true init_gid;
      Buffer.contents buff

  let start_dur gnode =
    let fs = G_node.get_fs gnode in
    match (G_fs.get_value_opt "_start" fs, G_fs.get_value_opt "_stop" fs) with
    | (Some (Float start), (Some Float stop)) -> (start, stop -. start)
    | _ -> (-1., -1.) (* TODO: exception *)


  let to_orfeo ?(deco=G_deco.empty) graph =
    let is_highlighted_gid gid = List.mem_assoc gid deco.nodes in

    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) nodes in

    let buff = Buffer.create 32 in
    CCList.iteri (fun i (gid, gnode) ->
        match G_fs.to_word_opt (G_node.get_fs gnode) with
        | None -> ()
        | Some word ->
          let (start, dur) = start_dur gnode in
          Printf.bprintf buff
            "<span id=\"tok%d\" data-dur=\"%g\" data-begin=\"%g\" tabindex=\"0\" data-index=\"%d\" %s>%s </span>"
            i dur start i
            (match i, is_highlighted_gid gid with
             | (1, true) -> "class=\"speaking highlight\""
             | (1, false) -> "class=\"speaking\""
             | (_, true) -> "class=\"highlight\""
             | (_, false) -> ""
            )
            word
      ) snodes;

    let bounds =
      match (CCList.nth_opt snodes 1, CCList.last_opt snodes) with (* 0 is the "conll root node" *)
      | (Some (_,node1), Some (_,node2)) ->
        begin
          match (G_fs.get_value_opt "_start" (G_node.get_fs node1), G_fs.get_value_opt "_stop" (G_node.get_fs node2)) with
          | (Some (Float i), Some (Float f)) -> Some (i,f)
          | _ -> None
        end
      | _ -> None in

    (Buffer.contents buff, bounds)


  (* -------------------------------------------------------------------------------- *)
  let to_dep ?filter ?main_feat ?(deco=G_deco.empty) ~config graph =

    (* split lexical // non-lexical nodes *)
    let (lexical_nodes, non_lexical_nodes) = Gid_map.fold
        (fun gid node (acc1, acc2) ->
           match G_node.get_position_opt node with
           | None -> (acc1, (gid,node)::acc2)
           | Some _ -> ((gid,node)::acc1, acc2)
        ) graph.map ([],[]) in

    let sorted_lexical_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) lexical_nodes in

    let insert (gid,node) nodes =
      match (G_fs.get_value_opt "parseme" (G_node.get_fs node), G_fs.get_value_opt "frsemcor" (G_node.get_fs node)) with
      | (None, None) -> (* dmrs node *)
        (gid,node) :: nodes
      | _ -> (* parseme / frsemcor node --> place it before its first lexical item *)
        let next_ids = Massoc_gid.fold (fun acc gid _ -> gid::acc) [] (G_node.get_next node) in
        let rec loop = function
          | [] -> [(gid,node)]
          | (h,n)::t when List.mem h next_ids -> (gid,node)::(h,n)::t
          | h::t -> h :: (loop t) in
        loop nodes in

    (* insertion of non lexical node to a sensible position in the linear order *)
    let nodes = List.fold_left (
        fun acc gid_node -> insert gid_node acc
      ) sorted_lexical_nodes non_lexical_nodes in

    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { opacity=0; scale = 200; fontname=\"Arial\"; }\n";
    bprintf buff "[WORDS] { \n";

    (* nodes *)
    List.iter
      (fun (id, node) ->
         let decorated_feat = try List.assoc id deco.G_deco.nodes with Not_found -> ("",[]) in
         let fs = G_node.get_fs node in

         let tail =
           match (!Global.debug, G_node.get_position_opt node) with
           | (true, Some pos) -> [sprintf "position=%d:B:lightblue" pos]
           | _ -> [] in

         let dep_fs = G_fs.to_dep ~decorated_feat ~tail ?filter ?main_feat fs in

         let style = match G_fs.get_value_opt "void" fs with
           | Some (String "y") -> "; forecolor=red; subcolor=red; "
           | _ -> match G_fs.get_value_opt "wordform" fs with
             | Some (String "__EMPTY__") -> "; forecolor=purple; subcolor=purple; "
             | _ -> "" in

         bprintf buff "N_%s { %s%s }\n"
           (Gid.to_string id)
           dep_fs
           style
      ) nodes;
    bprintf buff "} \n";

    (* edges *)
    bprintf buff "[EDGES] { \n";

    if !Global.debug
    then
      List.iter
        (fun (id, node) ->
           begin
             match G_node.get_pred_opt node with
             | None -> ()
             | Some p -> bprintf buff "N_%s -> N_%s { label=\"__PREC__\"; bottom; style=dot; color=lightblue; forecolor=lightblue; }\n" (Gid.to_string id) (Gid.to_string p)
           end;
           begin
             match G_node.get_succ_opt node with
             | None -> ()
             | Some s -> bprintf buff "N_%s -> N_%s { label=\"__SUCC__\"; bottom; style=dot; color=lightblue; forecolor=lightblue; }\n" (Gid.to_string id) (Gid.to_string s)
           end
        ) snodes;

    Gid_map.iter
      (fun gid elt ->
         Massoc_gid.iter
           (fun tar g_edge ->
              let deco = List.mem (gid,g_edge,tar) deco.G_deco.edges in
              match G_edge.to_dep_opt ~deco ~config g_edge with
              | None -> ()
              | Some string_edge -> bprintf buff "N_%s -> N_%s %s\n" (Gid.to_string gid) (Gid.to_string tar) string_edge
           ) (G_node.get_next elt)
      ) graph.map;

    bprintf buff "} \n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let list_num test =
    let rec loop n = function
      | [] -> raise Not_found
      | x::_ when test x -> n
      | _::t -> loop (n+1) t
    in loop 0

  (* -------------------------------------------------------------------------------- *)
  let to_dot ?main_feat ?(deco=G_deco.empty) ~config graph =
    let buff = Buffer.create 32 in

    bprintf buff "digraph G {\n";
    bprintf buff "  node [shape=box];\n";

    (* nodes *)
    Gid_map.iter
      (fun id node ->
         let decorated_feat =
           try List.assoc id deco.G_deco.nodes
           with Not_found -> ("",[]) in
         let fs = G_node.get_fs node in

         match G_fs.to_dot ~decorated_feat ?main_feat fs with
         | "" -> bprintf buff "  N_%s [label=\"\"]\n" (Gid.to_string id)
         | s -> bprintf buff "  N_%s [label=<%s>]\n"
                  (Gid.to_string id)
                  s
      ) graph.map;

    (* edges *)
    Gid_map.iter
      (fun id node ->
         Massoc_gid.iter
           (fun tar g_edge ->
              let deco = List.mem (id,g_edge,tar) deco.G_deco.edges in
              if g_edge = G_edge.sub
              then bprintf buff "  N_%s -> N_%s [dir=none];\n" (Gid.to_string id) (Gid.to_string tar)
              else
                match G_edge.to_dot_opt ~config ~deco g_edge with
                | None -> ()
                | Some string_edge -> bprintf buff "  N_%s -> N_%s%s;\n" (Gid.to_string id) (Gid.to_string tar) string_edge
           ) (G_node.get_next node)
      ) graph.map;

    (* Set "rank=same" and more edge in debug modes *)
    Gid_map.iter
      (fun id node ->
         begin
           match G_node.get_succ_opt node with
           | Some s when !Global.debug ->
             bprintf buff "  N_%s -> N_%s [label=\"SUCC\", style=dotted, fontcolor=lightblue, color=lightblue]; {rank=same; N_%s; N_%s };\n"
               (Gid.to_string id) (Gid.to_string s) (Gid.to_string id) (Gid.to_string s)
           | Some s -> bprintf buff " {rank=same; N_%s; N_%s };\n" (Gid.to_string id) (Gid.to_string s)
           | _ -> ()
         end
      ) graph.map;

    bprintf buff "}\n";
    Buffer.contents buff

  let get_feature_values feature_name t =
    Gid_map.fold
      (fun _ node acc ->
         match G_fs.get_value_opt feature_name (G_node.get_fs node) with
         | None -> acc
         | Some v -> String_set.add (string_of_value v) acc
      ) t.map String_set.empty

  let get_relations ~config t =
    Gid_map.fold
      (fun _ node acc ->
         Massoc_gid.fold_on_list
           (fun acc2 key edges ->
              List.fold_left
                (fun acc3 edge ->
                   match G_edge.to_string_opt ~config edge with
                   | None -> acc3
                   | Some e -> String_set.add e acc3
                ) acc2 edges
           ) acc (G_node.get_next node)
      ) t.map String_set.empty

  let get_features t =
    Gid_map.fold
      (fun _ node acc ->
         String_set.union
           (G_fs.get_features (G_node.get_fs node))
           acc
      ) t.map String_set.empty

  let is_projective t =
    let (arc_positions, pos_to_gid_map) =
      Gid_map.fold (fun src_gid src_node (acc, acc_map) ->
          match G_node.get_position_opt src_node with
          | None -> (acc, acc_map)
          | Some src_position ->
            let new_acc = Massoc_gid.fold (fun acc2 tar_gid edge ->
                let tar_node = find tar_gid t in
                match G_node.get_position_opt tar_node with
                | None -> acc2
                | Some tar_position -> (min src_position tar_position, max src_position tar_position) :: acc2
              ) acc (G_node.get_next_without_pred_succ_enhanced src_node) in
            (new_acc, Int_map.add src_position src_gid acc_map)
        ) t.map ([], Int_map.empty) in
    Dependencies.is_projective arc_positions

  (* --------------------------------------------------------------- *)
  (* Detection of graph structure: cycle, tree, …
     function [depth_first_search] implemented following:
     http://algorithmics.lsi.upc.edu/docs/Dasgupta-Papadimitriou-Vazirani.pdf (chap 3, pp 87-107)
     The algorithm is modified:
     - first search the roots
     - then apply the depth_first_search starting from these roots
       Tree detection is easier (is_tree <=> back_edges=0 and nontree_edges=0
  *)
  (* --------------------------------------------------------------- *)
  let dfs_debug = false

  let get_roots graph =
    let non_roots =
      Gid_map.fold
        (fun gid node acc ->
           Massoc_gid.fold_on_list (
             fun acc2 next_gid _ ->
               if dfs_debug then printf " %s ---> %s\n%!" (Gid.to_string gid) (Gid.to_string next_gid);
               Gid_set.add next_gid acc2
           ) acc (G_node.get_next_without_pred_succ_enhanced node)
        ) graph.map Gid_set.empty in
    let roots =
      Gid_map.fold
        (fun gid _ acc ->
           if Gid_set.mem gid non_roots
           then acc
           else Gid_set.add gid acc
        ) graph.map Gid_set.empty in
    roots

  type dfs_node =
    | Pre of int
    | Pre_post of int * int

  type dfs_info = {
    intervals: dfs_node Gid_map.t;
    back_edges: (Gid.t * Gid.t) list;
    nontree_edges: (Gid.t * Gid.t) list;
  }

  type dfs_output = {
    forest: bool;
    tree: bool;
    cyclic: bool;
  }

  let depth_first_search graph =
    let info = ref {intervals=Gid_map.empty; back_edges=[]; nontree_edges=[];} in
    let clock = ref 0 in

    let rec explore gid =
      info := {!info with intervals = Gid_map.add gid (Pre !clock) !info.intervals};
      incr clock;
      let node = Gid_map.find gid graph.map in
      Massoc_gid.iter (fun next_gid edge ->
          try
            match Gid_map.find next_gid !info.intervals with
            | Pre _ -> info := {!info with back_edges = (gid, next_gid) :: !info.back_edges};
            | Pre_post _ -> info := {!info with nontree_edges = (gid, next_gid) :: !info.nontree_edges};
          with
          | Not_found -> explore next_gid
        ) (G_node.get_next_without_pred_succ_enhanced node);
      match Gid_map.find_opt gid !info.intervals with
      | Some (Pre i) -> info := {!info with intervals = Gid_map.add gid (Pre_post (i,!clock)) !info.intervals}; incr clock;
      | _ -> assert false in

    let roots = get_roots graph in
    let nb_roots = Gid_set.cardinal roots in
    if dfs_debug then Printf.printf "|roots| = %d\n" nb_roots;
    Gid_set.iter (fun gid ->
        if dfs_debug then Printf.printf "  -----> explore %s\n" (Gid.to_string gid);
        explore gid
      ) roots;

    if dfs_debug then
      begin
        Printf.printf "======== Intervals =======\n";
        Gid_map.iter (fun gid node ->
            match Gid_map.find_opt gid !info.intervals with
            | None -> Printf.printf "None! %s" (Gid.to_string gid)
            | Some (Pre _) -> Printf.printf "Pre! %s" (Gid.to_string gid)
            | Some (Pre_post (i,j)) ->
              Printf.printf "%s --> [%d,%d] --> %s\n" (Gid.to_string gid) i j
                (G_fs.to_string (G_node.get_fs node))
          ) graph.map;

        Printf.printf "======== Back_edges =======\n";
        List.iter (fun (gid1, gid2) ->
            Printf.printf "%s --> %s\n" (Gid.to_string gid1) (Gid.to_string gid2)
          ) !info.back_edges;

        Printf.printf "======== nontree_edges =======\n";
        List.iter (fun (gid1, gid2) ->
            Printf.printf "%s --> %s\n" (Gid.to_string gid1) (Gid.to_string gid2)
          ) !info.nontree_edges
      end;

    if Gid_map.cardinal !info.intervals < Gid_map.cardinal graph.map
    then
      begin
        if dfs_debug then printf "Not covered\n%!";
        { forest = false; tree = false; cyclic = true}
      end
    else
      {
        forest = !info.nontree_edges = [] && !info.back_edges = [];
        tree = !info.nontree_edges = [] && !info.back_edges = [] && nb_roots = 1;
        cyclic = !info.back_edges <> [];
      }

end (* module G_graph *)



(* ================================================================================ *)
(* The module [Delta] defines a type for recording the effect of a set of commands on a graph *)
(* It is used as key to detect egal graphs based on rewriting history *)
module Delta = struct
  type status = Add | Del

  exception Inconsistent of string

  (* the three list are ordered *)
  type t = {
    del_nodes: Gid.t list;
    unordered_nodes: Gid.t list;
    edges: ((Gid.t * G_edge.t * Gid.t) * status) list;
    feats: ((Gid.t * feature_name) * (feature_value option)) list;
  }

  let empty = { del_nodes=[]; unordered_nodes=[]; edges=[]; feats=[]; }

  let del_node gid t =
    match List_.usort_insert_opt gid t.del_nodes with
    | None -> raise (Inconsistent "del_node")
    | Some new_del_nodes -> {
        del_nodes= new_del_nodes;
        unordered_nodes = List.filter (fun g -> g <> gid) t.unordered_nodes;
        edges = List.filter (fun ((g1,_,g2),_) -> g1 <> gid && g2 <> gid) t.edges;
        feats = List.filter (fun ((g,_),_) -> g <> gid) t.feats;
      }

  let add_edge src lab tar t =
    let rec loop = fun old -> match old with
      | []                                                 -> ((src,lab,tar),Add)::old
      | ((s,l,t),stat)::tail when (src,lab,tar) < (s,l,t)  -> ((src,lab,tar),Add)::old
      | ((s,l,t),stat)::tail when (src,lab,tar) > (s,l,t)  -> ((s,l,t),stat)::(loop tail)
      | ((s,l,t), Add)::tail (* (src,lab,tar) = (s,l,t) *) -> raise (Inconsistent "add_edge")
      | ((s,l,t), Del)::tail (* (src,lab,tar) = (s,l,t) *) -> tail in
    { t with edges = loop t.edges }

  let del_edge src lab tar t =
    let rec loop = fun old -> match old with
      | []                                                 -> ((src,lab,tar),Del)::old
      | ((s,l,t),stat)::tail when (src,lab,tar) < (s,l,t)  -> ((src,lab,tar),Del)::old
      | ((s,l,t),stat)::tail when (src,lab,tar) > (s,l,t)  -> ((s,l,t),stat)::(loop tail)
      | ((s,l,t), Del)::tail (* (src,lab,tar) = (s,l,t) *) -> raise (Inconsistent "del_edge")
      | ((s,l,t), Add)::tail (* (src,lab,tar) = (s,l,t) *) -> tail in
    { t with edges = loop t.edges }

  let set_feat seed_graph gid feat_name new_val_opt t =
    (* equal_orig is true iff new val is the same as the one in seed_graph *)
    let equal_orig =
      try (new_val_opt = G_fs.get_value_opt feat_name (G_node.get_fs (G_graph.find gid seed_graph)))
      with Not_found -> false (* when gid is in created nodes *) in
    let rec loop = fun old -> match old with
      | [] when equal_orig                                             -> []
      | []                                                             -> [(gid,feat_name), new_val_opt]
      | ((g,f),_)::tail when (gid,feat_name) < (g,f) && equal_orig     -> old
      | ((g,f),v)::tail when (gid,feat_name) < (g,f)                   -> ((gid,feat_name), new_val_opt)::old
      | ((g,f),v)::tail when (gid,feat_name) > (g,f)                   -> ((g,f),v)::(loop tail)
      | ((g,f),_)::tail when (* (g,f)=(gid,feat_name) && *) equal_orig -> tail
      | ((g,f),_)::tail (* when (g,f)=(gid,feat_name) *)               -> ((g,f), new_val_opt) :: tail in
    { t with feats = loop t.feats }

  let unorder gid t =
    match List_.usort_insert_opt gid t.unordered_nodes with
    | None -> raise (Inconsistent "unorder")
    | Some new_unordered_nodes -> { t with unordered_nodes = new_unordered_nodes }
end (* module Delta *)

(* ================================================================================ *)
module Graph_with_history = struct
  type t = {
    seed: G_graph.t;
    delta: Delta.t;
    graph: G_graph.t;
    added_gids: (string * Gid.t) list;
    e_mapping: (Gid.t * G_edge.t * Gid.t) String_map.t;
    added_gids_in_rule: (string * Gid.t) list;
    added_edges_in_rule: (Gid.t * G_edge.t * Gid.t) String_map.t;
  }

  let from_graph graph = { graph; seed=graph; delta = Delta.empty; added_gids = []; e_mapping = String_map.empty; added_gids_in_rule =[]; added_edges_in_rule=String_map.empty}

  (* WARNING: compare is correct only on data with the same seed! *)
  let compare t1 t2 = Stdlib.compare (t1.delta,t1.added_gids) (t2.delta, t2.added_gids)
end (* module Graph_with_history*)

(* ================================================================================ *)
module Graph_with_history_set = Set.Make (Graph_with_history)
