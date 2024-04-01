(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

open Grew_types
open Grew_ast
open Grew_utils

open Grew_edge
open Grew_fs
open Grew_node

(* ================================================================================ *)
module P_graph = struct
  type t = P_node.t Pid_map.t

  let empty = Pid_map.empty

  let find = Pid_map.find

  let set_node = Pid_map.add 

  let pid_name_list t = Pid_map.fold (fun _ node acc -> (P_node.get_name node)::acc) t []

  (** [get_name pid p_graph_list] returns the name of node with [pid] in the first [p_graph] in the list for which [pid] is defined *)
  let rec get_name pid = function
    | [] -> Error.run "inconsistent data in P_graph: pid `%s` not found" (Pid.to_string pid)
    | p_graph :: tail -> 
      match Pid_map.find_opt pid p_graph with
      | Some node -> P_node.get_name node
      | None -> get_name pid tail

  let is_injective pid graph_list =
    let pid_name = get_name pid graph_list in
    String.get pid_name ((String.length pid_name) - 1) <> '$'

    let _dump bases t =
    printf "============ P_graph._dump ===========\n";
      Pid_map.iter
        (fun pid _ -> 
          printf "pid=%s   name=%s    node=…\n%!" (Pid.to_string pid) (get_name pid bases)
        ) t;
    printf "///========= P_graph._dump ========///\n"

    
  let to_json_list ~config ?(base=empty) t =
    let local_get_name pid = get_name pid [base; t] in
    let nodes = Pid_map.fold 
      (fun _ n acc ->
        if P_node.is_empty n
        then acc
        else 
          (`String 
            (sprintf "%s %s" 
              (P_node.get_name n)
              (P_node.get_fs_disj n |> List.map P_fs.to_string |> List.map (sprintf "[%s]") |> String.concat "|")
            )
          ) :: acc
      ) t [] in
    let full = Pid_map.fold 
      (fun k n acc -> 
        let next = P_node.get_next n in
        Pid_massoc.fold 
          (fun acc2 pid_tar edge ->
            match P_edge.to_id_opt_and_string ~config edge with
            | (_,"__PRED__") -> acc2
            | (_,"__SUCC__") -> (`String (sprintf "%s < %s" (local_get_name k) (local_get_name pid_tar))) :: acc2
            | (None, "^") -> (* Because no constraint is expressed as "not + empty set" *) 
                (`String (sprintf "%s -> %s" (local_get_name k) (local_get_name pid_tar))) :: acc2
            | (None, e) ->
                (`String (sprintf "%s -[%s]-> %s" (local_get_name k) e (local_get_name pid_tar))) :: acc2
            | (Some id, "^") -> (* Because no constraint is expressed as "not + empty set" *) 
                (`String (sprintf "%s: %s -> %s" id (local_get_name k) (local_get_name pid_tar))) :: acc2
            | (Some id, e) -> 
                (`String (sprintf "%s: %s -[%s]-> %s" id (local_get_name k) e (local_get_name pid_tar))) :: acc2
          ) acc next
      ) t nodes in
    full


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
    let full_node_list = basic_ast.Ast.req_nodes
    and full_edge_list = basic_ast.Ast.req_edges in

    let nodes_id = List.map (fun ({Ast.node_id; _},_) -> node_id) full_node_list in
    let edges_id = CCList.filter_map (fun ({Ast.edge_id; _},_) -> edge_id) full_edge_list in

    begin
      match List_.intersect nodes_id edges_id with
      | [] -> ()
      | id::_ ->
        let loc = snd (List.find (fun ({Ast.node_id; _},_) -> node_id = id) full_node_list) in
        Error.build ~loc "The same identifier `%s` cannot be used both as a node identifier and as an edge identifier" id
    end;

    (* NB: insert searches for a previous node with the same name and uses unification rather than constraint *)
    (* NB: insertion of new node at the end of the list: not efficient but graph building is not the hard part. *)
    let rec insert (ast_node, loc) = function
      | [] -> [P_node.of_ast lexicons (ast_node, loc)]
      | (node_id,p_node)::tail when ast_node.Ast.node_id = node_id ->
        begin
          try 
          (node_id, P_node.unif_fs_disj (List.map (P_fs.of_ast lexicons) ast_node.Ast.fs_disj) p_node) :: tail
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
    let map_without_edges = CCList.foldi
        (fun acc i elt -> Pid_map.add (Pid.Ker i) elt acc)
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
      CCList.foldi
        (fun acc i elt -> Pid_map.add (Pid.Ext i) elt acc)
        Pid_map.empty
        new_node_list in

    let filter_on_ker_nodes =
      List.fold_left
        (fun acc (id,node) ->
           let ker_pid = Pid.Ker (Array_.dicho_find id ker_table) in
           let p_fs_disj = P_node.get_fs_disj node in
           match Pid_map.find_opt ker_pid acc with
           | None -> Pid_map.add ker_pid p_fs_disj acc
           | Some old_p_fs_disj -> Pid_map.add ker_pid (P_fs.unif_disj old_p_fs_disj p_fs_disj) acc
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
            begin
              match map_add_edge_opt src_pid P_edge.succ tar_pid acc_map with
              | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the order edge"
              | Some acc2 -> 
                begin
                  match map_add_edge_opt tar_pid P_edge.pred src_pid acc2 with
                  | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the order edge"
                  | Some m -> (m, acc_edge_ids)
                end
            end
          | _ ->
            let edge = P_edge.of_ast ~config (ast_edge, loc) in
            match map_add_edge_opt src_pid edge tar_pid acc_map with
            | Some m -> (m, match ast_edge.Ast.edge_id with Some id -> id::acc_edge_ids | None -> acc_edge_ids)
            | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the same edge %s"
                          (P_edge.to_string ~config edge)
        ) (ext_map_without_edges, edge_ids) full_edge_list in

        (ext_map_with_all_edges, filter_on_ker_nodes, ext_table, new_edge_ids)

  (* -------------------------------------------------------------------------------- *)

  (* return the list of [pid] of nodes without in edges (Pred & Succ are taken into account) *)
  let roots (p_graph: t) =
    let not_root_set =
      Pid_map.fold
        (fun _ p_node acc ->
          Pid_massoc.fold
            (fun acc2 tar_pid _ -> 
              Pid_set.add tar_pid acc2
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
  (* value is (f, Some g) for combined clause "f=v/g=u" and (j, None) else *)
  type highlighted_feat = string * string option

  type t = {
    (* a list of (node, (pid, features of nodes implied in the step)) *)
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
    rules: string list;
    trace: trace_item option;     (* if the rewriting history is kept *)
    impact: G_deco.t;
  }

  let get_meta_opt key t = List.assoc_opt key t.meta

  let get_meta_list t = t.meta

  let set_meta key value t = {t with meta = (key,value) :: List.remove_assoc key t.meta}

  let empty = { meta=[]; map=Gid_map.empty; highest_index=0; rules=[]; trace=None; impact=G_deco.empty }

  let is_initial g = g.rules = []

  let size t = Gid_map.cardinal (t.map)

  let find node_id graph = Gid_map.find node_id graph.map
  let find_opt node_id graph = Gid_map.find_opt node_id graph.map

  let node_exists fct t = Gid_map.exists (fun _ node -> fct node) t.map

  let subgraph graph seed depth =
    let todo_init = List.fold_left (fun acc gid -> Gid_map.add gid depth acc) Gid_map.empty seed in
    let rec loop (todo, ok) = 
      match Gid_map.choose_opt todo with
      | None -> ok
      | Some (gid, depth) ->
        let node = find gid graph in
        let next = G_node.get_next node in
        let (new_todo, new_ok) =
          if depth = 0
          then (Gid_map.remove gid todo, Gid_set.add gid ok)
          else 
            let tmp_ok = Gid_set.add gid ok in
            let tmp_todo = 
              Gid_massoc.fold_on_list 
                (fun acc gid' _ ->
                  if (Gid_set.mem gid' tmp_ok) || (Gid_map.mem gid' todo)
                  then acc
                  else Gid_map.add gid' (depth-1) acc
                ) (Gid_map.remove gid todo) next in
            (tmp_todo, tmp_ok) in
        loop (new_todo, new_ok) in 
    let selected_nodes = loop (todo_init, Gid_set.empty) in
    let sub_map = Gid_set.fold 
        (fun gid acc ->
          let node = find gid graph in
          let new_next = Gid_massoc.filter_key (fun gid -> Gid_set.mem gid selected_nodes) (G_node.get_next node) in
          let new_node = G_node.set_next new_next node in
          Gid_map.add gid new_node acc
        ) selected_nodes Gid_map.empty in

    {empty with map= sub_map}

  let fold_gid fct t init =
    Gid_map.fold (fun gid _ acc -> fct gid acc) t.map init

  let fold_node fct t init =
    Gid_map.fold (fun _ node acc -> fct node acc) t.map init

  let track_rules (rule_name,_) t =
    if !Global.track_rules
    then
      { t with rules = rule_name :: t.rules }
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

  let clear_rules t = { t with rules = [] }

  (* is there an edge e out of node i ? *)
  let edge_out ~config graph node_id label_cst =
    let node = Gid_map.find node_id graph.map in
    Gid_massoc.exists (fun _ e -> Label_cst.match_ ~config label_cst e) (G_node.get_next node)

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
  let map_del_edge src_gid label tar_gid map =
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
  let of_json (json: Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    let meta =
      try json |> member "meta" |> to_assoc 
          |> CCList.filter_map
            (fun (k,v) -> 
              match v with 
              | `String s -> Some (k,s)
              | `Int i -> Some (k, string_of_int i)
              | `Float f -> Some (k, String_.of_float_clean f)
              | _ -> None
            )
      with Type_error _ -> [] in

    (* for error reporting *)
    let sent_id_text () = match (List.assoc_opt "sent_id"  meta, List.assoc_opt "source_sent_id"  meta)  with
      | (Some id,_) -> sprintf ", sent_id=%s" id
      | (None, Some id) -> sprintf ", source_sent_id=%s" id
      | _ -> "" in

    (* check that there is no unknown fields *)
    List.iter
      (function 
        | "meta" | "nodes" | "edges" | "order" -> ()
        | x -> 
          Warning.blue "[G_graph.of_json%s] Unknown field `%s` (See https://grew.fr/doc/json)" (sent_id_text ()) x 
      ) (keys json);

    let nodes =
      match json |> member "nodes" with
      | `Null -> [] (* no nodes field *)
      | json_nodes ->
        try
          json_nodes
          |> to_assoc
          |> List.rev_map (* usage of rev_map to avoid stack overflow on large graph like lexical networks *)
            (fun (id,json_node) ->
              let fs =
                try [("label", json_node |> to_string)]
                with Type_error _ ->
                  json_node
                  |> to_assoc
                  |> List.map (fun (feat_name,json_value) -> (feat_name, json_value |> to_string)) in
              (id,fs)
            )
          |> List.rev  (* restore initial order *)
        with Type_error _ ->
          Error.build
            "[G_graph.of_json%s] Cannot parse field `nodes` (See https://grew.fr/doc/json):\n%s"
            (sent_id_text ())
            (Yojson.Basic.pretty_to_string json_nodes) in

    let edges = match json |> member "edges" with
      | `Null -> []
      | json_edges -> 
        try 
          json_edges
          |> to_list 
          |> List.rev_map (* usage of rev_map to avoid stack overflow on large graph like lexical networks *)
            (fun json_edge ->
              let fs =
                try
                  (* if [json_edge] is of type string, it is interpreted as [1=value] *)
                  try [("1", Feature_value.parse "1" (json_edge |> member "label" |> to_string))]
                  with Type_error _ ->
                    json_edge
                    |> member "label"
                    |> to_assoc
                    |> List.map (fun (x,y) -> (x,Feature_value.parse x (to_string y))) 
                with Type_error _ -> 
                  Error.build
                    "[G_graph.of_json%s] Cannot parse field json `edge` (See https://grew.fr/doc/json):\n%s"
                    (sent_id_text ())
                    (Yojson.Basic.pretty_to_string json_edge) in
              let src = match json_edge |> member "src" with
                | `String s -> s
                | `Null ->
                  Error.build
                    "[G_graph.of_json%s] Missing field `src` in json `edge` (See https://grew.fr/doc/json):\n%s"
                      (sent_id_text ())
                      (Yojson.Basic.pretty_to_string json_edge) 
                | _ ->
                  Error.build
                    "[G_graph.of_json%s] `src` must be a string in json `edge` (See https://grew.fr/doc/json):\n%s"
                      (sent_id_text ())
                      (Yojson.Basic.pretty_to_string json_edge) in
              let tar = match json_edge |> member "tar" with
                | `String s -> s
                | `Null ->
                  Error.build
                    "[G_graph.of_json%s] Missing field `tar` in json `edge` (See https://grew.fr/doc/json):\n%s"
                      (sent_id_text ())
                      (Yojson.Basic.pretty_to_string json_edge) 
                | _ ->
                  Error.build
                    "[G_graph.of_json%s] `tar` must be a string in json `edge` (See https://grew.fr/doc/json):\n%s"
                      (sent_id_text ())
                      (Yojson.Basic.pretty_to_string json_edge) in
               (src,fs,tar)
            )
          |> List.rev  (* restore initial order *)
        with Type_error _ -> 
          Error.build
            "[G_graph.of_json%s] Cannot parse field `edges` (See https://grew.fr/doc/json):\n%s"
              (sent_id_text ())
              (Yojson.Basic.pretty_to_string json_edges) in

    (* build a map with nodes only *)
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

    let order = match json |> member "order" with
      | `Null -> [] (* No "order" field *)
      | json_order -> 
        try 
          json_order 
          |> to_list 
          |> List.map 
            (fun x -> 
              try String_map.find (x |> to_string) table
              with Not_found ->
                Error.build "[G_graph.of_json%s] unknown node identifier `%s` used in `order`"
                  (sent_id_text ()) 
                  (x |> to_string)
            )
        with
        | Type_error _ -> 
          Error.build "[G_graph.of_json%s] cannot parse `order` field, it should be a list of string but is:\n%s"
            (sent_id_text ()) 
            (Yojson.Basic.pretty_to_string json_order) in

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

    (* build a map with nodes + order *)
    let maps_with_order = loop_order (map_without_edges, 0) order in

    (* build a map with nodes + order + edges *)
    let map =
      List.fold_left
        (fun acc (id_src, edge_items, id_tar) ->
          match (String_map.find_opt id_src table, String_map.find_opt id_tar table) with
          | (Some gid_1, Some gid_2) ->
            let edge = G_edge.from_items edge_items in
            begin
              match map_add_edge_opt acc gid_1 edge gid_2 with
              | Some g -> g
              | None -> 
                Error.build "[G_graph.of_json%s] try to build a graph with twice the same edge `%s`from `%s` to `%s`" 
                  (sent_id_text ()) (G_edge.to_string edge) id_src id_tar
            end
          | (None, _) -> Error.build "[G_graph.of_json%s] undefined node id `%s` used as `src` in edges" (sent_id_text ()) id_src
          | (_, None) -> Error.build "[G_graph.of_json%s] undefined node id `%s` used as `tar` in edges" (sent_id_text ()) id_tar
        ) maps_with_order edges in

    { empty with
      meta;
      map;
      highest_index = final_index - 1;
    }

  (* -------------------------------------------------------------------------------- *)
  let get_ordered_gids graph =
    let gid_position_list =
      Gid_map.fold
        (fun gid node acc ->
          match G_node.get_position_opt node with
          | None -> acc
          | Some p -> (gid,p) :: acc
        ) graph.map [] in
    List.map fst (List.sort (fun (_,p1) (_,p2) -> Stdlib.compare p1 p2) gid_position_list)

  (* -------------------------------------------------------------------------------- *)
  let to_json graph =
    let meta = `Assoc (List.map (fun (k,v) -> (k, `String v)) graph.meta) in

    let nodes =
      Gid_map.fold
        (fun gid node acc ->
          let item = (G_node.get_name gid node, G_fs.to_json (G_node.get_fs node)) in
          item :: acc
        ) graph.map []
      |> List.rev in

    let name_of_gid gid =
      let node = find gid graph in
      G_node.get_name gid node in

    let edges =
      Gid_map.fold
        (fun src_gid node acc ->
          let src = `String (G_node.get_name src_gid node) in
          Gid_massoc.fold
            (fun acc2 tar_gid edge ->
              match G_edge.to_json_opt edge with
              | None -> acc2
              | Some js -> (`Assoc [("src", src); ("label", js); ("tar", `String (name_of_gid tar_gid)) ]) :: acc2
            ) acc (G_node.get_next node)
        ) graph.map [] in

    let order = List.map (fun s -> `String (name_of_gid s)) (get_ordered_gids graph) in

    let modified_nodes =
      List.map
        (fun (gid,(_,hf_list)) ->
          `Assoc [("id", `String (name_of_gid gid)); ("features", `List (List.map (fun (fn, _) -> `String fn) hf_list))]
        ) graph.impact.nodes in

    let modified_edges =
      List.map
        (fun (gid1,edge,gid2) ->
          `Assoc [("src", `String (name_of_gid gid1)); ("edge", G_edge.to_json edge); ("tar", `String (name_of_gid gid2))]
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
      List.filter
        (function
          | (_,`List []) -> false
          | (_,`Assoc []) -> false
          | _ -> true
        ) full_assoc_list
    )

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
        let new_nodes =
          List.fold_left
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
      | Some (new_node, new_edge, capture) -> Some ({graph with map = Gid_map.add src_gid new_node graph.map}, new_edge, capture)
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
  let insert_before inserted_gid site_gid graph = 
    let inserted_node = Gid_map.find inserted_gid graph.map
    and site_node = Gid_map.find site_gid graph.map in
    match (G_node.get_position_opt inserted_node, G_node.get_position_opt site_node) with
    | (None, Some p) -> 
      let shifted_map = shift_position 1 site_gid graph.map in
      let new_node = G_node.set_position p inserted_node in
      let new_map = match G_node.get_pred_opt site_node with
        | None ->
          shifted_map
          |> (Gid_map.add inserted_gid new_node)
          |> (map_add_pred_succ inserted_gid site_gid)
        | Some prec_gid ->
          shifted_map
          |> (Gid_map.add inserted_gid new_node)
          |> (map_del_pred_succ prec_gid site_gid)
          |> (map_add_pred_succ inserted_gid site_gid)
          |> (map_add_pred_succ prec_gid inserted_gid) in
      { graph with map=new_map }
    | (Some _,_) -> Error.run "[G_node.insert_before] node already ordered"
    | (_,None) -> Error.run "[G_node.insert_before] unordered node"

  (* -------------------------------------------------------------------------------- *)
  let insert_after inserted_gid site_gid graph = 
    let inserted_node = Gid_map.find inserted_gid graph.map
    and site_node = Gid_map.find site_gid graph.map in
    match (G_node.get_position_opt inserted_node, G_node.get_position_opt site_node) with
    | (None, Some p) -> 
      let new_node = G_node.set_position (p+1) inserted_node in
      let new_map = match G_node.get_succ_opt site_node with
        | None ->
          graph.map
          |> (Gid_map.add inserted_gid new_node)
          |> (map_add_pred_succ site_gid inserted_gid)
        | Some succ_gid ->
          let shifted_map = shift_position 1 succ_gid graph.map in
          shifted_map
          |> (Gid_map.add inserted_gid new_node)
          |> (map_del_pred_succ site_gid succ_gid)
          |> (map_add_pred_succ site_gid inserted_gid )
          |> (map_add_pred_succ inserted_gid succ_gid) in
      { graph with map=new_map }
    | (Some _,_) -> Error.run "[G_node.insert_after] node already ordered"
    | (_,None) -> Error.run "[G_node.insert_after] unordered node"

  (* -------------------------------------------------------------------------------- *)
  let add_before gid graph =
    let node = Gid_map.find gid graph.map in
    let position = match G_node.get_position_opt node with
      | None -> Error.run "[G_node.add_before] unordered node"
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
    | Some _ ->
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
      Gid_massoc.fold
        (fun (acc_src_next,acc_tar_next) next_gid edge ->
          if Label_cst.match_ ~config label_cst edge && not (is_gid_local next_gid)
          then
            match Gid_massoc.add_opt next_gid edge acc_tar_next with
            | None when !Global.safe_commands -> Error.run ~loc "The [shift_out] command tries to build a duplicate edge (with label \"%s\")" (G_edge.to_string ~config  edge)
            | None ->
              del_edges := (src_gid,edge,next_gid) :: !del_edges;
              (Gid_massoc.remove next_gid edge acc_src_next, acc_tar_next)
            | Some new_acc_tar_next ->
              del_edges := (src_gid,edge,next_gid) :: !del_edges;
              add_edges := (tar_gid,edge,next_gid) :: !add_edges;
              (Gid_massoc.remove next_gid edge acc_src_next, new_acc_tar_next)
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
          if is_gid_local node_id (* shift does not move request edges *)
          then node
          else
            let node_next = G_node.get_next node in
            match Gid_massoc.assoc src_gid node_next with
            | [] -> node (* no edges from node to src *)
            | node_src_edges ->
              let node_tar_edges = Gid_massoc.assoc tar_gid node_next in
              let (new_node_src_edges, new_node_tar_edges) =
                List.fold_left
                  (fun (acc_node_src_edges,acc_node_tar_edges) edge ->
                    if Label_cst.match_ ~config label_cst edge
                    then
                      match List_.usort_insert_opt edge acc_node_tar_edges with
                      | None when !Global.safe_commands ->
                        Error.run ~loc "The [shift_in] command tries to build a duplicate edge (with label \"%s\")" (G_edge.to_string ~config edge)
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
                |> (Gid_massoc.replace src_gid new_node_src_edges)
                |> (Gid_massoc.replace tar_gid new_node_tar_edges) in
              G_node.set_next new_next node
        ) graph.map in
    (
      { graph with map = new_map },
      !del_edges,
      !add_edges
    )

  (* -------------------------------------------------------------------------------- *)
  let shift_edges ~config loc src_gid tar_gid is_gid_local label_cst graph =
    let (g1,de1,ae1) = shift_out ~config loc src_gid tar_gid is_gid_local label_cst graph in
    let (g2,de2,ae2) = shift_in ~config loc src_gid tar_gid is_gid_local label_cst g1 in
    (g2, de1 @ de2, ae1 @ ae2)

  (* -------------------------------------------------------------------------------- *)
  let update_feat graph node_id feat_name new_value =
    let node = Gid_map.find node_id graph.map in
    let new_fs = G_fs.set_value feat_name new_value (G_node.get_fs node) in
    let new_node = G_node.set_fs new_fs node in
    { graph with map = Gid_map.add node_id new_node graph.map }

  (* -------------------------------------------------------------------------------- *)
  let concat_feats_opt ?loc graph side src_id tar_id separator regexp =
    let src_node = Gid_map.find src_id graph.map in
    let tar_node = Gid_map.find tar_id graph.map in
    match G_node.concat_feats_opt ?loc side src_node tar_node separator regexp with
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
  let space_after node =
    match G_fs.get_value_opt "SpaceAfter" (G_node.get_fs node) with
    | Some (String "No") -> false
    | _ -> true

  let to_sentence ?pivot ?(deco=G_deco.empty) graph =
    let high_list = match pivot with
      | None -> List.map fst deco.nodes
      | Some pivot ->
        match List.find_opt (fun (_, (pid,_)) -> pid = pivot) deco.nodes with
        | None -> Error.run "Undefined pivot %s" pivot
        | Some (gid,_) -> [gid] in

    let is_highlighted_gid gid = List.mem gid high_list in

    let exception Find of Gid.t in
    let init_gid_opt =
      try
        Gid_map.iter
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
          | (Some s, true, true) -> bprintf buff "<span class=\"highlight\">%s</span> " s in
        let node = Gid_map.find gid graph.map in
        let fs = G_node.get_fs node in
        let (new_current_form, new_flag_highlight, new_flag_sa) =
          match (G_fs.get_value_opt "wordform" fs, G_fs.get_value_opt "textform" fs, G_fs.get_value_opt "form" fs) with
          | (Some (String "__EMPTY__"), _, _) -> (current_form, flag_highlight, flag_sa)
          | (_, Some (String "_"),_) -> (current_form, flag_highlight || (is_highlighted_gid gid), space_after node)
          | (_, None, Some (String "__0__")) -> (current_form, flag_highlight, false)
          | (_, Some (String form), _)
          | (_, None, Some (String form)) ->
            let form = match form with "UNDERSCORE" -> "_" | x -> x in (* '_' is escaped in textform to avoid ambiguity with textform=_ in multi-word tokens *)
            to_buff (current_form, flag_highlight, flag_sa); (Some form, is_highlighted_gid gid, space_after node)
          | _ -> (current_form, flag_highlight, space_after node) in
        match G_node.get_succ_opt node with
        | Some succ_gid -> loop new_current_form new_flag_highlight new_flag_sa succ_gid
        | None -> to_buff (new_current_form, new_flag_highlight, new_flag_sa) in
      loop None false true init_gid;
      Buffer.contents buff
  let bound_time gnode = 
    let fs = G_node.get_fs gnode in
    let start_opt = match (G_fs.get_value_opt "_start" fs, G_fs.get_value_opt "AlignBegin" fs) with
    | (Some (Float start),_) -> Some start
    | (_,Some (Float align_begin)) -> Some (align_begin /. 1000.)
    | _ -> None
    and ending_opt = match (G_fs.get_value_opt "_stop" fs, G_fs.get_value_opt "AlignEnd" fs) with
    | (Some (Float stop),_) -> Some stop
    | (_,Some (Float align_end)) -> Some (align_end /. 1000.)
    | _ -> None in
    match (start_opt, ending_opt) with
    | (Some s, Some e) -> Some (s,e)
    | _ -> None

  let to_sentence_audio ?(deco=G_deco.empty) graph =
    let is_highlighted_gid gid = List.mem_assoc gid deco.nodes in

    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) nodes in

    let buff = Buffer.create 32 in
    let sentence_bounds = ref None in
    List.iter
      (fun (gid, gnode) ->
        let g_fs = G_node.get_fs gnode in
        match G_fs.to_word_opt g_fs with
        | None -> ()
        | Some "__0__" -> ()
        | Some word ->
          let word_no_escape = Str.global_replace (Str.regexp_string "\\\"") "\"" word in
          match bound_time gnode with
          | Some (start, ending) ->
            begin
              match !sentence_bounds with
              | None -> sentence_bounds := Some (start, ending)
              | Some (s, _) -> sentence_bounds := Some (s, ending)
            end;
            Printf.bprintf buff
              "<span data-begin=\"%g\" data-dur=\"%g\" tabindex=\"0\" %s>%s </span>"
              start
              (ending -. start)
              (if is_highlighted_gid gid then "class=\"highlight\"" else "")
              word_no_escape
          | None -> 
            Printf.bprintf buff 
              "<span %s>%s </span>"
              (if is_highlighted_gid gid then "class=\"highlight\"" else "")
              word_no_escape
      ) snodes; 
    (Buffer.contents buff, !sentence_bounds, get_meta_opt "sound_url" graph)


  let remove_conll_root_node graph =
    let exception Find of Gid.t in
    try 
      Gid_map.iter 
        (fun gid node -> 
          if G_node.is_conll_zero node
          then raise (Find gid) 
        ) graph.map; graph
    with Find gid_root -> match del_node_opt gid_root graph with
      | Some g -> g
      | None -> graph

  (* -------------------------------------------------------------------------------- *)
  let to_dep ?filter ?(no_root=false) ?main_feat ?(deco=G_deco.empty) ~config graph =

    let graph = if no_root then remove_conll_root_node graph else graph in 

    (* split lexical // non-lexical nodes *)
    let (ordered_nodes, non_ordered_nodes) =
      Gid_map.fold
        (fun gid node (acc1, acc2) ->
          match G_node.get_position_opt node with
          | None -> (acc1, (gid,node)::acc2)
          | Some _ -> ((gid,node)::acc1, acc2)
        ) graph.map ([],[]) in

    let sorted_ordered_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) ordered_nodes in

    let insert (gid,node) nodes =
      let fs = G_node.get_fs node in
      match (G_fs.get_value_opt "parseme" fs, G_fs.get_value_opt "frsemcor" fs, G_fs.get_value_opt "Cxn" fs) with
      | (None, None, None) -> (* dmrs node *)
        (gid,node) :: nodes
      | _ -> (* parseme / frsemcor / Cxn node --> place it before its first lexical item *)
        let next_ids = Gid_massoc.fold (fun acc gid _ -> gid::acc) [] (G_node.get_next node) in
        let rec loop = function
          | [] -> [(gid,node)]
          | (h,n)::t when List.mem h next_ids -> (gid,node)::(h,n)::t
          | h::t -> h :: (loop t) in
        loop nodes in

    (* insertion of non lexical node to a sensible position in the linear order *)
    let nodes =
      List.fold_left (
        fun acc gid_node -> insert gid_node acc
      ) sorted_ordered_nodes non_ordered_nodes in

    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { opacity=0; scale = 200; fontname=\"Arial\"; }\n";
    bprintf buff "[WORDS] { \n";

    (* nodes *)
    List.iter
      (fun (id, node) ->
        let decorated_feat = try List.assoc id deco.G_deco.nodes with Not_found -> ("",[]) in
        let fs = G_node.get_fs node in

        let loop_edge = Gid_massoc.assoc id (G_node.get_next node) in
        let loops = List.map (fun edge -> sprintf "[[%s]]:B:#82CAAF" (G_edge.to_string ~config edge)) loop_edge in

        let tail =
          match (!Global.debug, G_node.get_position_opt node) with
          | (true, Some pos) -> (sprintf "position=%d:B:lightblue" pos) :: loops
          | _ -> loops in
  
        let dep_fs = G_fs.to_dep ~decorated_feat ~tail ?filter ?main_feat fs in

        let style =
          match G_fs.get_value_opt "void" fs with
          | Some (String "y") -> "; forecolor=red; subcolor=red; "
          | _ ->
          match G_fs.get_value_opt "wordform" fs with
          | Some (String "__EMPTY__") -> "; forecolor=#cc00cb; subcolor=#cc00cb; "
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
        ) sorted_ordered_nodes;

    Gid_map.iter
      (fun gid elt ->
        Gid_massoc.iter
          (fun tar g_edge ->
            let deco = List.mem (gid,g_edge,tar) deco.G_deco.edges in
            match G_edge.to_dep_opt ~deco ~config g_edge with
            | None -> ()
            | Some string_edge ->
              if gid <> tar (* if gid=tar, the loop is encoded in the node see `loops` above  *)
              then bprintf buff "N_%s -> N_%s %s\n" (Gid.to_string gid) (Gid.to_string tar) string_edge
          ) (G_node.get_next elt)
      ) graph.map;

    bprintf buff "} \n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)

  module Layers = Map.Make (struct type t = string option let compare = Stdlib.compare end)

  (* -------------------------------------------------------------------------------- *)
  let to_dot ?main_feat ?(deco=G_deco.empty) ~config graph =
    let buff = Buffer.create 32 in

    bprintf buff "digraph G {\n";
    bprintf buff "  node [shape=box];\n";

    let layers =
      Gid_map.fold
        (fun id node acc ->
          let layer_opt = G_fs.get_value_opt "layer" (G_node.get_fs node) |> CCOption.map Feature_value.to_string in
          let prev = match Layers.find_opt layer_opt acc with
            | None -> []
            | Some l -> l in
          Layers.add layer_opt ((id,node) :: prev) acc
        ) graph.map Layers.empty in

    (* nodes *)
    Layers.iter
      (fun layer_opt node_list ->
        CCOption.iter (fun l -> bprintf buff "	subgraph cluster_%s {\n" l) layer_opt;
        List.iter (
          fun (id,node) ->
            let fs = G_node.get_fs node in
            let shape = match 
              (
                fs |> G_fs.get_value_opt "type" |> CCOption.map Feature_value.to_string,
                fs |> G_fs.get_value_opt "label" |> CCOption.map Feature_value.to_string
              )
             with
            | (Some "v",_) -> "shape=oval, "
            | (Some "f", Some "temp") -> "style=filled, color=\"#3e9fcf\", shape=invhouse, "
            | (Some "f", Some "neg") -> "style=filled, color=\"#CFB464\", shape=invhouse, "
            | (Some "f", Some "modal") -> "style=filled, color=\"#45a729\", shape=invhouse, "
            | (Some "f", Some "quant") -> "style=filled, color=\"#f07065\", shape=invhouse, "
            | (Some "f", Some "loc") -> "style=filled, color=\"pink\", shape=invhouse, "
            | _ -> "" in

            let decorated_feat =
              try List.assoc id deco.G_deco.nodes
              with Not_found -> ("",[]) in
            let fs = G_node.get_fs node |> G_fs.del_feat "layer" |> G_fs.del_feat "type" in

            match G_fs.to_dot ~decorated_feat ?main_feat fs with
            | "" -> bprintf buff "  N_%s [%slabel=\"\"]\n" (Gid.to_string id) shape
            | s -> bprintf buff "  N_%s [%slabel=<%s>]\n" (Gid.to_string id) shape s
          ) node_list;
         CCOption.iter (fun _ -> bprintf buff "	}\n") layer_opt
      ) layers;

    (* edges *)
    Gid_map.iter
      (fun id node ->
        Gid_massoc.iter
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

    (* more edge in debug modes *)
    Gid_map.iter
      (fun id node ->
        match G_node.get_succ_opt node with
        | Some s -> 
          (* make ordered nodes appear at the same level and in the right order with white (lightblue in debug) edges *)
          bprintf buff " { rank=same; N_%s; N_%s; }\n" (Gid.to_string id) (Gid.to_string s);
          let color = if !Global.debug then "lightblue" else "white" in 
          bprintf buff "  N_%s -> N_%s [label=\"SUCC\", style=dotted, fontcolor=%s, color=%s];\n"
            (Gid.to_string id) (Gid.to_string s) color color
        | _ -> ()
      ) graph.map;

    bprintf buff "}\n";
    Buffer.contents buff

  let add_feat_value feat_name feat_value acc =
    let string_feat_value = Feature_value.to_string feat_value in
    let old_sub =
      String_map.find_opt feat_name acc
      |> CCOption.get_or ~default: String_map.empty in
    let old_count = 
      String_map.find_opt string_feat_value old_sub
      |> CCOption.get_or ~default: 0 in
    String_map.add feat_name 
      (String_map.add string_feat_value (old_count + 1) old_sub) acc

  let count_feature_values ?(filter=fun _ -> true) ?(acc=String_map.empty) t =
    Gid_map.fold
      (fun _ node acc2 ->
        if G_node.is_conll_zero node
        then acc2
        else 
          let fs = G_node.get_fs node in
          let features = G_fs.get_features fs in
          String_set.fold
          (fun feat_name acc3 ->
            if filter feat_name
            then
              match G_fs.get_value_opt feat_name fs with
              | None -> assert false
              | Some v -> add_feat_value feat_name v acc3
            else
              acc3
          ) features acc2
      ) t.map acc

  (* -------------------------------------------------------------------------------- *)
  let get_feature_values feature_name t =
    Gid_map.fold
      (fun _ node acc ->
        match G_fs.get_value_opt feature_name (G_node.get_fs node) with
        | None -> acc
        | Some v -> String_set.add (Feature_value.to_string v) acc
      ) t.map String_set.empty

  let get_relations ~config t =
    Gid_map.fold
      (fun _ node acc ->
        Gid_massoc.fold_on_list
          (fun acc2 _ edges ->
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

  let append_in_ag_lex keys t proj = 
    fold_node 
      (fun node acc -> 
        if G_node.is_conll_zero node
        then acc
        else G_node.append_in_ag_lex keys node acc 
      ) t proj 

  let is_projective t =
    let (arc_positions, _) =
      Gid_map.fold
        (fun src_gid src_node (acc, acc_map) ->
          match G_node.get_position_opt src_node with
          | None -> (acc, acc_map)
          | Some src_position ->
            let new_acc =
              Gid_massoc.fold 
                (fun acc2 tar_gid _ ->
                  let tar_node = find tar_gid t in
                  match G_node.get_position_opt tar_node with
                  | None -> acc2
                  | Some tar_position -> (min src_position tar_position, max src_position tar_position) :: acc2
                ) acc (G_node.get_next_basic src_node) in
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
       Tree detection is easier (is_tree <=> back_edges=0 and nontree_edges=0)
  *)
  (* --------------------------------------------------------------- *)
  let get_roots graph =
    let non_roots =
      Gid_map.fold
        (fun _ node acc ->
          Gid_massoc.fold_on_list
            (fun acc2 next_gid _ ->
              Gid_set.add next_gid acc2
            ) acc (G_node.get_next_basic node)
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
      Gid_massoc.iter
        (fun next_gid _ ->
          try
            match Gid_map.find next_gid !info.intervals with
            | Pre _ -> info := {!info with back_edges = (gid, next_gid) :: !info.back_edges};
            | Pre_post _ -> info := {!info with nontree_edges = (gid, next_gid) :: !info.nontree_edges};
          with
          | Not_found -> explore next_gid
        ) (G_node.get_next_basic node);
      match Gid_map.find_opt gid !info.intervals with
      | Some (Pre i) -> info := {!info with intervals = Gid_map.add gid (Pre_post (i,!clock)) !info.intervals}; incr clock;
      | _ -> assert false in

    let roots = get_roots graph in
    let nb_roots = Gid_set.cardinal roots in
    Gid_set.iter
      (fun gid ->
        explore gid
      ) roots;

    if Gid_map.cardinal !info.intervals < Gid_map.cardinal graph.map
    then
      begin
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
    ordered_nodes: Gid.t list;
    edges: ((Gid.t * G_edge.t * Gid.t) * status) list;
    feats: ((Gid.t * string) * (Feature_value.t option)) list;
  }

  let init ordered_nodes = { del_nodes=[]; ordered_nodes; edges=[]; feats=[]; }

  let del_node gid t =
    match List_.usort_insert_opt gid t.del_nodes with
    | None -> raise (Inconsistent "del_node")
    | Some new_del_nodes -> {
        del_nodes= new_del_nodes;
        ordered_nodes = List.filter (fun g -> g <> gid) t.ordered_nodes;
        edges = List.filter (fun ((g1,_,g2),_) -> g1 <> gid && g2 <> gid) t.edges;
        feats = List.filter (fun ((g,_),_) -> g <> gid) t.feats;
      }

  let add_edge src lab tar t =
    let rec loop = fun old -> match old with
      | []                                                 -> ((src,lab,tar),Add)::old
      | ((s,l,t),_)::_ when (src,lab,tar) < (s,l,t)        -> ((src,lab,tar),Add)::old
      | ((s,l,t),stat)::tail when (src,lab,tar) > (s,l,t)  -> ((s,l,t),stat)::(loop tail)
      | ((_,_,_), Add)::_ (* (src,lab,tar) = (s,l,t) *)    -> raise (Inconsistent "add_edge")
      | ((_,_,_), Del)::tail (* (src,lab,tar) = (s,l,t) *) -> tail in
    { t with edges = loop t.edges }

  let del_edge src lab tar t =
    let rec loop = fun old -> match old with
      | []                                                 -> ((src,lab,tar),Del)::old
      | ((s,l,t),_)::_ when (src,lab,tar) < (s,l,t)        -> ((src,lab,tar),Del)::old
      | ((s,l,t),stat)::tail when (src,lab,tar) > (s,l,t)  -> ((s,l,t),stat)::(loop tail)
      | ((_,_,_), Del)::_ (* (src,lab,tar) = (s,l,t) *)    -> raise (Inconsistent "del_edge")
      | ((_,_,_), Add)::tail (* (src,lab,tar) = (s,l,t) *) -> tail in
    { t with edges = loop t.edges }

  let set_feat seed_graph gid feat_name new_val_opt t =
    (* equal_orig is true iff new val is the same as the one in seed_graph *)
    let equal_orig =
      try (new_val_opt = G_fs.get_value_opt feat_name (G_node.get_fs (G_graph.find gid seed_graph)))
      with Not_found -> false (* when gid is in created nodes *) in
    let rec loop = fun old -> match old with
      | [] when equal_orig                                             -> []
      | []                                                             -> [(gid,feat_name), new_val_opt]
      | ((g,f),_)::_ when (gid,feat_name) < (g,f) && equal_orig        -> old
      | ((g,f),_)::_ when (gid,feat_name) < (g,f)                      -> ((gid,feat_name), new_val_opt)::old
      | ((g,f),v)::tail when (gid,feat_name) > (g,f)                   -> ((g,f),v)::(loop tail)
      | ((_,_),_)::tail when (* (g,f)=(gid,feat_name) && *) equal_orig -> tail
      | ((g,f),_)::tail (* when (g,f)=(gid,feat_name) *)               -> ((g,f), new_val_opt) :: tail in
    { t with feats = loop t.feats }

  let unorder gid t = { t with ordered_nodes = CCList.remove ~eq:(=) ~key:gid t.ordered_nodes } 

  let insert_after inserted_gid site_gid t =
    let rec loop = function
      | [] -> Error.bug "Delta.insert_after"
      | x::t when x = site_gid -> x :: inserted_gid :: t
      | x::t -> x::(loop t) in
    { t with ordered_nodes = loop t.ordered_nodes }

  let insert_before inserted_gid site_gid t =
    let rec loop = function
      | [] -> Error.bug "Delta.insert_before"
      | x::t when x = site_gid -> inserted_gid :: x :: t
      | x::t -> x::(loop t) in
    { t with ordered_nodes = loop t.ordered_nodes }
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
  }

  let from_graph graph = { graph; seed=graph; delta = Delta.init (G_graph.get_ordered_gids graph); added_gids = []; e_mapping = String_map.empty; added_gids_in_rule =[] }

  (* WARNING: compare is correct only on data with the same seed! *)
  let compare t1 t2 = Stdlib.compare (t1.delta,t1.added_gids) (t2.delta, t2.added_gids)
end (* module Graph_with_history*)

(* ================================================================================ *)
module Graph_with_history_set = Set.Make (Graph_with_history)
