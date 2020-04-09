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
open Grew_domain
open Grew_fs
open Grew_node

(* ================================================================================ *)
module P_graph = struct
  type t = P_node.t Pid_map.t

  let empty = Pid_map.empty

  let find = Pid_map.find

  let pid_name_list t = Pid_map.fold (fun _ node acc -> (P_node.get_name node)::acc) t []

  let to_json ?domain t =
    `List (
      Pid_map.fold
        (fun pid p_node acc ->
           (`Assoc [
               ("id", `String (Pid.to_string pid));
               ("node", P_node.to_json ?domain p_node)
             ]) :: acc
        ) t []
    )

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge src_pid label tar_pid map =
    let src_node =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Pid_map.find src_pid map with Not_found -> P_node.empty in
    match P_node.add_edge_opt label tar_pid src_node with
    | None -> None
    | Some new_node -> Some (Pid_map.add src_pid new_node map)

  (* -------------------------------------------------------------------------------- *)
  let build ?domain lexicons basic_ast =
    let full_node_list = basic_ast.Ast.pat_nodes
    and full_edge_list = basic_ast.Ast.pat_edges in

    (* NB: insert searches for a previous node with the same name and uses unification rather than constraint *)
    (* NB: insertion of new node at the end of the list: not efficient but graph building is not the hard part. *)
    let rec insert (ast_node, loc) = function
      | [] -> [P_node.build_from_ast ?domain lexicons (ast_node, loc)]
      | (node_id,fs)::tail when ast_node.Ast.node_id = node_id ->
        begin
          try (node_id, P_node.unif_fs (P_fs.build ?domain lexicons ast_node.Ast.fs) fs) :: tail
          with Error.Build (msg,_) -> raise (Error.Build (msg,Some loc))
        end
      | head :: tail -> head :: (insert (ast_node, loc) tail) in

    let (named_nodes : (Id.name * P_node.t) list) =
      List.fold_left
        (fun acc ast_node -> insert ast_node acc)
        [] full_node_list in

    let sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Stdlib.compare id1 id2) named_nodes in
    let (sorted_ids, node_list) = List.split sorted_nodes in

    (* [pos_table] contains the sorted list of node ids *)
    let pos_table = Array.of_list sorted_ids in

    (* the nodes, in the same order *)
    let map_without_edges = List_.foldi_left
        (fun i acc elt -> Pid_map.add (Pid.Pos i) elt acc)
        Pid_map.empty node_list in

    let (map,edge_ids : t * string list) =
      List.fold_left
        (fun (acc_map,acc_edge_ids) (ast_edge, loc) ->
           let i1 = Id.build ~loc ast_edge.Ast.src pos_table in
           let i2 = Id.build ~loc ast_edge.Ast.tar pos_table in
           match ast_edge.Ast.edge_label_cst with
           | Ast.Pred -> (* when a Pred is declared, add two edges pred & succ *)
             (match map_add_edge (Pid.Pos i1) P_edge.succ (Pid.Pos i2) acc_map with
              | Some acc2 -> (match map_add_edge (Pid.Pos i2) P_edge.pred (Pid.Pos i1) acc_map with
                  | Some m -> (m, acc_edge_ids)
                  | None -> Error.build ~loc "[P_graph.build] try to build a graph with twice the order edge"
                )
              | None -> Error.build ~loc "[P_graph.build] try to build a graph with twice the order edge"
             )
           | _ ->
             let edge = P_edge.build ?domain (ast_edge, loc) in
             (match map_add_edge (Pid.Pos i1) edge (Pid.Pos i2) acc_map with
              | Some m -> (m, match ast_edge.Ast.edge_id with Some id -> id::acc_edge_ids | None -> acc_edge_ids)
              | None -> Error.build ~loc "[P_graph.build] try to build a graph with twice the same edge %s"
                          (P_edge.to_string ?domain edge)
             )
        ) (map_without_edges,[]) full_edge_list in

    (map, pos_table, edge_ids)


  (* -------------------------------------------------------------------------------- *)
  (* a type for extension of graph (a former graph exists):
     in grew the former is a positive basic and an extension is a negative basic ("without") *)
  type extension = {
    ext_map: t; (* node description for new nodes and for edge "Old -> New"  *)
    old_map: t; (* a partial map for new constraints on old nodes "Old [...]" *)
  }

  (* -------------------------------------------------------------------------------- *)
  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_extension ?domain lexicons pos_table edge_ids full_node_list full_edge_list =

    let built_nodes = List.map (P_node.build_from_ast ?domain lexicons) full_node_list in

    let (old_nodes, new_nodes) =
      List.partition
        (function (id,_) when Array_.dicho_mem id pos_table -> true | _ -> false)
        built_nodes in

    let new_sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Stdlib.compare id1 id2) new_nodes in

    let (new_sorted_ids, new_node_list) = List.split new_sorted_nodes in

    (* table contains the sorted list of node ids *)
    let new_table = Array.of_list new_sorted_ids in

    (* the nodes, in the same order stored with index -1, -2, ... -N TODO check ?? *)
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

    let (ext_map_with_all_edges, new_edge_ids) =
      List.fold_left
        (fun (acc_map, acc_edge_ids) (ast_edge, loc) ->
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

           match ast_edge.Ast.edge_label_cst with
           | Ast.Pred ->
             (match map_add_edge i1 P_edge.succ i2 acc_map with
              | Some acc2 -> (match map_add_edge i2 P_edge.pred i1 acc_map with
                  | Some m -> (m, acc_edge_ids)
                  | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the order edge"
                )
              | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the order edge"
             )
           | _ ->
             let edge = P_edge.build ?domain (ast_edge, loc) in
             (match map_add_edge i1 edge i2 acc_map with
              | Some m -> (m, match ast_edge.Ast.edge_id with Some id -> id::acc_edge_ids | None -> acc_edge_ids)
              | None -> Error.build ~loc "[P_graph.build_extension] try to build a graph with twice the same edge %s"
                          (P_edge.to_string ?domain edge)
             )
        ) (ext_map_without_edges, edge_ids) full_edge_list in
    ({ext_map = ext_map_with_all_edges; old_map = old_map_without_edges}, new_table, new_edge_ids)


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
end (* module G_deco *)

(* ================================================================================ *)
module G_graph = struct
  type fusion_item = {
    first: Gid.t;
    last: Gid.t;
    word: string;
    efs: (string * string) list;
  }

  type t = {
    domain: Domain.t option;
    meta: string list;            (* meta-informations *)
    map: G_node.t Gid_map.t;      (* node description *)
    fusion: fusion_item list;     (* the list of fusion word considered in UD conll *)
    highest_index: int;           (* the next free integer index *)
    rules: int String_map.t;
  }

  let get_meta_opt key t =
    let rec loop = function
      | [] -> None
      | line::tail ->
        begin
          match Str.bounded_full_split (Str.regexp "[#=\t ]+") line 3 with
          | [Str.Delim _; Str.Text k; Str.Delim _; Str.Text value] when k = key -> Some value
          | s -> loop tail
        end in loop t.meta

  let empty = { domain=None; meta=[]; map=Gid_map.empty; fusion=[]; highest_index=0; rules=String_map.empty; }

  let is_empty t = Gid_map.is_empty t.map

  let size t = Gid_map.cardinal (t.map)

  let get_domain_opt t = t.domain

  let find node_id graph = Gid_map.find node_id graph.map
  let find_opt node_id graph = Gid_map.find_opt node_id graph.map

  let node_exists fct t = Gid_map.exists (fun _ node -> fct node) t.map

  let fold_gid fct t init =
    Gid_map.fold (fun gid _ acc -> fct gid acc) t.map init

  let push_rule rule_name t =
    if !Global.track_rules
    then
      let old = try String_map.find rule_name t.rules with Not_found -> 0 in
      { t with rules = String_map.add rule_name (old+1) t.rules }
    else t

  let string_rules t =
    String_map.fold
      (fun k v acc ->
         sprintf "%s:%d; %s" k v acc
      ) t.rules ""

  (* is there an edge e out of node i ? *)
  let edge_out graph node_id label_cst =
    let domain = get_domain_opt graph in
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> Label_cst.match_ ?domain label_cst e) (G_node.get_next node)

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

  let map_del_pred_succ gid1 gid2 map =
    map
    |> (map_del_edge gid1 G_edge.succ gid2)
    |> (map_del_edge gid2 G_edge.pred gid1)


  (* -------------------------------------------------------------------------------- *)
  let build ?domain gr_ast =

    let (ordered_nodes, unordered_nodes) =
      List.fold_left
        (fun (orderd_acc, unordered_acc) (node,loc) ->
           match Id.get_pos_opt node.Ast.node_id with
           | Some p -> ((p,(node,loc)) :: orderd_acc, unordered_acc)
           | None -> (orderd_acc, (node,loc) :: unordered_acc)
        ) ([],[]) gr_ast.Ast.nodes in

    let sorted_nodes = List.sort (fun (p1,_) (p2,_) -> Stdlib.compare p1 p2) ordered_nodes in

    let rec loop already_bound index pred = function
      | [] -> (Gid_map.empty,[])
      | (_, (ast_node, loc))::tail ->
        let node_id = ast_node.Ast.node_id in
        if List.mem node_id already_bound
        then Error.build ~loc "[GRS] [G_graph.build] try to build a graph with twice the same node id '%s'" node_id
        else
          let (new_tail, table) = loop (node_id :: already_bound) (index+1) (Some index) tail in
          let new_node = G_node.build_from_ast ?domain ~position:index (ast_node, loc) in
          (
            new_tail
            |> (Gid_map.add index new_node)
            |> (fun map -> match pred with | Some p -> map_add_pred_succ p index map | None -> map),
            (node_id,index)::table
          ) in

    let (map_with_ordered_nodes, table_ordered) = loop [] 0 None sorted_nodes in

    let (map_without_edges, table, final_index) =
      List.fold_left
        (fun (acc_map, acc_table, acc_index) (ast_node,loc) ->
           let node_id = ast_node.Ast.node_id in
           let new_node = G_node.build_from_ast ?domain (ast_node,loc) in
           (
             Gid_map.add acc_index new_node acc_map,
             (node_id,acc_index)::acc_table,
             acc_index + 1
           )
        ) (map_with_ordered_nodes, table_ordered, List.length sorted_nodes) unordered_nodes in


    let map =
      List.fold_left
        (fun acc (ast_edge, loc) ->
           let i1 = List.assoc ast_edge.Ast.src table in
           let i2 = List.assoc ast_edge.Ast.tar table in
           let edge = G_edge.build (ast_edge, loc) in
           (match map_add_edge_opt acc i1 edge i2 with
            | Some g -> g
            | None -> Error.build ~loc "[G_graph.build] try to build a graph with twice the same edge %s"
                        (G_edge.dump edge)
           )
        ) map_without_edges gr_ast.Ast.edges in

    {
      domain;
      meta=gr_ast.Ast.meta;
      map;
      fusion = [];
      highest_index = final_index - 1;
      rules = String_map.empty;
    }

  (* -------------------------------------------------------------------------------- *)
  let of_json = function
    | `Assoc (l : (string * Yojson.Basic.t) list) ->
      let (ast_node_list, ast_edge_list) = List.fold_left
          (fun (acc_node, acc_edge) -> function
             | (id, `List [`Assoc feat_json_list; `List succ]) ->
               let fs = List.map (function
                   | (feat_name, `String value) -> ({Ast.name= feat_name; kind = Ast.Equality [value]}, Loc.empty)
                   | _ -> Error.build "[Graph.of_json] not an valid feature structure"
                 ) feat_json_list in
               let new_edges = List.map
                   (function
                     | `List [`String rel; `String tar] -> ({Ast.edge_id=None; edge_label_cst=Ast.Pos_list [rel]; src=id; tar},Loc.empty)
                     | _ -> Error.build "[Graph.of_json] not an valid succ list"
                   ) succ in
               (
                 ({ Ast.node_id=id; position=None; fs}, Loc.empty) :: acc_node,
                 new_edges @ acc_edge
               )
             | _ -> Error.build "[Graph.of_json] not an assoc list"
          ) ([],[]) l in
      let graph_ast = { Ast.meta=[]; nodes=ast_node_list; edges=ast_edge_list}
      in build graph_ast
    | _ -> Error.build "[Graph.of_json] not an assoc list"

  (* -------------------------------------------------------------------------------- *)
  let of_conll ?domain conll =
    let sorted_lines = Conll.root :: (List.sort Conll.compare conll.Conll.lines) in

    (* [gtable] maps *)
    let gtable = (
      Array.of_list (List.map (fun line -> line.Conll.id) sorted_lines),
      Conll.Id.to_dot
    ) in

    let rec loop index pred = function
      | [] -> Gid_map.empty
      | [last] ->
        let loc = Loc.file_opt_line conll.Conll.file last.Conll.line_num in
        let node =
          G_node.build_from_conll ~loc ?domain (Some index) last
          |> (fun x -> match pred with None -> x | Some p -> G_node.add_edge G_edge.pred p x)
        in
        Gid_map.add index node Gid_map.empty
      | line::tail ->
        let loc = Loc.file_opt_line conll.Conll.file line.Conll.line_num in
        let node =
          G_node.build_from_conll ~loc ?domain (Some index) line
          |> (fun x -> match pred with None -> x | Some p -> G_node.add_edge G_edge.pred p x)
          |> G_node.add_edge G_edge.succ (index+1)
        in
        Gid_map.add index node (loop (index+1) (Some index) tail) in

    let map_without_edges = loop 0 None sorted_lines in

    let map_with_edges =
      List.fold_left
        (fun acc line ->
           let loc = Loc.file_opt_line conll.Conll.file line.Conll.line_num in
           let dep_id = Id.gbuild ~loc line.Conll.id gtable in
           List.fold_left
             (fun acc2 (gov, dep_lab) ->
                let gov_id = Id.gbuild ~loc gov gtable in
                let edge = G_edge.from_string dep_lab in
                (match map_add_edge_opt acc2 gov_id edge dep_id with
                 | Some g -> g
                 | None -> Error.build ~loc "[Graph.of_conll] try to build a graph with twice the same edge %s"
                             (G_edge.dump edge)
                )
             ) acc line.Conll.deps
        ) map_without_edges conll.Conll.lines in

    let fusion =
      List.map
        (fun {Conll.first; last; fusion; mw_line_num; mw_efs} ->
           let loc = Loc.file_opt_line_opt conll.Conll.file mw_line_num in
           (
             {
               first = Id.gbuild ~loc (first,None) gtable;
               last = Id.gbuild ~loc (last, None) gtable;
               word = fusion;
               efs = mw_efs;
             }
           )
        ) conll.Conll.multiwords in

    let (map_with_nl_nodes, free_index) =
      Conll_types.Int_map.fold
        (fun key mwe (acc, free_index) ->
           let kind = match mwe.Mwe.kind with Mwe.Ne -> "NE" | Mwe.Mwe -> "MWE" in
           let fs1 = G_fs.set_atom ?domain "kind" kind G_fs.empty in
           let fs2 = match mwe.Mwe.label with None -> fs1 | Some p -> G_fs.set_atom ?domain "label" p fs1 in
           let fs3 = match mwe.Mwe.mwepos with None -> fs2 | Some p -> G_fs.set_atom ?domain "mwepos" p fs2 in
           let fs4 = match mwe.Mwe.criterion with None -> fs3 | Some c -> G_fs.set_atom ?domain "criterion" c fs3 in

           let new_node = G_node.set_fs fs4 (G_node.build ()) in

           (* add a new node *)
           let new_map_1 = (Gid_map.add free_index new_node acc) in
           (* add a link to the first component *)
           let new_map_2 = map_add_edge free_index (G_edge.from_string kind) (Id.gbuild (fst mwe.Mwe.first) gtable) new_map_1 in
           let new_map_3 = match snd mwe.Mwe.first with
             | None -> new_map_2
             | Some i -> map_add_edge free_index (G_edge.from_string (sprintf "%d" i)) (Id.gbuild (fst mwe.Mwe.first) gtable) new_map_2 in

           (* add a link to each other component *)
           let new_map_4 =
             Id_with_proj_set.fold (
               fun item acc2 ->
                 let tmp = map_add_edge free_index (G_edge.from_string kind) (Id.gbuild (fst item) gtable) acc2 in
                 match snd item with
                 | None -> tmp
                 | Some i -> map_add_edge free_index (G_edge.from_string (sprintf "%d" i)) (Id.gbuild (fst item) gtable) tmp
             ) mwe.Mwe.items new_map_3 in

           (new_map_4, free_index+1)
        ) conll.Conll.mwes (map_with_edges, List.length sorted_lines) in

    {
      domain;
      meta = conll.Conll.meta;
      map = map_with_nl_nodes;
      fusion;
      highest_index = free_index -1;
      rules = String_map.empty;
    }

  (* -------------------------------------------------------------------------------- *)
  (** input : "Le/DET/le petit/ADJ/petit chat/NC/chat dort/V/dormir ./PONCT/." *)

  let re = Str.regexp "/\\(ADJ\\|ADJWH\\|ADV\\|ADVWH\\|CC\\|CLO\\|CLR\\|CLS\\|CS\\|DET\\|DETWH\\|ET\\|I\\|NC\\|NPP\\|P\\|P\\+D\\|P\\+PRO\\|PONCT\\|PREF\\|PRO\\|PROREL\\|PROWH\\|V\\|VIMP\\|VINF\\|VPP\\|VPR\\|VS\\)/"

  let of_brown ?domain ?sentid brown =
    let units = Str.split (Str.regexp " ") brown in
    let conll_lines = List.mapi
        (fun i item -> match Str.full_split re item with
           | [Str.Text form; Str.Delim pos; Str.Text lemma] ->
             let pos = String.sub pos 1 ((String.length pos)-2) in
             Conll.build_line ~id:(i+1,None) ~form ~lemma ~xpos:pos ~feats:[] ~deps:([((i,None), "SUC")]) ()
           | _ -> Error.build "[Graph.of_brown] Cannot parse Brown item >>>%s<<< (expected \"phon/POS/lemma\") in >>>%s<<<" item brown
        ) units in
    let meta = match sentid with Some id -> ["# sent_id = "^id] | None -> [] in
    of_conll ?domain { Conll.file=None; meta; lines=conll_lines; multiwords=[]; mwes=Conll_types.Int_map.empty; }

  (* -------------------------------------------------------------------------------- *)
  let of_pst ?domain pst =
    let cpt = ref 0 in
    let fresh_id () = incr cpt; !cpt - 1 in

    let leaf_list = ref [] in

    let rec loop nodes = function
      | Ast.Leaf (loc, phon) ->
        let fid = fresh_id () in
        let node = G_node.build_pst_leaf ~loc ?domain phon in
        leaf_list := fid :: ! leaf_list;
        (fid, Gid_map.add fid node nodes)

      | Ast.T (loc, cat, daughters) ->
        let fid = fresh_id () in
        let new_node = G_node.build_pst_node ~loc ?domain cat in
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
    {
      domain;
      meta=[];
      map=prec_loop 1 map (List.rev !leaf_list);
      fusion = [];
      highest_index = !cpt;
      rules = String_map.empty;
    }

  let update_edge_feature_opt ?loc edge_id feat_name new_value (src_gid,edge,tar_gid) graph =
    match Gid_map.find_opt src_gid graph.map with
    | None -> Error.run ?loc "[Graph.update_edge_feature_opt] cannot find source node of edge \"%s\"" edge_id
    | Some src_node ->
      match G_node.update_edge_opt tar_gid edge feat_name new_value src_node with
      | Some (new_node, new_edge) -> Some ({graph with map = Gid_map.add src_gid new_node graph.map}, new_edge)
      | None -> None

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

    let new_map = match G_node.get_pred_opt node with
      | None ->
        shifted_map
        |> (Gid_map.add new_gid (G_node.build ~position ()))
        |> (map_add_pred_succ new_gid gid)
      | Some prec_gid ->
        shifted_map
        |> (Gid_map.add new_gid (G_node.build ~position ()))
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
      | Some pos -> pos + 1 in
    let new_gid = graph.highest_index + 1 in
    let new_map =
      graph.map
      |> (Gid_map.add new_gid (G_node.build ~position ()))
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
    let map = Gid_map.add new_gid (G_node.build ()) graph.map in
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
  (* move out-edges (which respect cst [labels,neg]) from [src_gid] are moved to out-edges out off node [tar_gid] *)
  let shift_out loc src_gid tar_gid is_gid_local label_cst graph =
    let domain = get_domain_opt graph in
    let del_edges = ref [] and add_edges = ref [] in

    let src_node = Gid_map.find src_gid graph.map in
    let tar_node = Gid_map.find tar_gid graph.map in

    let src_next = G_node.get_next src_node in
    let tar_next = G_node.get_next tar_node in

    let (new_src_next, new_tar_next) =
      Massoc_gid.fold
        (fun (acc_src_next,acc_tar_next) next_gid edge ->
           if Label_cst.match_ ?domain label_cst edge && not (is_gid_local next_gid)
           then
             match Massoc_gid.add_opt next_gid edge acc_tar_next with
             | None when !Global.safe_commands -> Error.run ~loc "The [shift_out] command tries to build a duplicate edge (with label \"%s\")" (G_edge.dump edge)
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
  let shift_in loc src_gid tar_gid is_gid_local label_cst graph =
    let domain = get_domain_opt graph in
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
                      if Label_cst.match_ ?domain label_cst edge
                      then
                        match List_.usort_insert_opt edge acc_node_tar_edges with
                        | None when !Global.safe_commands ->
                          Error.run ~loc "The [shift_in] command tries to build a duplicate edge (with label \"%s\")" (G_edge.dump edge)
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
  let shift_edges loc src_gid tar_gid is_gid_local label_cst graph =
    let (g1,de1,ae1) = shift_out loc src_gid tar_gid is_gid_local label_cst graph in
    let (g2,de2,ae2) = shift_in loc src_gid tar_gid is_gid_local label_cst g1 in
    (g2, de1 @ de2, ae1 @ ae2)

  (* -------------------------------------------------------------------------------- *)
  let set_feat ?loc graph node_id feat_name new_value =
    let domain = get_domain_opt graph in
    let node = Gid_map.find node_id graph.map in
    let new_fs = G_fs.set_atom ?loc ?domain feat_name new_value (G_node.get_fs node) in
    let new_node = G_node.set_fs new_fs node in
    { graph with map = Gid_map.add node_id new_node graph.map }

  (* -------------------------------------------------------------------------------- *)
  let update_feat ?loc graph tar_id tar_feat_name item_list =
    let strings_to_concat =
      List.map
        (function
          | Concat_item.Feat (node_gid, feat_name) ->
            let node = Gid_map.find node_gid graph.map in
            (match G_fs.get_string_atom_opt feat_name (G_node.get_fs node) with
             | Some atom -> atom
             | None -> Error.run ?loc "Cannot update_feat, some feature (named \"%s\") is not defined" feat_name
            )
          | Concat_item.String s -> s
        ) item_list in
    let new_feature_value = List_.to_string (fun s->s) "" strings_to_concat in
    (set_feat ?loc graph tar_id tar_feat_name new_feature_value, new_feature_value)

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
  let to_json graph =
    let gr_id id = G_node.get_name id (Gid_map.find id graph.map) in

    let nodes = Gid_map.fold
        (fun id node acc ->
           let node_id = gr_id id
           and fs = G_node.get_fs node
           and succ =
             Massoc_gid.fold
               (fun acc tar edge ->
                  (`List [G_edge.to_json edge; `String (gr_id tar)]) :: acc
               ) [] (G_node.get_next node) in
           (node_id,`List [G_fs.to_json fs; `List succ])::acc
        ) graph.map [] in

    `Assoc nodes

  (* -------------------------------------------------------------------------------- *)
  let to_gr graph =
    let buff = Buffer.create 32 in
    bprintf buff "graph {\n";

    (* meta data *)
    List.iter (bprintf buff "  %s;\n") graph.meta;

    (* node_list *)
    let nodes = Gid_map.fold (fun gid node acc -> (gid,node)::acc) graph.map [] in
    let sorted_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) nodes in
    List.iter
      (fun (gid,node) ->
         bprintf buff "  N_%d %s;\n" gid (G_node.to_gr node)
      ) sorted_nodes;

    (* edges *)
    List.iter
      (fun (src_gid,node) ->
         Massoc_gid.iter
           (fun tar_gid edge ->
              match G_edge.to_string_opt edge with
              | Some s -> bprintf buff "  N_%d -[%s]-> N_%d;\n" src_gid s tar_gid
              | None -> ()
           ) (G_node.get_next node)
      ) sorted_nodes;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let fusion_item_space_after fi =
    try if List.assoc "SpaceAfter" fi.efs = "No" then "" else " "
    with Not_found -> " "

  let space_after gnode =
    match G_fs.get_string_atom_opt "_MISC_SpaceAfter" (G_node.get_fs gnode) with
    | Some "No" -> ""
    | _ -> " "

  let esc s = Str.global_replace (Str.regexp "<") "&lt;" s

  let to_sentence ?pivot ?(deco=G_deco.empty) graph =
    let high_list = match pivot with
      | None -> List.map fst deco.nodes
      | Some pivot ->
        match List.find_opt
                (fun (gid, (pattern_id,_)) -> pattern_id = pivot
                ) deco.nodes with
        | None -> Error.run "Undefined pivot %s" pivot
        | Some (gid,_) -> [gid] in

    let is_highlighted_gid gid = List.mem gid high_list in

    let inside fusion_item gid =
      let first = Gid_map.find fusion_item.first graph.map in
      let last = Gid_map.find fusion_item.last graph.map in
      let node = Gid_map.find gid graph.map in
      match (G_node.get_position_opt first, G_node.get_position_opt node, G_node.get_position_opt last) with
      | (Some f, Some n, Some l) when f <= n && n <= l -> true
      | _ -> false in

    let is_highlighted_fusion_item fusion_item =
      List.exists (fun gid -> inside fusion_item gid) high_list in

    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) nodes in

    let rec loop skip = function
      | [] -> ""
      | (gid, gnode)::gtail when skip = None ->
        begin
          match List.find_opt (fun fusion_item -> fusion_item.first=gid) graph.fusion with
          | Some fusion_item ->
            (if is_highlighted_fusion_item fusion_item
             then sprintf "<span class=\"highlight\">%s</span>" (esc fusion_item.word)
             else (esc fusion_item.word))
            ^ (fusion_item_space_after fusion_item)
            ^ (loop (Some fusion_item.last) gtail)
          | None ->
            match G_fs.to_word_opt (G_node.get_fs gnode) with
            | None -> (loop None gtail)
            | Some text ->
              (if is_highlighted_gid gid
               then sprintf "<span class=\"highlight\">%s</span>" (esc text)
               else esc (text))
              ^ (space_after gnode)
              ^ (loop None gtail)
        end
      | (gid, gnode)::gtail when skip = Some gid -> loop None gtail
      | (gid, gnode)::gtail -> loop skip gtail in

    Sentence.fr_clean_spaces (loop None snodes)


  let start_dur gnode =
    let fs = G_node.get_fs gnode in
    match (G_fs.get_string_atom_opt "_start" fs, G_fs.get_string_atom_opt "_stop" fs) with
    | (Some _start, Some _stop) ->
      let start = float_of_string _start
      and stop = float_of_string _stop in
      (start, stop -. start)
    | _ -> (-1., -1.)


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
          match (G_fs.get_string_atom_opt "_start" (G_node.get_fs node1), G_fs.get_string_atom_opt "_stop" (G_node.get_fs node2)) with
          | (Some i, Some f) -> (try Some (float_of_string i, float_of_string f) with Failure _ -> None)
          | _ -> None
        end
      | _ -> None in

    (Buffer.contents buff, bounds)


  (* -------------------------------------------------------------------------------- *)
  let is_non_lexical_node node =
    let fs = G_node.get_fs node in G_fs.get_string_atom_opt "kind" fs <> None

  let to_dep ?filter ?main_feat ?(deco=G_deco.empty) graph =
    let domain = get_domain_opt graph in

    (* split lexical // non-lexical nodes *)
    let (nodes, nl_nodes) = Gid_map.fold
        (fun id elt (acc1, acc2) ->
           if is_non_lexical_node elt
           then (acc1, (id,elt)::acc2)
           else ((id,elt)::acc1, acc2)
        ) graph.map ([],[]) in

    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) nodes in

    let insert (mid,mwe) nodes =
      let next_ids = Massoc_gid.fold (fun acc gid _ -> gid::acc) [] (G_node.get_next mwe) in
      let rec loop = function
        | [] -> [(mid,mwe)]
        | (h,n)::t when List.mem h next_ids -> (mid,mwe)::(h,n)::t
        | h::t -> h :: (loop t) in
      loop nodes in

    let all_nodes = List.fold_left (
        fun acc mwe -> insert mwe acc
      ) snodes nl_nodes
    in

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

         let style = match G_fs.get_string_atom_opt "void" fs with
           | Some "y" -> "; forecolor=red; subcolor=red; "
           | _ -> match G_fs.get_string_atom_opt "_UD_empty" fs with
             | Some "Yes" -> "; forecolor=purple; subcolor=purple; "
             | _ -> "" in

         bprintf buff "N_%s { %s%s }\n"
           (Gid.to_string id)
           dep_fs
           style
      ) all_nodes;
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
              match G_edge.to_dep_opt ?domain ~deco g_edge with
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
  exception Skip

  let to_conll graph =
    let ordered_nodes = Gid_map.fold
        (fun id node acc ->
           if G_node.get_position_opt node <> None
           then (id,node)::acc
           else acc
        ) graph.map [] in

    (* sort nodes wrt position *)
    let sorted_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) ordered_nodes in

    (* [mapping] associated gid to Conll.Id.t *)
    let (_,_,mapping) = List.fold_left
        (fun (acc_pos, acc_empty, acc) (gid,node) ->
           if G_node.get_position_opt node = Some 0
           then (acc_pos, acc_empty, Gid_map.add gid (0,None) acc)
           else
           if G_node.is_eud_empty node
           then
             let new_empty = match acc_empty with
               | None -> Some 1
               | Some 9 -> Error.run ("Too much empty nodes")
               | Some i -> Some (i+1) in
             (acc_pos, new_empty, Gid_map.add gid (acc_pos,new_empty) acc)
           else
             (acc_pos+1, None, Gid_map.add gid (acc_pos+1,None) acc)
        ) (0, None, Gid_map.empty) sorted_nodes in

    let all_govs = List.fold_left
        (fun acc (src_gid, node) ->
           Massoc_gid.fold
             (fun acc2 tar_gid edge ->
                match G_edge.to_string_opt edge with
                | None -> acc2
                | Some string_edge ->
                  let old = try Gid_map.find tar_gid acc2 with Not_found -> [] in
                  Gid_map.add tar_gid ((Gid_map.find src_gid mapping, string_edge) :: old) acc2
             ) acc (G_node.get_next node)
        ) Gid_map.empty sorted_nodes in

    let lines = List.map
        (fun (gid,node) ->
           let fs = G_node.get_fs node in
           let deps = try Gid_map.find gid all_govs with Not_found -> [] in

           Conll.build_line
             ~id: (Gid_map.find gid mapping)
             ~form: (match G_fs.get_string_atom_opt "form" fs with Some p -> p | None -> "_")
             ~lemma: (match G_fs.get_string_atom_opt "lemma" fs with Some p -> p | None -> "_")
             ~upos: (match G_fs.get_string_atom_opt "upos" fs with Some p -> p | None -> "_")
             ~xpos: (match G_fs.get_string_atom_opt "xpos" fs with Some p -> p | None -> "_")
             ~feats: (G_fs.to_conll ~exclude: ["form"; "lemma"; "upos"; "xpos"] fs)
             ~deps
             ()
        )
        (match sorted_nodes with
         | (_,h)::t when G_node.get_position_opt h = Some 0 -> t (* the first element in the Conll_root which must not be displayed *)
         | l -> l
        ) in

    let (_,mwes) = Gid_map.fold
        (fun _ node (num,acc) ->
           try
             let fs = G_node.get_fs node in
             let kind = match G_fs.get_string_atom_opt "kind" fs with
               | Some "NE" -> Mwe.Ne
               | Some "MWE" -> Mwe.Mwe
               | _ -> raise Skip in
             let nexts = G_node.get_next node in
             let next_list =
               List.sort_uniq
                 (fun gid1 gid2 ->
                    let n1 = List.assoc gid1 sorted_nodes
                    and n2 = List.assoc gid2 sorted_nodes in
                    match (G_node.get_position_opt n1, G_node.get_position_opt n2) with
                    | (Some i, Some j) -> Stdlib.compare i j
                    | _ -> 0
                 )
                 (Massoc_gid.fold (fun acc2 k _ -> k::acc2) [] nexts) in
             match next_list with
             | [] -> Error.bug "[G_graph.to_conll] mwe node with no next node"
             | head_gid::tail_gids ->
               let head_conll_id = Gid_map.find head_gid mapping in
               let head_proj = CCList.find_map
                   (fun e -> match G_edge.to_string_opt e with Some s -> int_of_string_opt s | None -> None)
                   (Massoc_gid.assoc head_gid nexts) in
               let items = List.fold_left
                   (fun acc gid ->
                      let conll_id = Gid_map.find gid mapping in
                      let proj = CCList.find_map
                          (fun e -> match G_edge.to_string_opt e with Some s -> int_of_string_opt s | None -> None)
                          (Massoc_gid.assoc gid nexts) in
                      Id_with_proj_set.add (conll_id, proj) acc
                   ) Id_with_proj_set.empty tail_gids in
               let mwe = {
                 Mwe.kind;
                 Mwe.mwepos = G_fs.get_string_atom_opt "mwepos" fs;
                 Mwe.label = G_fs.get_string_atom_opt "label" fs;
                 Mwe.criterion = G_fs.get_string_atom_opt "criterion" fs;
                 first = (head_conll_id,head_proj);
                 items;
               } in
               (num+1, Conll_types.Int_map.add num mwe acc)
           with Skip -> (num,acc)
        ) graph.map (1,Conll_types.Int_map.empty) in

    let meta =
      if !Global.track_rules
      then graph.meta @ ["# rules = " ^ (string_rules graph)]
      else graph.meta in

    { Conll.void with Conll.meta = meta; lines; mwes; }

  let to_conll_string ?cupt graph =
    let conll = to_conll graph in
    Conll.to_string ?cupt (Conll.normalize_multiwords conll)

  (* -------------------------------------------------------------------------------- *)
  let to_dot ?main_feat ?(get_url = fun _ -> None) ?(deco=G_deco.empty) graph =
    let domain = get_domain_opt graph in
    let buff = Buffer.create 32 in

    bprintf buff "digraph G {\n";
    bprintf buff "  node [shape=Mrecord];\n";

    (* nodes *)
    Gid_map.iter
      (fun id node ->
         let decorated_feat =
           try List.assoc id deco.G_deco.nodes
           with Not_found -> ("",[]) in
         let fs = G_node.get_fs node in
         let lab_url = match G_fs.get_string_atom_opt "label" fs with
           | None -> None
           | Some lab ->
             match get_url lab with
             | None -> None
             | Some url -> Some (lab,url)
         in
         bprintf buff "  N_%s [label=<%s>%s]\n"
           (Gid.to_string id)
           (G_fs.to_dot ~decorated_feat ?main_feat fs)
           (match lab_url with None -> "" | Some (lab,url) -> sprintf ", URL=\"%s\", target=_blank, tooltip=\"%s\", shape=record" url lab)
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
                match G_edge.to_dot_opt ?domain ~deco g_edge with
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

  let cast ?domain graph = match (domain, graph.domain) with
    | (None, _) -> graph
    | (Some new_domain, Some dom) when dom == new_domain ->
      (* ====== NO CAST NEEDED ====== *) graph
    | _ ->
      (* ====== CASTING NEEDED ====== *) of_conll ?domain (to_conll graph)

  let is_projective t =
    let (arc_positions, pos_to_gid_map) =
      Gid_map.fold (fun src_gid src_node (acc, acc_map) ->
          match G_node.get_position_opt src_node with
          | None -> (acc, acc_map)
          | Some src_pos ->
            let new_acc = Massoc_gid.fold (fun acc2 tar_gid edge ->
                let tar_node = find tar_gid t in
                match G_node.get_position_opt tar_node with
                | None -> acc2
                | Some tar_pos -> (min src_pos tar_pos, max src_pos tar_pos) :: acc2
              ) acc (G_node.get_next_without_pred_succ src_node) in
            (new_acc, Int_map.add src_pos src_gid acc_map)
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
           ) acc (G_node.get_next_without_pred_succ node)
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
        ) (G_node.get_next_without_pred_succ node);
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
    feats: ((Gid.t * feature_name) * (value option)) list;
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
      try (new_val_opt = G_fs.get_atom_opt feat_name (G_node.get_fs (G_graph.find gid seed_graph)))
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

  }

  let from_graph graph = { graph; seed=graph; delta = Delta.empty; added_gids = []; e_mapping = String_map.empty; added_gids_in_rule =[]; }

  (* WARNING: compare is correct only on data with the same seed! *)
  let compare t1 t2 = Stdlib.compare (t1.delta,t1.added_gids) (t2.delta, t2.added_gids)
end (* module Graph_with_history*)

(* ================================================================================ *)
module Graph_with_history_set = Set.Make (Graph_with_history)
