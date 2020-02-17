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
module Concat_item = struct
  type t =
    | Feat of (Gid.t * feature_name)
    | String of string
end (* module Concat_item *)

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
  let map_add_edge map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Pid_map.find id_src map with Not_found -> P_node.empty in
    match P_node.add_edge label id_tar node_src with
    | None -> None
    | Some new_node -> Some (Pid_map.add id_src new_node map)

  (* -------------------------------------------------------------------------------- *)
  let build ?domain lexicons basic_ast =
    let (full_node_list : Ast.node list) = basic_ast.Ast.pat_nodes
    and full_edge_list = basic_ast.Ast.pat_edges in

    (* NB: insert searches for a previous node with the Same name and uses unification rather than constraint *)
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

    let (map : t) =
      List.fold_left
        (fun acc (ast_edge, loc) ->
           let i1 = Id.build ~loc ast_edge.Ast.src pos_table in
           let i2 = Id.build ~loc ast_edge.Ast.tar pos_table in
           let edge = P_edge.build ?domain (ast_edge, loc) in
           (match map_add_edge acc (Pid.Pos i1) edge (Pid.Pos i2) with
            | Some g -> g
            | None -> Error.build ~loc "[Graph.build] try to build a graph with twice the same edge %s"
                        (P_edge.to_string ?domain edge)
           )
        ) map_without_edges full_edge_list in
    (map, pos_table)


  (* -------------------------------------------------------------------------------- *)
  (* a type for extension of graph (a former graph exists):
     in grew the former is a positive basic and an extension is a negative basic ("without") *)
  type extension = {
    ext_map: t; (* node description for new nodes and for edge "Old -> New"  *)
    old_map: t; (* a partial map for new constraints on old nodes "Old [...]" *)
  }

  (* -------------------------------------------------------------------------------- *)
  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_extension ?domain lexicons pos_table full_node_list full_edge_list =

    let built_nodes = List.map (P_node.build_from_ast ?domain lexicons) full_node_list in

    let (old_nodes, new_nodes) =
      List.partition
        (function (id,_) when Array_.dicho_mem id pos_table -> true | _ -> false)
        built_nodes in

    let new_sorted_nodes = List.sort (fun (id1,_) (id2,_) -> Stdlib.compare id1 id2) new_nodes in

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
           let edge = P_edge.build ?domain (ast_edge, loc) in
           match map_add_edge acc i1 edge i2 with
           | Some map -> map
           | None -> Error.bug "[Graph.build_extension] add_edge cannot fail in pattern extension"
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

  let shift_fusion_item n fusion_item =
    { fusion_item with
      first = fusion_item.first + n;
      last = fusion_item.last + n;
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

  let shift user_id n graph =
    { graph with
      fusion = List.map (shift_fusion_item n) graph.fusion;
      map = Gid_map.map_key_value (fun i -> i+n) (fun node -> G_node.shift user_id n node) graph.map;
      highest_index = graph.highest_index + n;
    }

  let unshift user_id graph =
    { graph with
      map = Gid_map.map (fun node -> G_node.unshift user_id node) graph.map;
    }

  let empty = { domain=None; meta=[]; map=Gid_map.empty; fusion=[]; highest_index=0; rules=String_map.empty; }

  let is_empty t = Gid_map.is_empty t.map

  let size t = Gid_map.cardinal (t.map)

  let get_domain t = t.domain

  let find node_id graph = Gid_map.find node_id graph.map

  let equals t t' = Gid_map.equal (=) t.map t'.map

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
    let domain = get_domain graph in
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> Label_cst.match_ ?domain label_cst e) (G_node.get_next node)

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge_opt map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Gid_map.find id_src map with Not_found -> (G_node.build ()) in
    match G_node.add_edge label id_tar node_src with
    | None -> None
    | Some new_node -> Some (Gid_map.add id_src new_node map)

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge map id_src label id_tar =
    let node_src = Gid_map.find id_src map in
    match G_node.add_edge label id_tar node_src with
    | Some new_node -> Gid_map.add id_src new_node map
    | None -> Error.bug "[Graph.map_add_edge] duplicate edge"

  (* -------------------------------------------------------------------------------- *)
  let add_edge graph id_src label id_tar =
    match map_add_edge_opt graph.map id_src label id_tar with
    | Some new_map -> Some {graph with map = new_map }
    | None -> None

  (* -------------------------------------------------------------------------------- *)
  let build ?domain gr_ast =

    let (ordered_nodes, unordered_nodes) =
      List.fold_left
        (fun (orderd_acc, unordered_acc) (node,loc) ->
           match Id.get_pos node.Ast.node_id with
           | Some p -> ((p,(node,loc)) :: orderd_acc, unordered_acc)
           | None -> (orderd_acc, (node,loc) :: unordered_acc)
        ) ([],[]) gr_ast.Ast.nodes in

    let sorted_nodes = List.sort (fun (p1,_) (p2,_) -> Stdlib.compare p1 p2) ordered_nodes in

    let rec loop already_bound index prec = function
      | [] -> (Gid_map.empty,[])
      | (_, (ast_node, loc))::tail ->
        let node_id = ast_node.Ast.node_id in
        if List.mem node_id already_bound
        then Error.build ~loc "[GRS] [Graph.build] try to build a graph with twice the same node id '%s'" node_id
        else
          let (new_tail, table) = loop (node_id :: already_bound) (index+1) (Some index) tail in
          let succ = if tail = [] then None else Some (index+1) in
          let new_node = G_node.build_from_ast ?domain ?prec ?succ ~position:index (ast_node, loc) in
          (
            Gid_map.add index new_node new_tail,
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
           let edge = G_edge.build ?domain (ast_edge, loc) in
           (match map_add_edge_opt acc i1 edge i2 with
            | Some g -> g
            | None -> Error.build ~loc "[Graph.build] try to build a graph with twice the same edge %s"
                        (G_edge.to_string ?domain edge)
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

    let rec loop index prec = function
      | [] -> Gid_map.empty
      | [last] ->
        let loc = Loc.file_opt_line conll.Conll.file last.Conll.line_num in
        Gid_map.add index (G_node.build_from_conll ~loc ?domain ?prec (Some index) last) Gid_map.empty
      | line::tail ->
        let loc = Loc.file_opt_line conll.Conll.file line.Conll.line_num in
        Gid_map.add index (G_node.build_from_conll ~loc ?domain ?prec ~succ:(index+1) (Some index) line)
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
                let edge = G_edge.from_string ?domain ~loc dep_lab in
                (match map_add_edge_opt acc2 gov_id edge dep_id with
                 | Some g -> g
                 | None -> Error.build ~loc "[Graph.of_conll] try to build a graph with twice the same edge %s"
                             (G_edge.to_string ?domain edge)
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
           let new_map_2 = map_add_edge new_map_1 free_index (G_edge.from_string ?domain kind) (Id.gbuild (fst mwe.Mwe.first) gtable) in
           let new_map_3 = match snd mwe.Mwe.first with
             | None -> new_map_2
             | Some i -> map_add_edge new_map_2 free_index (G_edge.from_string ?domain (sprintf "%d" i)) (Id.gbuild (fst mwe.Mwe.first) gtable) in

           (* add a link to each other component *)
           let new_map_4 =
             Id_with_proj_set.fold (
               fun item acc2 ->
                 let tmp = map_add_edge acc2 free_index (G_edge.from_string ?domain kind) (Id.gbuild (fst item) gtable) in
                 match snd item with
                 | None -> tmp
                 | Some i -> map_add_edge tmp free_index (G_edge.from_string ?domain (sprintf "%d" i)) (Id.gbuild (fst item) gtable)
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
               map_add_edge new_map fid G_edge.sub daughter_id
            ) with_mother daughters in
        (fid, new_nodes) in

    let (_,map) = loop Gid_map.empty pst in

    let rec prec_loop map = function
      | [] | [_] -> map
      | n1 :: n2 :: tail ->
        let new_map = prec_loop map (n2 :: tail) in

        let node1 = Gid_map.find n1 new_map
        and node2 = Gid_map.find n2 new_map in
        new_map
        |> (Gid_map.add n1 (G_node.set_succ n2 node1))
        |> (Gid_map.add n2 (G_node.set_prec n1 node2)) in

    {
      domain;
      meta=[];
      map=prec_loop map (List.rev !leaf_list);
      fusion = [];
      highest_index = !cpt;
      rules = String_map.empty;
    }

  let update_edge_feature ?loc edge_id feat_name new_value (src_gid,edge,tar_gid) graph =
    match Gid_map.find_opt src_gid graph.map with
    | None -> Error.run ?loc "[Graph.update_edge_feature] cannot find source node of edge \"%s\"" edge_id
    | Some src_node ->
      match G_node.update_edge tar_gid edge feat_name new_value src_node with
      | Some (new_node, new_edge) -> Some ({graph with map = Gid_map.add src_gid new_node graph.map}, new_edge)
      | None -> None

  let del_edge_feature ?loc edge_id feat_name (src_gid,edge,tar_gid) graph =
    match Gid_map.find_opt src_gid graph.map with
    | None -> Error.run ?loc "[Graph.del_edge_feature] cannot find source node of edge \"%s\"" edge_id
    | Some src_node ->
      match G_node.del_edge_feature tar_gid edge feat_name src_node with
      | Some (new_node, new_edge) -> Some ({graph with map = Gid_map.add src_gid new_node graph.map}, new_edge)
      | None -> None

  (* -------------------------------------------------------------------------------- *)
  let del_edge ?loc src_gid label tar_gid graph =
    match Gid_map.find_opt src_gid graph.map with
    | None -> Error.bug ?loc "[Graph.del_edge] Some edge refers to a dead node"
    | Some src_node ->
      match G_node.remove_edge tar_gid label src_node with
      | None -> None
      | Some new_node -> Some {graph with map = Gid_map.add src_gid new_node graph.map}


  (* -------------------------------------------------------------------------------- *)
  (* [shift_position delta gid map] applies a position shift of [delta] to all nodes
     of the [map] that are successors of [gid] *)
  let rec shift_position delta gid map =
    let node = Gid_map.find gid map in
    match G_node.get_position node with
    | None -> Error.run "[G_node.shift_position] unordered node"
    | Some p ->
      let new_node = G_node.set_position (p + delta) node in
      let next_map = Gid_map.add gid new_node map in
      match G_node.get_succ node with
      | None -> next_map
      | Some next_gid -> shift_position delta next_gid next_map

  (* -------------------------------------------------------------------------------- *)
  let add_before gid graph =
    let node = Gid_map.find gid graph.map in
    let position = match G_node.get_position node with
      | None -> Error.run "[G_node.insert_before] unordered node"
      | Some p -> p in
    let shifted_map = shift_position 1 gid graph.map in
    let new_gid = graph.highest_index + 1 in

    let new_map = match G_node.get_prec node with
      | None ->
        shifted_map
        |> (Gid_map.add new_gid (G_node.build ~succ:gid ~position ()))
        |> (Gid_map.add gid (G_node.set_prec new_gid node))
      | Some prec_gid ->
        shifted_map
        |> (Gid_map.add new_gid (G_node.build ~prec:prec_gid ~succ:gid ~position ()))
        |> (Gid_map.add prec_gid (G_node.set_succ new_gid (Gid_map.find prec_gid graph.map)))
        |> (Gid_map.add gid (G_node.set_prec new_gid node)) in

    (new_gid, { graph with map=new_map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  (* WARNING: use only if [last_gid] is the last ordered node in graph! *)
  let append last_gid graph =
    let last_node = Gid_map.find last_gid graph.map in
    let new_pos = match G_node.get_position last_node with
      | None -> Error.run "[G_node.append] unordered nodes"
      | Some pos -> pos + 1 in
    let new_gid = graph.highest_index + 1 in
    let new_map =
      graph.map
      |> (Gid_map.add new_gid (G_node.build ~prec:last_gid ~position:new_pos ()))
      |> (Gid_map.add last_gid (G_node.set_succ new_gid last_node)) in
    (new_gid, { graph with map=new_map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let add_after gid graph =
    match G_node.get_succ (Gid_map.find gid graph.map) with
    | Some gid_succ -> add_before gid_succ graph
    | None -> append gid graph

  (* -------------------------------------------------------------------------------- *)
  let add_unordered graph =
    let new_gid = graph.highest_index + 1 in
    let map = Gid_map.add new_gid (G_node.build ()) graph.map in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  (* build a new map when one node is remove from the list of ordered nodes:
     1) update prec and succ fields of neighbour
     2) shift position of nodes after [node]
  *)
  let map_unorder node map =
    match (G_node.get_prec node, G_node.get_succ node) with
    | (Some id_prec, Some id_succ) ->
      begin
        let prec = Gid_map.find id_prec map
        and succ = Gid_map.find id_succ map in
        map
        |> (Gid_map.add id_prec (G_node.set_succ id_succ prec))
        |> (Gid_map.add id_succ (G_node.set_prec id_prec succ))
        |> (shift_position (-1) id_succ)
      end
    | (Some id_prec, None) ->
      begin
        let prec = Gid_map.find id_prec map in
        map
        |> (Gid_map.add id_prec (G_node.remove_succ prec))
      end
    | (None, Some id_succ) ->
      begin
        let succ = Gid_map.find id_succ map in
        map
        |> (Gid_map.add id_succ (G_node.remove_prec succ))
        |> (shift_position (-1) id_succ)
      end
    | (None, None) -> map

  (* -------------------------------------------------------------------------------- *)
  let unorder node_id graph =
    match Gid_map.find_opt node_id graph.map with
    | None -> None
    | Some node ->
      match G_node.get_position node with
      | None -> None
      | Some _ ->
        let new_map =
          graph.map
          |> (Gid_map.add node_id (G_node.unset_position node))
          |> (map_unorder node) in
        Some { graph with map = new_map }

  (* -------------------------------------------------------------------------------- *)
  let del_node graph node_id =
    match Gid_map.find_opt node_id graph.map with
    | None -> None
    | Some node ->
      let map_wo_node =
        Gid_map.fold
          (fun id value acc ->
             if id = node_id
             then acc
             else Gid_map.add id (G_node.remove_key node_id value) acc
          ) graph.map Gid_map.empty in
      Some { graph with map = map_unorder node map_wo_node }

  (* -------------------------------------------------------------------------------- *)
  (* move out-edges (which respect cst [labels,neg]) from id_src are moved to out-edges out off node id_tar *)
  let shift_out loc src_gid tar_gid is_gid_local label_cst graph =
    let domain = get_domain graph in
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
             | None when !Global.safe_commands -> Error.run ~loc "The [shift_out] command tries to build a duplicate edge (with label \"%s\")" (G_edge.to_string ?domain edge)
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

    let new_map = graph.map
                  |> (Gid_map.add src_gid (G_node.set_next new_src_next src_node))
                  |> (Gid_map.add tar_gid (G_node.set_next new_tar_next tar_node)) in
    ( { graph with map = new_map },
      !del_edges,
      !add_edges
    )

  (* -------------------------------------------------------------------------------- *)
  let shift_in loc src_gid tar_gid is_gid_local label_cst graph =
    let domain = get_domain graph in
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
                        match List_.usort_insert edge acc_node_tar_edges with
                        | None when !Global.safe_commands ->
                          Error.run ~loc "The [shift_in] command tries to build a duplicate edge (with label \"%s\")" (G_edge.to_string ?domain edge)
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
    let domain = get_domain graph in
    let node = Gid_map.find node_id graph.map in
    let new_node =
      match feat_name with
      | "position" -> G_node.set_position (int_of_string new_value) node
      | _ ->
        let new_fs = G_fs.set_atom ?loc ?domain feat_name new_value (G_node.get_fs node) in
        (G_node.set_fs new_fs node) in
    { graph with map = Gid_map.add node_id new_node graph.map }

  (* -------------------------------------------------------------------------------- *)
  let update_feat ?loc graph tar_id tar_feat_name item_list =
    let strings_to_concat =
      List.map
        (function
          | Concat_item.Feat (node_gid, "position") ->
            let node = Gid_map.find node_gid graph.map in
            begin
              match G_node.get_position node with
              | Some p -> sprintf "%d" p
              | _ -> Error.run ?loc "Try to read position of an unordered node"
            end
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
  let append_feats ?loc graph src_id tar_id separator regexp =
    let src_node = Gid_map.find src_id graph.map in
    let tar_node = Gid_map.find tar_id graph.map in
    match G_node.append_feats ?loc src_node tar_node separator regexp with
    | Some (new_tar_node, updated_feats) ->
      Some ({ graph with map = Gid_map.add tar_id new_tar_node graph.map }, updated_feats)
    | None -> None

  (* -------------------------------------------------------------------------------- *)
  let del_feat graph node_id feat_name =
    let node = Gid_map.find node_id graph.map in
    match G_fs.del_feat feat_name (G_node.get_fs node) with
    | Some new_fs -> Some { graph with map = Gid_map.add node_id (G_node.set_fs new_fs node) graph.map }
    | None -> None

  (* -------------------------------------------------------------------------------- *)
  let to_json graph =
    let domain = get_domain graph in

    let gr_id id = G_node.get_name id (Gid_map.find id graph.map) in

    let nodes = Gid_map.fold
        (fun id node acc ->
           let node_id = gr_id id
           and fs = G_node.get_fs node
           and succ =
             Massoc_gid.fold
               (fun acc tar edge ->
                  (`List [`String (G_edge.to_string ?domain edge); `String (gr_id tar)]) :: acc
               ) [] (G_node.get_next node) in
           (node_id,`List [G_fs.to_json fs; `List succ])::acc
        ) graph.map [] in

    `Assoc nodes

  (* -------------------------------------------------------------------------------- *)
  let to_gr graph =
    let domain = get_domain graph in
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
              bprintf buff "  N_%d -[%s]-> N_%d;\n" src_gid (G_edge.to_string ?domain edge) tar_gid
           ) (G_node.get_next node)
      ) sorted_nodes;

    bprintf buff "}\n";
    Buffer.contents buff

  (* -------------------------------------------------------------------------------- *)
  let fusion_item_space_after fi =
    try if List.assoc "SpaceAfter" fi.efs = "No" then "" else " "
    with Not_found -> " "

  let space_after gnode =
    match G_fs.get_string_atom "_MISC_SpaceAfter" (G_node.get_fs gnode) with
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
      match (G_node.get_position first, G_node.get_position node, G_node.get_position last) with
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
            match G_fs.to_word (G_node.get_fs gnode) with
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
    match (G_fs.get_string_atom "_start" fs, G_fs.get_string_atom "_stop" fs) with
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
        match G_fs.to_word (G_node.get_fs gnode) with
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
    Buffer.contents buff


  (* -------------------------------------------------------------------------------- *)
  let is_non_lexical_node node =
    let fs = G_node.get_fs node in G_fs.get_string_atom "kind" fs <> None

  let to_dep ?filter ?main_feat ?(deco=G_deco.empty) graph =
    let domain = get_domain graph in

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
         let pos = G_node.get_position node in
         let dep_fs = G_fs.to_dep ~decorated_feat ?position:pos ?filter ?main_feat fs in

         let style = match G_fs.get_string_atom "void" fs with
           | Some "y" -> "; forecolor=red; subcolor=red; "
           | _ -> match G_fs.get_string_atom "_UD_empty" fs with
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
              let deco = List.mem (gid,g_edge,tar) deco.G_deco.edges in
              bprintf buff "N_%s -> N_%s %s\n" (Gid.to_string gid) (Gid.to_string tar) (G_edge.to_dep ?domain ~deco g_edge)
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

    let domain = get_domain graph in

    let ordered_nodes = Gid_map.fold
        (fun id node acc ->
           if G_node.get_position node <> None
           then (id,node)::acc
           else acc
        ) graph.map [] in

    (* sort nodes wrt position *)
    let sorted_nodes = List.sort (fun (_,n1) (_,n2) -> G_node.compare n1 n2) ordered_nodes in

    (* [mapping] associated gid to Conll.Id.t *)
    let (_,_,mapping) = List.fold_left
        (fun (acc_pos, acc_empty, acc) (gid,node) ->
           if G_node.get_position node = Some 0
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
                let old = try Gid_map.find tar_gid acc2 with Not_found -> [] in
                Gid_map.add tar_gid ((Gid_map.find src_gid mapping, G_edge.to_string ?domain edge) :: old) acc2
             ) acc (G_node.get_next node)
        ) Gid_map.empty sorted_nodes in

    let lines = List.map
        (fun (gid,node) ->
           let fs = G_node.get_fs node in
           let deps = try Gid_map.find gid all_govs with Not_found -> [] in

           Conll.build_line
             ~id: (Gid_map.find gid mapping)
             ~form: (match G_fs.get_string_atom "form" fs with Some p -> p | None -> "_")
             ~lemma: (match G_fs.get_string_atom "lemma" fs with Some p -> p | None -> "_")
             ~upos: (match G_fs.get_string_atom "upos" fs with Some p -> p | None -> "_")
             ~xpos: (match G_fs.get_string_atom "xpos" fs with Some p -> p | None -> "_")
             ~feats: (G_fs.to_conll ~exclude: ["form"; "lemma"; "upos"; "xpos"; "position"] fs)
             ~deps
             ()
        )
        (match sorted_nodes with
         | (_,h)::t when G_node.get_position h = Some 0 -> t (* the first element in the Conll_root which must not be displayed *)
         | l -> l
        ) in

    let (_,mwes) = Gid_map.fold
        (fun _ node (num,acc) ->
           try
             let fs = G_node.get_fs node in
             let kind = match G_fs.get_string_atom "kind" fs with
               | Some "NE" -> Mwe.Ne
               | Some "MWE" -> Mwe.Mwe
               | _ -> raise Skip in
             let nexts = G_node.get_next node in
             let next_list =
               List.sort_uniq
                 (fun gid1 gid2 ->
                    let n1 = List.assoc gid1 sorted_nodes
                    and n2 = List.assoc gid2 sorted_nodes in
                    match (G_node.get_position n1, G_node.get_position n2) with
                    | (Some i, Some j) -> Stdlib.compare i j
                    | _ -> 0
                 )
                 (Massoc_gid.fold (fun acc2 k _ -> k::acc2) [] nexts) in
             match next_list with
             | [] -> Error.bug "[G_graph.to_conll] mwe node with no next node"
             | head_gid::tail_gids ->
               let head_conll_id = Gid_map.find head_gid mapping in
               let head_proj = CCList.find_map
                   (fun e -> int_of_string_opt (G_edge.to_string ?domain e))
                   (Massoc_gid.assoc head_gid nexts) in
               let items = List.fold_left
                   (fun acc gid ->
                      let conll_id = Gid_map.find gid mapping in
                      let proj = CCList.find_map
                          (fun e -> int_of_string_opt (G_edge.to_string ?domain e))
                          (Massoc_gid.assoc gid nexts) in
                      Id_with_proj_set.add (conll_id, proj) acc
                   ) Id_with_proj_set.empty tail_gids in
               let mwe = {
                 Mwe.kind;
                 Mwe.mwepos = G_fs.get_string_atom "mwepos" fs;
                 Mwe.label = G_fs.get_string_atom "label" fs;
                 Mwe.criterion = G_fs.get_string_atom "criterion" fs;
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
    let domain = get_domain graph in
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
         let lab_url = match G_fs.get_string_atom "label" fs with
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
              else bprintf buff "  N_%s -> N_%s%s;\n" (Gid.to_string id) (Gid.to_string tar) (G_edge.to_dot ?domain ~deco g_edge)
           ) (G_node.get_next node)
      ) graph.map;

    (* Set "rank=same" and more edge in debug modes *)
    Gid_map.iter
      (fun id node ->
         begin
           match G_node.get_succ node with
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
          match G_node.get_position src_node with
          | None -> (acc, acc_map)
          | Some src_pos ->
            let new_acc = Massoc_gid.fold (fun acc2 tar_gid edge ->
                let tar_node = find tar_gid t in
                match G_node.get_position tar_node with
                | None -> acc2
                | Some tar_pos -> (min src_pos tar_pos, max src_pos tar_pos) :: acc2
              ) acc (G_node.get_next src_node) in
            (new_acc, Int_map.add src_pos src_gid acc_map)
        ) t.map ([], Int_map.empty) in
    let sorted_arc_positions = List.sort Dependencies.lex_cmp arc_positions in
    match Dependencies.is_projective sorted_arc_positions with
    | Some (p1, p2) -> Some (Int_map.find p1 pos_to_gid_map, Int_map.find p1 pos_to_gid_map)
    | None -> None

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
           ) acc (G_node.get_next node)
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
        ) (G_node.get_next node);
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
    match List_.usort_insert gid t.del_nodes with
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
      try (new_val_opt = G_fs.get_atom feat_name (G_node.get_fs (G_graph.find gid seed_graph)))
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
    match List_.usort_insert gid t.unordered_nodes with
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

(* ================================================================================ *)
module Multigraph = struct
  module String_set = Set.Make(String)

  type t = {
    graph: G_graph.t;
    users: String_set.t;
  }

  let empty = { graph = G_graph.empty; users = String_set.empty }
  let is_empty t = G_graph.is_empty t.graph

  let to_graph t = t.graph

  let remove_layer user_id t =
    let new_map = Gid_map.filter
        (fun gid node ->
           G_fs.get_string_atom "user" (G_node.get_fs node) <> Some user_id
        ) t.graph.map in
    let new_graph = { t.graph with map = new_map } in
    { graph = new_graph; users = String_set.remove user_id t.users }

  let add_layer user_id layer t =
    (* first remove old binding if any *)
    let new_t =
      if String_set.mem user_id t.users
      then remove_layer user_id t
      else t in

    (* Shift the new layer to new gids *)
    let shifted_layer = G_graph.shift user_id (new_t.graph.G_graph.highest_index + 1) layer in
    let union_map = Gid_map.union (fun _ _ _ -> failwith "overlap") new_t.graph.map shifted_layer.G_graph.map in
    let new_graph =
      if is_empty t
      then { new_t.graph with map = union_map; highest_index = shifted_layer.highest_index; meta=layer.meta;  }
      else { new_t.graph with map = union_map; highest_index = shifted_layer.highest_index } in
    { graph = new_graph; users = String_set.add user_id new_t.users }

  let get_users t = t.users

  let user_graph user_id t =
    if not (String_set.mem user_id t.users)
    then None
    else
      let full_graph = t.graph in
      let new_map = Gid_map.fold
          (fun gid node acc ->
             let fs = G_node.get_fs node in
             match (G_fs.get_string_atom "user" fs, G_fs.del_feat "user" fs) with
             | (Some u, Some new_fs) when u = user_id -> Gid_map.add gid (G_node.set_fs new_fs node) acc
             | _ -> acc
          ) full_graph.G_graph.map Gid_map.empty in

      Some { full_graph with G_graph.map = new_map}

  let graphs t =
    String_set.fold
      (fun user_id acc ->
         match user_graph user_id t with
         | None -> acc
         | Some g -> (user_id, G_graph.unshift user_id g) :: acc
      ) t.users []

  let save out_ch t =
    String_set.iter
      (fun user_id ->
         match user_graph user_id t with
         | None -> ()
         | Some g ->
           fprintf out_ch "# user_id = %s\n%s\n"
             user_id
             (G_graph.to_conll_string g)
      ) t.users
end
