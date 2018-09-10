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
  let build ?domain lexicons (full_node_list : Ast.node list) full_edge_list =

    (* NB: insert searches for a previous node with the Same name and uses unification rather than constraint *)
    (* NB: insertion of new node at the end of the list: not efficient but graph building is not the hard part. *)
    let rec insert (ast_node, loc) = function
      | [] -> [P_node.build ?domain lexicons (ast_node, loc)]
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
          let edge = P_edge.build ?domain (ast_edge, loc) in
          (match map_add_edge acc (Pid.Pos i1) edge (Pid.Pos i2) with
            | Some g -> g
            | None -> Error.build "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s"
              (P_edge.to_string ?domain edge)
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
  let build_extension ?domain lexicons pos_table full_node_list full_edge_list =

    let built_nodes = List.map (P_node.build ?domain lexicons) full_node_list in

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
          let edge = P_edge.build ?domain (ast_edge, loc) in
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
  (* value is (f, Some g) for combined request "f=v/g=u" and (j, None) else *)
  type highlighted_feat = string * string option

  type t = {
    nodes: (Gid.t * (string * highlighted_feat list)) list;  (* a list of (node, (pattern_id, features of nodes implied in the step)) *)
    edges: (Gid.t * G_edge.t * Gid.t) list;        (* an edge list *)
  }

  let empty = {nodes=[]; edges=[]}
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
  }

  let empty = { domain=None; meta=[]; map=Gid_map.empty; fusion=[]; highest_index=0; }

  let get_domain t = t.domain

  let get_highest g = g.highest_index

  let find node_id graph = Gid_map.find node_id graph.map

  let equals t t' = Gid_map.equal (=) t.map t'.map

  let node_exists fct t = Gid_map.exists (fun _ node -> fct node) t.map

  let fold_gid fct t init =
    Gid_map.fold (fun gid _ acc -> fct gid acc) t.map init

  (* is there an edge e out of node i ? *)
  let edge_out graph node_id label_cst =
    let domain = get_domain graph in
    let node = Gid_map.find node_id graph.map in
    Massoc_gid.exists (fun _ e -> Label_cst.match_ ?domain label_cst e) (G_node.get_next node)

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge_opt map id_src label id_tar =
    let node_src =
      (* Not found can be raised when adding an edge from pos to neg *)
      try Gid_map.find id_src map with Not_found -> G_node.empty in
    match G_node.add_edge label id_tar node_src with
      | None -> None
      | Some new_node -> Some (Gid_map.add id_src new_node map)

  (* -------------------------------------------------------------------------------- *)
  let map_add_edge map id_src label id_tar =
    let node_src = Gid_map.find id_src map in
    match G_node.add_edge label id_tar node_src with
      | Some new_node -> Gid_map.add id_src new_node map
      | None -> Log.fbug "[Graph.map_add_edge] duplicate"; exit 2

  (* -------------------------------------------------------------------------------- *)
  let add_edge graph id_src label id_tar =
    match map_add_edge_opt graph.map id_src label id_tar with
      | Some new_map -> Some {graph with map = new_map }
      | None -> None

  (* -------------------------------------------------------------------------------- *)
  let build ?domain ?(grewpy=false) gr_ast =
    let full_node_list =
      if grewpy
      then List.sort (Ast.grewpy_compare) gr_ast.Ast.nodes
      else gr_ast.Ast.nodes
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
          let new_node = G_node.build ?domain ?prec ?succ ~position:(float index) (ast_node, loc) in
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
          let edge = G_edge.build ?domain (ast_edge, loc) in
          (match map_add_edge_opt acc i1 edge i2 with
            | Some g -> g
            | None -> Error.build "[GRS] [Graph.build] try to build a graph with twice the same edge %s %s"
              (G_edge.to_string ?domain edge)
              (Loc.to_string loc)
          )
        ) map_without_edges full_edge_list in

    {
      domain;
      meta=gr_ast.Ast.meta;
      map;
      fusion = [];
      highest_index = (List.length full_node_list) -1
    }

  (* -------------------------------------------------------------------------------- *)
  let of_json = function
  | `Assoc (l : (string * Yojson.Basic.json) list) ->
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
      in build ~grewpy:true graph_ast
  | _ -> Error.build "[Graph.of_json] not an assoc list"

  (* -------------------------------------------------------------------------------- *)
  let of_conll ?domain conll =
    let sorted_lines = Conll.root :: (List.sort Conll.compare conll.Conll.lines) in

    (* [gtable] maps *)
    let gtable = (Array.of_list (List.map (fun line -> line.Conll.id) sorted_lines), Conll.Id.to_dot) in

    let rec loop index prec = function
      | [] -> Gid_map.empty
      | [last] ->
        let loc = Loc.file_opt_line conll.Conll.file last.Conll.line_num in
        Gid_map.add index (G_node.of_conll ?domain ~loc ?prec last) Gid_map.empty
      | line::tail ->
        let loc = Loc.file_opt_line conll.Conll.file line.Conll.line_num in
        Gid_map.add index (G_node.of_conll ?domain ~loc ?prec ~succ:(index+1) line)
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
              let edge = G_edge.make ?domain ~loc dep_lab in
              (match map_add_edge_opt acc2 gov_id edge dep_id with
                | Some g -> g
                | None -> Error.build "[GRS] [Graph.of_conll] try to build a graph with twice the same edge %s %s"
                  (G_edge.to_string ?domain edge)
                  (Loc.to_string loc)
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
            let fs1 = G_fs.set_feat ?domain "kind" kind G_fs.empty in
            let fs2 = match mwe.Mwe.label with None -> fs1 | Some p -> G_fs.set_feat ?domain "label" p fs1 in
            let fs3 = match mwe.Mwe.mwepos with None -> fs2 | Some p -> G_fs.set_feat ?domain "mwepos" p fs2 in
            let fs4 = match mwe.Mwe.criterion with None -> fs3 | Some c -> G_fs.set_feat ?domain "criterion" c fs3 in

            let new_node =
              (G_node.fresh_unordered ())
              |> G_node.set_fs fs4 in

            (* add a new node *)
            let new_map_1 = (Gid_map.add free_index new_node acc) in
            (* add a link to the first component *)
            let new_map_2 = map_add_edge new_map_1 free_index (G_edge.make ?domain kind) (Id.gbuild mwe.Mwe.first gtable) in
            (* add a link to each other component *)
            let new_map_3 =
              Conll_types.Id_set.fold (
                fun item acc2 ->
                  map_add_edge acc2 free_index (G_edge.make ?domain kind) (Id.gbuild item gtable)
              ) mwe.Mwe.items new_map_2 in

              (* (match map_add_edge_opt acc2 gov_id edge dep_id with
                | Some g -> g
                | None -> Error.build "[GRS] [Graph.of_conll] try to build a graph with twice the same edge %s %s"
                  (G_edge.to_string ?domain edge)
                  (Loc.to_string loc)
              ) *)



            (new_map_3, free_index+1)
          ) conll.Conll.mwes (map_with_edges, List.length sorted_lines) in

    {
      domain;
      meta = conll.Conll.meta;
      map = map_with_nl_nodes;
      fusion;
      highest_index = free_index -1
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
    let get_pos () = incr cpt; !cpt - 1 in

    let leaf_list = ref [] in

    let rec loop nodes = function
    | Ast.Leaf (loc, phon) ->
      let fresh_id = get_pos () in
      let node = G_node.pst_leaf ~loc ?domain phon fresh_id in
      leaf_list := fresh_id :: ! leaf_list;
      (fresh_id, Gid_map.add fresh_id node nodes)

    | Ast.T (loc, cat, daughters) ->
      let fresh_id = get_pos () in
      let new_node = G_node.pst_node ~loc ?domain cat fresh_id in
      let with_mother = Gid_map.add fresh_id new_node nodes in
      let new_nodes = List.fold_left
        (fun map daughter ->
          let (daughter_id, new_map) = loop map daughter in
          map_add_edge new_map fresh_id G_edge.sub daughter_id
        ) with_mother daughters in
      (fresh_id, new_nodes) in

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
      highest_index = !cpt
    }

  (* -------------------------------------------------------------------------------- *)
  let del_edge ?edge_ident loc graph id_src label id_tar =
    let node_src =
      try Gid_map.find id_src graph.map
      with Not_found ->
        match edge_ident with
          | None -> Log.fcritical "[RUN] Some edge refers to a dead node, please report"
          | Some id -> Error.run ~loc "[Graph.del_edge] cannot find source node of edge \"%s\"" id in
    match G_node.remove_opt id_tar label node_src with
    | None -> None
    | Some new_node -> Some {graph with map = Gid_map.add id_src new_node graph.map}

  (* -------------------------------------------------------------------------------- *)
  let del_node graph node_id =
    let map_wo_node =
      Gid_map.fold
        (fun id value acc ->
          if id = node_id
          then acc
          else Gid_map.add id (G_node.remove_key node_id value) acc
        ) graph.map Gid_map.empty in
    try
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
      Some { graph with map = new_map }
    with Not_found -> None

  (* -------------------------------------------------------------------------------- *)
  let insert id1 id2 graph =
    let node1 = Gid_map.find id1 graph.map in
    let node2 = Gid_map.find id2 graph.map in
    let new_pos = match (G_node.get_position node1, G_node.get_position node2) with
    | (G_node.Ordered pos1, G_node.Ordered pos2) -> (pos1 +. pos2) /. 2.
    | _ -> Error.run "Try to insert into non ordered nodes" in
    let new_gid = graph.highest_index + 1 in
    let map = graph.map
      |> (Gid_map.add new_gid (G_node.fresh ~prec:id1 ~succ:id2 new_pos))
      |> (Gid_map.add id1 (G_node.set_succ new_gid node1))
      |> (Gid_map.add id2 (G_node.set_prec new_gid node2)) in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let append id graph =
    let node = Gid_map.find id graph.map in
    let new_pos = match G_node.get_position node with
    | G_node.Ordered pos -> pos +. 1.
    | _ -> Error.run "Try to append into non ordered nodes" in
    let new_gid = graph.highest_index + 1 in
    let map = graph.map
      |> (Gid_map.add new_gid (G_node.fresh ~prec:id new_pos))
      |> (Gid_map.add id (G_node.set_succ new_gid node)) in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let prepend id graph =
    let node = Gid_map.find id graph.map in
    let new_pos = match G_node.get_position node with
    | G_node.Ordered pos -> pos -. 1.
    | _ -> Error.run "Try to prepend into non ordered nodes" in
    let new_gid = graph.highest_index + 1 in
    let map = graph.map
      |> (Gid_map.add new_gid (G_node.fresh ~succ:id new_pos))
      |> (Gid_map.add id (G_node.set_prec new_gid node)) in
    (new_gid, { graph with map; highest_index = new_gid })

  (* -------------------------------------------------------------------------------- *)
  let add_after node_id graph =
    let node = Gid_map.find node_id graph.map in
    match G_node.get_succ node with
    | Some gid_succ -> insert node_id gid_succ graph
    | None -> append node_id graph

  (* -------------------------------------------------------------------------------- *)
  let add_before node_id graph =
    let node = Gid_map.find node_id graph.map in
    match G_node.get_prec node with
    | Some gid_prec -> insert gid_prec node_id graph
    | None -> prepend node_id graph

  (* -------------------------------------------------------------------------------- *)
  let add_unordered graph =
    let new_gid = graph.highest_index + 1 in
    let map = Gid_map.add new_gid (G_node.fresh_unordered ()) graph.map in
    (new_gid, { graph with map; highest_index = new_gid })

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
          | None when !Global.safe_commands -> Error.run ~loc "The [shift_out] command tries to build a duplicate edge (with label \"%s\")" (Label.to_string ?domain edge)
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
                    Error.run ~loc "The [shift_in] command tries to build a duplicate edge (with label \"%s\")" (Label.to_string ?domain edge)
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
        | "position" -> G_node.set_position (float_of_string new_value) node
        | _ ->
          let new_fs = G_fs.set_feat ?loc ?domain feat_name new_value (G_node.get_fs node) in
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
              | G_node.Ordered p -> sprintf "%g" p
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

    let gr_id id = G_node.get_name id (Gid_map.find id graph.map) in

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
        bprintf buff "  %s %s;\n" (gr_id id) (G_node.to_gr node)
      ) sorted_nodes;

    (* edges *)
    List.iter
      (fun (id,node) ->
        Massoc_gid.iter
          (fun tar edge ->
            bprintf buff "  %s -[%s]-> %s;\n" (gr_id id) (G_edge.to_string ?domain edge) (gr_id tar)
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

  let to_sentence ?main_feat ?(deco=G_deco.empty) graph =

    let is_highlighted_gid gid = List.mem_assoc gid deco.nodes in

    let inside fusion_item gid =
      let first = Gid_map.find fusion_item.first graph.map in
      let last = Gid_map.find fusion_item.last graph.map in
      let node = Gid_map.find gid graph.map in
      match (G_node.get_position first, G_node.get_position node, G_node.get_position last) with
      | (Ordered f, Ordered n, Ordered l) when f <=n && n <= l -> true
      | _ -> false in

    let is_highlighted_fusion_item fusion_item =
      List.exists (fun (gid,_) -> inside fusion_item gid) deco.nodes in

    let nodes = Gid_map.fold (fun id elt acc -> (id,elt)::acc) graph.map [] in
    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in

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

    let snodes = List.sort (fun (_,n1) (_,n2) -> G_node.position_comp n1 n2) nodes in

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
        let pos= match G_node.get_position node with G_node.Ordered x -> Some x | _ -> None in
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
            if not (G_edge.is_void ?domain g_edge)
            then
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
  let to_conll graph =
    let domain = get_domain graph in

    (* split lexical // non-lexical nodes *)
    let (nodes, nl_nodes) = Gid_map.fold
      (fun id elt (acc1, acc2) ->
        if is_non_lexical_node elt
        then (acc1, (id,elt)::acc2)
        else ((id,elt)::acc1, acc2)
      ) graph.map ([],[]) in

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
      else G_node.get_float (List.assoc gid snodes) in

    (* Warning: [govs_labs] maps [gid]s to [num]s *)
    let govs_labs =
      List.fold_right
        (fun (src_gid, node) acc ->
          let src_num = get_num src_gid in
          Massoc_gid.fold
            (fun acc2 tar_gid edge  ->
              let old = try Gid_map.find tar_gid acc2 with Not_found -> [] in
              Gid_map.add tar_gid ((sprintf "%g" src_num, G_edge.to_string ?domain edge)::old) acc2
            ) acc (G_node.get_next node)
        ) nodes Gid_map.empty in

    let lines = List_.opt_map
    (fun (gid,node) ->
      if G_node.is_conll_root node
      then None
      else
      let gov_labs = try Gid_map.find gid govs_labs with Not_found -> [] in

      let sorted_gov_labs =
        List.sort
          (fun (g1,l1) (g2,l2) ->
            if l1 <> "" && l1.[0] <> 'I' && l1.[0] <> 'D' && l1.[0] <> 'E'
            then -1
            else if l2 <> "" && l2.[0] <> 'I' && l2.[0] <> 'D' && l2.[0] <> 'E'
            then 1
            else
              match compare (String_.to_float g1) (String_.to_float g2) with
                | 0 -> compare l1 l2
                | x -> x
          ) gov_labs in

    let id_of_gid gid = Conll.Id.of_string (string_of_float (get_num gid)) in
    let fs = G_node.get_fs node in
      Some {
      Conll.line_num = 0;
      id = id_of_gid gid;
      form = (match G_fs.get_string_atom "form" fs with Some p -> p | None -> "_");
      lemma = (match G_fs.get_string_atom "lemma" fs with Some p -> p | None -> "_");
      upos = (match G_fs.get_string_atom "upos" fs with Some p -> p | None -> "_");
      xpos = (match G_fs.get_string_atom "xpos" fs with Some p -> p | None -> "_");
      feats = (G_fs.to_conll ~exclude: ["form"; "lemma"; "upos"; "xpos"; "position"] fs);
      deps = List.map (fun (gov,lab) -> ( Conll.Id.of_string gov, lab)) sorted_gov_labs;
      efs = G_node.get_efs node;
    } ) snodes in

    let snl_nodes = List.sort (fun (gid1,_) (gid2,_) -> Pervasives.compare gid1 gid2) nl_nodes in

    let mwes = List_.foldi_left
      (fun i acc (_,nl_node) ->
        let fs = G_node.get_fs nl_node in
        let kind = match G_fs.get_string_atom "kind" fs with
          | Some "NE" -> Mwe.Ne
          | Some "MWE" -> Mwe.Mwe
          | _ -> Error.run "[G_graph.to_conll] cannot interpreted kind" in
        let nexts = G_node.get_next nl_node in
        let next_list = List.sort Pervasives.compare (Massoc_gid.fold (fun acc2 k _ -> k::acc2) [] nexts) in
        match next_list with
        | [] -> Error.bug "[G_graph.to_conll] mwe node with no next node"
        | head_gid::tail_gids ->
          let head_pos = match G_node.get_position (List.assoc head_gid snodes) with
          | Ordered f -> int_of_float f
          | _ -> Error.run "[G_graph.to_conll] nl_node going to Unordered node" in
          let items = List.fold_left
            (fun acc gid ->
              let pos = match G_node.get_position (List.assoc gid snodes) with
              | Ordered f -> int_of_float f
              | _ -> Error.run "[G_graph.to_conll] nl_node going to Unordered node" in
              Conll_types.Id_set.add (pos,None) acc
            ) Conll_types.Id_set.empty tail_gids in
          let mwe = {
            Mwe.kind;
            Mwe.mwepos = G_fs.get_string_atom "mwepos" fs;
            Mwe.label = G_fs.get_string_atom "label" fs;
            Mwe.criterion = G_fs.get_string_atom "criterion" fs;
            first = (head_pos, None);
            items;
          } in
        Conll_types.Int_map.add (i+1) mwe acc
      ) Conll_types.Int_map.empty snl_nodes in

    {
      Conll.file = None;
      Conll.meta = graph.meta;
      lines;
      multiwords = []; (* multiwords are handled by _UD_* features *)
      mwes;
    }

  let to_conll_string ?cupt graph =
    let conll = to_conll graph in
    Conll.to_string ?cupt (Conll.normalize_multiwords conll)

  (* -------------------------------------------------------------------------------- *)
  let to_dot ?main_feat ?(deco=G_deco.empty) graph =
    let domain = get_domain graph in
    let buff = Buffer.create 32 in

    bprintf buff "digraph G {\n";
    bprintf buff "  node [shape=Mrecord];\n";
    (* bprintf buff "  rankdir=LR;\n"; *)
    (* bprintf buff "  node [shape=none];\n"; *)

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
    edges: ((Gid.t * Label.t * Gid.t) * status) list;
    feats: ((Gid.t * feature_name) * (value option)) list;
  }

  let empty = { del_nodes=[]; edges=[]; feats=[]; }

  let del_node gid t =
    match List_.usort_insert gid t.del_nodes with
    | None -> raise (Inconsistent "del_node")
    | Some new_del_nodes -> {
      del_nodes= new_del_nodes;
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
end (* module Delta *)

(* ================================================================================ *)
module Graph_with_history = struct
  type t = {
    seed: G_graph.t;
    delta: Delta.t;
    graph: G_graph.t;
    added_gids: (string * Gid.t) list;
  }

  let from_graph graph = { graph; seed=graph; delta = Delta.empty; added_gids = [] }

  (* WARNING: compare is correct only on data with the same seed! *)
  let compare t1 t2 = Pervasives.compare (t1.delta,t1.added_gids) (t2.delta, t2.added_gids)
end (* module Graph_with_history*)

(* ================================================================================ *)
module Graph_with_history_set = Set.Make (Graph_with_history)
