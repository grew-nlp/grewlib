(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open CCOption.Infix
open Printf

open Grew_types
open Grew_utils
open Grew_ast
open Grew_edge
open Grew_fs
open Grew_node
open Grew_command
open Grew_graph
open Grew_loader

(* ================================================================================ *)
module Constraint = struct
  type edge_relative_position =
    | Included
    | Contained
    | Disjoint
    | Crossing
  (* let json_of_edge_relative_position = function
    | Included -> `String "Included"
    | Contained -> `String "Contained"
    | Disjoint -> `String "Disjoint"
    | Crossing -> `String "Crossing" *)

  let build_relative_position l1 r1 l2 r2 =
    if r1 <= l2 || r2 <= l1
    then Disjoint
    else if l1 <= l2 && r2 <= r1
    then Contained
    else if l2 <= l1 && r1 <= r2
    then Included
    else Crossing

  let min_max x y = if x < y then (x,y) else (y,x)

  let check_relative_position erp (src1,_,tar1) (src2,_,tar2) graph =
    match (
      G_node.get_position_opt (G_graph.find src1 graph),
      G_node.get_position_opt (G_graph.find tar1 graph),
      G_node.get_position_opt (G_graph.find src2 graph),
      G_node.get_position_opt (G_graph.find tar2 graph)
    ) with
    | (Some pos_src1, Some pos_tar1, Some pos_src2, Some pos_tar2) ->
      let (l1, r1) = min_max pos_src1 pos_tar1 in
      let (l2, r2) = min_max pos_src2 pos_tar2 in
      build_relative_position l1 r1 l2 r2 = erp
    | _ -> false

  type base =
    | Node_id of Pid.t
    | Edge_id of string
    | Lexicon_id of string

  (* let json_of_base = function
    | Node_id pid -> `String (Pid.to_string pid)
    | Edge_id id -> `String id
    | Lexicon_id id -> `String id *)

  type t =
    (*   N -[…]-> *   *)
    | Cst_out of Pid.t * Label_cst.t
    (*   * -[…]-> N   *)
    | Cst_in of Pid.t * Label_cst.t
    (*   N.upos = M.upos   *)
    (*   e1.2 = e2.2   *)
    (*   N.upos = lex.pos   *)
    (*   N.upos <> M.upos   *)
    | Feature_cmp of Cmp.t * base * string * base * string
    (*   N.upos = VERB   *)
    (*   e.2 = comp   *)
    (*   e.2 <> comp   *)
    | Feature_cmp_value of Cmp.t * base * string * Feature_value.t
    (*   N.ExtPos/upos = NOUN *)
    | Feature_else of base * string * string * Feature_value.t
    (*   e.2 = re"…"   *)
    | Feature_cmp_regexp of Cmp.t * base * string * string
    (*   e1.level < e2.level   *)
    | Feature_ineq of Ast.ineq * base * string * base * string
    (*   e1.level < 3   *)
    | Feature_ineq_cst of Ast.ineq * base * string * float
    (*   N [upos=VERB]   *)
    (* ⚠ used only when a extension imposes a fs on a node also defined by the kernel request part *)
    | Filter of Pid.t * P_fs.t
    (*   N << M   *)
    | Node_large_prec of Pid.t * Pid.t
    (*   e1 << e2   *)
    (*   e1 <> e2   *)
    (*   e1 >< e2   *)
    | Edge_relative of edge_relative_position * string * string
    (*   N << e2   *)
    | Covered of Pid.t * string (* node_id, edge_id *)

  let to_json ~config p_graph_list const =
    let pid_name pid = P_graph.get_name pid p_graph_list in
    let base_to_string = function
    | Node_id pid -> pid_name pid
    | Edge_id id -> id
    | Lexicon_id id -> id in
  
    match const with 
    | Cst_out (pid, label_cst) -> sprintf "%s -[%s]-> *" (pid_name pid) (Label_cst.to_string ~config label_cst)
    | Cst_in (pid, label_cst) -> sprintf "* -[%s]-> %s" (Label_cst.to_string ~config label_cst) (pid_name pid)
    | Feature_cmp (cmp,id1,fn1,id2,fn2) -> sprintf "%s.%s %s %s.%s" (base_to_string id1) fn1 (Cmp.to_string cmp) (base_to_string id2) fn2
    | Feature_cmp_value (cmp,id,fn,value) -> sprintf "%s.%s %s %s" (base_to_string id) fn (Cmp.to_string cmp) (Feature_value.to_string ~quote:true value)
    | Feature_else (id, fn1, fn2, value) -> sprintf "%s.%s/%s %s" (base_to_string id) fn1 fn2 (Feature_value.to_string ~quote:true value)
    | Feature_cmp_regexp (cmp,id,fn,regexp) -> sprintf "%s.%s %s re\"%s\"" (base_to_string id) fn (Cmp.to_string cmp) regexp
    | Feature_ineq (_,id1,fn1,id2,fn2) -> sprintf "%s.%s < %s.%s" (base_to_string id1) fn1 (base_to_string id2) fn2
    | Feature_ineq_cst (_,id,fn,f) -> sprintf "%s.%s  %g" (base_to_string id) fn f
    | Filter (pid, p_fs) -> sprintf "%s [%s]" (pid_name pid) (P_fs.to_string p_fs)
    | Node_large_prec (pid1, pid2) ->  sprintf "%s << %s" (pid_name pid1) (pid_name pid2)
    | Covered (pid1, eid2) -> sprintf "%s << %s" (pid_name pid1) eid2
    | Edge_relative (Disjoint, eid1, eid2) -> sprintf "%s <> %s" eid1 eid2
    | Edge_relative (Crossing, eid1, eid2) -> sprintf "%s >< %s" eid1 eid2
    | Edge_relative (Included, eid1, eid2) ->  sprintf "%s << %s" eid1 eid2
    | Edge_relative (Contained, _, _) -> Error.bug "Unexpected Edge_relative"

  let build ~config lexicons ker_table ext_table edge_ids const =
    let parse_id loc id = match (Id.build_opt id ker_table, Id.build_opt id ext_table) with
      | (Some pid,_) -> Node_id (Pid.Ker pid)
      | (None, Some pid) -> Node_id (Pid.Ext pid)
      | (None, None) when List.mem id edge_ids -> Edge_id id
      | (None, None) when List.mem_assoc id lexicons -> Lexicon_id id
      | _ -> Error.build ~loc "[Constraint.build] Identifier '%s' not found" id in

    let pid_of_name loc node_name =
      match Id.build_opt node_name ker_table with
      | Some i -> Pid.Ker i
      | None -> Pid.Ext (Id.build ~loc node_name ext_table) in

      match const with
    | (Ast.Cst_out (id,label_cst), loc) ->
      Cst_out (pid_of_name loc id, Label_cst.of_ast ~loc ~config label_cst)
    | (Ast.Cst_in (id,label_cst), loc) ->
      Cst_in (pid_of_name loc id, Label_cst.of_ast ~loc ~config label_cst)

    | (Ast.Feature_cmp (cmp, (id1, feat_name1),(id2, feat_name2)), loc)  ->
      Feature_cmp (cmp, parse_id loc id1, feat_name1, parse_id loc id2, feat_name2)

    | (Ast.Feature_ineq (ineq, (id1, feat_name1), (id2, feat_name2)), loc) ->
      Feature_ineq (ineq, parse_id loc id1, feat_name1, parse_id loc id2, feat_name2)

    | (Ast.Feature_ineq_cst (ineq, (id1, feat_name1), constant), loc) ->
      Feature_ineq_cst (ineq, parse_id loc id1, feat_name1, constant)

    | (Ast.Feature_cmp_regexp (cmp, (id, feat_name), regexp), loc) ->
      Feature_cmp_regexp (cmp, parse_id loc id, feat_name, regexp)

    | (Ast.Feature_cmp_value (cmp, (id, feat_name), value), loc) ->
      Feature_cmp_value (cmp, parse_id loc id, feat_name, value)

    | (Ast.Feature_else ((id, feat_name1), feat_name2, value), loc) ->
      Feature_else (parse_id loc id, feat_name1, feat_name2, value)

    | (Ast.Large_prec (id1, id2), loc) ->
      begin
        match (parse_id loc id1, parse_id loc id2) with
        | (Edge_id eid1, Edge_id eid2) -> Edge_relative (Included, eid1, eid2)
        | (Node_id pid1, Node_id pid2) -> Node_large_prec (pid1, pid2)
        | (Node_id pid1, Edge_id eid2) -> Covered (pid1, eid2)
        | _ -> Error.build "Operator << cannot be used with \"edge << node\""
      end

    | (Ast.Edge_disjoint (eid1, eid2), _) ->
      Edge_relative (Disjoint, eid1, eid2)
    | (Ast.Edge_crossing (eid1, eid2), _) ->
      Edge_relative (Crossing, eid1, eid2)

end (* module Constraint *)

(* ================================================================================ *)
module Request = struct

  type basic = {
    graph: P_graph.t;
    constraints: Constraint.t list;
  }

  let basic_to_json ~config ?base basic =
    let bases = match base with Some g -> [g ; basic.graph] | None -> [basic.graph] in
    `List (
      (P_graph.to_json_list ~config ?base basic.graph)
      @ 
      (List.map 
        (fun x -> `String (Constraint.to_json ~config bases x))
        basic.constraints
      )
    )

  let build_ker_basic ~config lexicons basic_ast =
    let (graph, ker_table, edge_ids) = P_graph.of_ast ~config lexicons basic_ast in
    (
      {
        graph = graph;
        constraints = List.map (Constraint.build ~config lexicons ker_table [||] edge_ids) basic_ast.Ast.req_const
      },
      ker_table,
      edge_ids
    )

  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_ext_basic ~config lexicons ker_table edge_ids basic_ast =
    let (graph, filter_map, ext_table, edge_ids) =
      P_graph.of_ast_extension ~config lexicons ker_table edge_ids basic_ast.Ast.req_nodes basic_ast.Ast.req_edges in

    let filters = Pid_map.fold (fun id p_fs acc -> Constraint.Filter (id, p_fs) :: acc) filter_map [] in
    {
      graph;
      constraints = filters @ List.map (Constraint.build ~config lexicons ker_table ext_table edge_ids) basic_ast.Ast.req_const ;
    }

  let get_edge_ids basic =
    Pid_map.fold
      (fun _ node acc ->
         Pid_massoc.fold
           (fun acc2 _ edge -> match P_edge.get_id_opt edge with Some id -> id::acc2 | None -> acc2)
           acc (P_node.get_next node)
      ) basic.graph []

  (* a [request] is described by the kernel basic and a list of extention basics. *)
  type t = {
    global: Ast.glob list;
    ker: basic;
    exts: (basic * bool) list; (* with iff true and without iff false *)
    table: Id.table;  (* needed to build whether *)
    edge_ids: string list;  (* needed to build whether *)
  }

  let to_json ~config t =
    let without_list = 
      List.map 
      (function
        | (basic,false) -> `Assoc [("without", basic_to_json ~config ~base:t.ker.graph basic)]
        | (basic,true) -> `Assoc [("with", basic_to_json ~config ~base:t.ker.graph basic)]
    ) t.exts in
    let pattern = `Assoc [("pattern", basic_to_json ~config t.ker)] in
    match t.global with
    | [] -> `List (pattern :: without_list )
    | l -> let global = `Assoc [("global", `List (List.map (fun glob -> `String (Ast.glob_to_string glob)) l))] in
      `List (global:: pattern :: without_list )

  let pid_name_list request = P_graph.pid_name_list request.ker.graph

  let of_ast ~config ?(lexicons=[]) request_ast =
    let (ker, ker_table, edge_ids) =
      try build_ker_basic ~config lexicons request_ast.Ast.req_pos
      with P_fs.Fail_unif -> Error.build "feature structures declared in the `pattern` clauses are inconsistent " in
    let exts =
      List_.try_map
        P_fs.Fail_unif (* Skip the without parts that are incompatible with the match part *)
        (fun (basic_ast, flag) -> 
          (build_ext_basic ~config lexicons ker_table edge_ids basic_ast, flag)
        )
        request_ast.Ast.req_exts in
    { ker; exts; global=request_ast.req_glob; table=ker_table; edge_ids; }

  let build_whether ~config request basic_ast =
    build_ext_basic ~config [] request.table request.edge_ids (Ast.complete_basic basic_ast)

  (* New type with a "real" whether type *)
  type cluster_item =
    | Key of string
    | Whether of basic
  
  (* Note that we need to know the request in order to build the baisc type *)
  let parse_cluster_item ~config request s =
    let clean_s = CCString.trim s in
    if clean_s.[0] = '{'
    then Whether (build_whether ~config request (Parser.basic clean_s))
    else Key clean_s
  
  let string_of_json request = 
    let open Yojson.Basic.Util in
    try
      request 
      |> to_list 
      |> List.map 
        (fun item -> 
          item |> to_assoc |> 
          (function
          | [keyword, l] -> sprintf "  %s {%s}" keyword (l |> to_list |> List.map to_string |> String.concat ";\n")
          | _ -> Error.build "[Request.string_of_json]"
          )
        )  
      |> String.concat "\n" 
    with Type_error _ -> 
      Error.build "[Request.string_of_json]"

  let of_json ~config request = 
    let ast = Parser.request (string_of_json request) in
    of_ast ~config ast
end (* module Request *)

(* ================================================================================ *)
module Matching = struct

  (* ====================================================================== *)
  type t = {
    n_match: Gid.t Pid_map.t;                      (* partial fct: request nodes |--> graph nodes *)
    e_match: (Gid.t*G_edge.t*Gid.t) String_map.t;  (* edge matching: edge ident  |--> (src,label,tar) *)
    l_param: Lexicons.t;                           (* *)
  }

  let empty ?(lexicons=[]) () =
    { n_match = Pid_map.empty; e_match = String_map.empty; l_param = lexicons;}

  let intern s = String_.re_match (Str.regexp "__.*__") s

  let to_json ?(all_edges=false) request graph m =
    let node_name gid = G_node.get_name gid (G_graph.find gid graph) in
    let nodes = Pid_map.fold (fun pid gid acc ->
        let pnode = P_graph.find pid request.Request.ker.graph in
        (P_node.get_name pnode, `String (node_name gid))::acc
      ) m.n_match [] in
    let edges = String_map.fold (fun id (src,lab,tar) acc ->
        if all_edges || not (intern id)
        then (id, `Assoc [
            ("source", `String (node_name src));
            ("label", G_edge.to_json lab);
            ("target", `String (node_name tar));
          ]) :: acc
        else acc
      ) m.e_match [] in
    `Assoc [
      ("nodes", `Assoc nodes);
      ("edges", `Assoc edges)
    ]

  let node_matching request graph { n_match; _ } =
    Pid_map.fold
      (fun pid gid acc ->
         let pnode = P_graph.find pid request.Request.ker.graph in
         let gnode = G_graph.find gid graph in
         (P_node.get_name pnode, G_node.get_name gid gnode) :: acc
      ) n_match []

  let e_match_add edge_id new_edge matching =
    if String_map.mem edge_id matching.e_match
    then Error.run "The edge identifier '%s' is binded twice in the same request" edge_id
    else { matching with e_match = String_map.add edge_id new_edge matching.e_match }

  let build_deco request matching =
    { G_deco.nodes =
        Pid_map.fold
          (fun pid gid acc ->
             let pnode = P_graph.find pid request.Request.ker.graph in
             let request_feat_list = P_fs.feat_list (P_node.get_fs pnode) in
             (gid, (P_node.get_name pnode, request_feat_list)) :: acc
          ) matching.n_match [];
      G_deco.edges = String_map.fold (fun _ edge acc -> edge::acc) matching.e_match [];
    }

  let find cnode ?loc (matching, created_nodes) =
    match cnode with
    | Command.Req pid ->
      (try Pid_map.find pid matching.n_match
       with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name ->
      (try List.assoc name created_nodes
       with Not_found -> Error.run ?loc "Identifier '%s' not found" name)

  let down_deco (edge_mapping,matching,created_nodes) commands =
    let feat_to_highlight = List.fold_left
        (fun acc -> function
           | (Command.UPDATE_FEAT (tar_cn,feat_name,_),_) ->
             let gid = find tar_cn (matching, created_nodes) in
             let old_feat_list = try Gid_map.find gid acc with Not_found -> [] in
             Gid_map.add gid (feat_name :: old_feat_list) acc
           | _ -> acc
        ) Gid_map.empty commands in

    {
      G_deco.nodes =
        List.map
          (fun (gid,feat_list) ->
             (gid, ("", (List.map (fun x -> (x,None)) feat_list)))
          ) (Gid_map.bindings feat_to_highlight);
      G_deco.edges =
        List.fold_left
          (fun acc -> function
             | (Command.ADD_EDGE (src_cn,tar_cn,edge),_) ->
               (find src_cn (matching, created_nodes), edge, find tar_cn (matching, created_nodes)) :: acc
             | (Command.UPDATE_EDGE_FEAT (edge_id,_,_), _) ->
               begin
                 match String_map.find_opt edge_id edge_mapping with
                 | None -> acc (* the edge may have been deleted after being modified... {e.2=y; del_edge e; } *)
                 | Some edge -> edge :: acc
               end
             | _ -> acc
          ) [] commands;
    }

  exception Fail
  type partial = {
    sub: t;
    unmatched_nodes: Pid.t list;
    unmatched_edges: (Pid.t * P_edge.t * Pid.t) list;
    already_matched_gids: Gid.t list; (* to ensure injectivity *)
    check: Constraint.t list (* constraints to verify at the end of the matching *)
  }

  (*  ---------------------------------------------------------------------- *)
  let init ?lexicons basic =
    let roots = P_graph.roots basic.Request.graph in

    let node_list = Pid_map.fold (fun pid _ acc -> pid::acc) basic.graph [] in

    (* put all roots in the front of the list to speed up the algo *)
    let sorted_node_list =
      List.sort
        (fun n1 n2 -> match (List.mem n1 roots, List.mem n2 roots) with
           | true, false -> -1
           | false, true -> 1
           | _ -> 0) node_list in
    {
      sub = empty ?lexicons ();
      unmatched_nodes = sorted_node_list;
      unmatched_edges = [];
      already_matched_gids = [];
      check = basic.constraints;
    }

  type out =
    | Value of Feature_value.t
    | Lex of (string * string)

  (*  ---------------------------------------------------------------------- *)
  let apply_cst ~config graph matching cst : t =
    let get_value base feat_name = match base with
      | Constraint.Node_id pid ->
        let gid = Pid_map.find pid matching.n_match in
        if feat_name = "__id__"
        then Value (Float (float_of_int gid))
        else
          let node = G_graph.find gid graph in
          begin
            match G_fs.get_value_opt feat_name (G_node.get_fs node) with
            | Some f -> Value f
            | None -> raise Fail (* no such feat_name here *)
          end
      | Edge_id edge_id ->
        let (_,g_edge,_) as e = String_map.find edge_id matching.e_match in
        begin
          match feat_name with
          | "label" -> (match G_edge.to_string_opt ~config g_edge with Some s -> Value (String s) | None -> raise Fail)
          | "length" -> (match G_graph.edge_length_opt e graph with Some s -> Value (Float (float_of_int s)) | None -> raise Fail)
          | "delta" -> (match G_graph.edge_delta_opt e graph with Some s -> Value (Float (float_of_int s)) | None -> raise Fail)
          | _ ->
            match G_edge.get_sub_opt feat_name g_edge with
            | None -> raise Fail
            | Some s -> Value s
        end
      | Lexicon_id id -> Lex (id, feat_name) in

    match cst with
    | Constraint.Cst_out (pid,label_cst) ->
      let gid = Pid_map.find pid matching.n_match in
      if G_graph.edge_out ~config graph gid label_cst
      then matching
      else raise Fail
    | Cst_in (pid,label_cst) ->
      let gid = Pid_map.find pid matching.n_match in
      if G_graph.node_exists
          (fun node ->
             List.exists (fun e -> Label_cst.match_ ~config label_cst e) (Gid_massoc.assoc gid (G_node.get_next node))
          ) graph
      then matching
      else raise Fail
    | Filter (pid, fs) ->
      begin
        try
          let gid = Pid_map.find pid matching.n_match in
          let gnode = G_graph.find gid graph in
          let new_param = P_fs.match_ ~lexicons:(matching.l_param) fs (G_node.get_fs gnode) in
          {matching with l_param = new_param }
        with P_fs.Fail -> raise Fail
      end
    | Feature_cmp (cmp, base1, feat_name1, base2, feat_name2) ->
      begin
        match (get_value base1 feat_name1, get_value base2 feat_name2) with
        | (Value v1, Value v2) -> if Cmp.fct cmp v1 v2 then matching else raise Fail
        | (Value v, Lex (lexicon,field))
        | (Lex (lexicon,field), Value v) ->
          let old_lex = List.assoc lexicon matching.l_param in
          begin
            match Lexicon.filter_opt cmp field (Feature_value.to_string v) old_lex with
            | None -> raise Fail
            | Some new_lex -> {matching with l_param = (lexicon, new_lex) :: (List.remove_assoc lexicon matching.l_param) }
          end
        | _ -> Error.run "[Matching.apply_cst] cannot compare two lexicon fields"
      end
    | Feature_cmp_value (cmp, id1, feat_name1, value) ->
      begin
        match get_value id1 feat_name1 with
        | Value fv when Cmp.fct cmp fv value -> matching
        | Lex (lexicon,field) ->
          let old_lex = List.assoc lexicon matching.l_param in
          begin
            match Lexicon.filter_opt cmp field (Feature_value.to_string value) old_lex with
            | None -> raise Fail
            | Some new_lex -> {matching with l_param = (lexicon, new_lex) :: (List.remove_assoc lexicon matching.l_param) }
          end
        | _ -> raise Fail
      end

    | Feature_else (id1, feat_name1, feat_name2, value) ->
      begin
        let value_in_graph = 
          try
            match get_value id1 feat_name1 with
           | Value fv -> fv 
           | _ -> raise Fail
          with Fail ->
            match get_value id1 feat_name2 with
            | Value fv -> fv
           | _ -> raise Fail in
          if value_in_graph = value then matching else raise Fail
      end

    | Feature_ineq (ineq, base1, feat_name1, base2, feat_name2) ->
      begin
        match (get_value base1 feat_name1, get_value base2 feat_name2) with
        | (Value (Float v1), Value (Float v2)) -> if Ast.check_ineq v1 ineq v2 then matching else raise Fail
        | (_, _) ->
          Error.run "[Matching.apply_cst] Cannot check inequality on feature values %s and %s (available only on numeric values)"
            feat_name1 feat_name2
      end
    | Feature_ineq_cst (ineq, base, feat_name, constant) ->
      begin
        match (get_value base feat_name) with
        | Value (Float f) -> if Ast.check_ineq f ineq constant then matching else raise Fail
        | _ -> Error.run "[Matching.apply_cst] Cannot check inequality on feature value %s (available only on numeric values)" feat_name
      end

    | Feature_cmp_regexp (cmp, id, feat_name, regexp) ->
      begin
        match get_value id feat_name with
        | Lex _ -> Error.run "[Matching.apply_cst] test regexp against lexicon is not available"
        | Value (Float _) -> Error.run "[Matching.apply_cst] test regexp against numeric value is not available"
        | Value (String string_feat) ->
          let re = Str.regexp regexp in
          match (cmp, String_.re_match re string_feat) with
          | (Eq, true) | (Neq, false) -> matching
          | _ -> raise Fail
      end

    | Node_large_prec (pid1, pid2) ->
      let gnode1 = G_graph.find (Pid_map.find pid1 matching.n_match) graph in
      let gnode2 = G_graph.find (Pid_map.find pid2 matching.n_match) graph in
      begin
        match (G_node.get_position_opt gnode1, G_node.get_position_opt gnode2) with
        | Some i1, Some i2 when i1 < i2 -> matching
        | _ -> raise Fail
      end

    | Edge_relative (erp, eid1, eid2) ->
      begin
        match (String_map.find_opt eid1 matching.e_match, String_map.find_opt eid2 matching.e_match) with
        | (Some e1, Some e2) when Constraint.check_relative_position erp e1 e2 graph -> matching
        | (Some _, Some _) -> raise Fail
        | (None, _) -> Error.run "Edge identifier '%s' not found" eid1
        | (_, None) -> Error.run "Edge identifier '%s' not found" eid2
      end

    | Covered (pid, eid) ->
      begin
        let gnode = G_graph.find (Pid_map.find pid matching.n_match) graph in
        match (String_map.find_opt eid matching.e_match) with
        | (Some edge) when G_graph.covered gnode edge graph -> matching
        | Some _ -> raise Fail
        | (None) -> Error.run "Edge identifier '%s' not found" eid;

      end

  (*  ---------------------------------------------------------------------- *)
  (* returns all extension of the partial input matching *)
  let rec extend_matching ~config ((ker_graph,ext_graph) : P_graph.t * P_graph.t ) (graph:G_graph.t) (partial:partial) =
    match (partial.unmatched_edges, partial.unmatched_nodes) with
    | [], [] ->
      begin
        try
          let new_matching =
            List.fold_left
              (fun acc const ->
                 apply_cst ~config graph acc const
              ) partial.sub partial.check in
          [new_matching, partial.already_matched_gids]
        with Fail -> []
      end
    | (src_pid, p_edge, tar_pid)::tail_ue, _ ->
      begin
        try (* is the tar already found in the matching ? *)
          let new_partials =
            let src_gid = Pid_map.find src_pid partial.sub.n_match in
            let tar_gid = Pid_map.find tar_pid partial.sub.n_match in
            let src_gnode = G_graph.find src_gid graph in
            let g_edges = Gid_massoc.assoc tar_gid (G_node.get_next src_gnode) in

            match P_edge.match_list ~config p_edge g_edges with
            | P_edge.Fail -> (* no good edge in graph for this request edge -> stop here *)
              []
            | P_edge.Pass -> [ {partial with unmatched_edges = tail_ue } ]
            | P_edge.Binds (id,labels) -> (* n edges in the graph match the identified p_edge -> make copies of the [k] matchings (and returns n*k matchings) *)
              List.map
                (fun label ->
                   {partial with sub = e_match_add id (src_gid,label,tar_gid) partial.sub; unmatched_edges = tail_ue }
                ) labels
          in CCList.flat_map (extend_matching ~config (ker_graph,ext_graph) graph) new_partials
        with Not_found -> (* p_edge goes to an unmatched node *)
          let candidates = (* candidates (of type (gid, matching)) for m(tar_pid) = gid) with new partial matching m *)
            let (src_gid : Gid.t) = Pid_map.find src_pid partial.sub.n_match in
            let src_gnode = G_graph.find src_gid graph in
            Gid_massoc.fold
              (fun acc gid_next g_edge ->
                 match P_edge.match_ ~config p_edge g_edge with
                 | P_edge.Fail -> (* g_edge does not fit, no new candidate *)
                   acc
                 | P_edge.Pass -> (gid_next, partial.sub) :: acc
                 | P_edge.Binds (id,[label]) -> (* g_edge fits with an extended matching *)
                   (gid_next, e_match_add id (src_gid, label, gid_next) partial.sub) :: acc
                 | _ -> Error.bug "P_edge.match_ must return exactly one label"
              ) [] (G_node.get_next src_gnode) in
          CCList.flat_map
            (fun (gid_next, matching) ->
               extend_matching_from ~config (ker_graph,ext_graph) graph tar_pid gid_next
                 {partial with sub=matching; unmatched_edges = tail_ue}
            ) candidates
      end
    | [], pid :: _ ->
      G_graph.fold_gid
        (fun gid acc ->
           (extend_matching_from ~config (ker_graph,ext_graph) graph pid gid partial) @ acc
        ) graph []

  (*  ---------------------------------------------------------------------- *)
  and extend_matching_from ~config (ker_graph,ext_graph) (graph:G_graph.t) pid (gid : Gid.t) partial =
    let injective_pid = P_graph.is_injective pid [ker_graph; ext_graph] in
    if injective_pid && List.mem gid partial.already_matched_gids
    then [] (* the required association pid -> gid is not injective *)
    else
      let p_node =
        try P_graph.find pid ker_graph
        with Not_found ->
        try P_graph.find pid ext_graph
        with Not_found -> Error.bug "[Grew_rule.extend_matching_from] cannot find node" in

      let g_node = try G_graph.find gid graph with Not_found -> Error.bug "[extend_matching_from] cannot find gid in graph" in

      try
        let new_lex_set = P_node.match_ ~lexicons:partial.sub.l_param p_node g_node in
        (* add all out-edges from pid in request *)
        let new_unmatched_edges =
          Pid_massoc.fold
            (fun acc pid_next p_edge -> (pid, p_edge, pid_next) :: acc
            ) partial.unmatched_edges (P_node.get_next p_node) in

        let new_partial =
          { partial with
            unmatched_nodes = (try List_.remove pid partial.unmatched_nodes with Not_found -> Error.bug "[extend_matching_from] cannot find pid in unmatched_nodes");
            unmatched_edges = new_unmatched_edges;
            already_matched_gids = if injective_pid then gid :: partial.already_matched_gids else partial.already_matched_gids;
            sub = {partial.sub with n_match = Pid_map.add pid gid partial.sub.n_match; l_param = new_lex_set};
          } in
        extend_matching ~config (ker_graph,ext_graph) graph new_partial
      with P_fs.Fail -> []

  (*  [test_locality matching created_nodes gid] checks if [gid] is a "local" node:
      either it belongs to the codomain of [matching] or it is one of the [created_nodes] *)
  let test_locality matching created_nodes gid =
    (Pid_map.exists (fun _ id -> id=gid) matching.n_match) || (List.exists (fun (_,id) -> id=gid) created_nodes)

  (* test_extension returns [true] iff the matching (sub, already_matched_gids) of [ker_graph] into [graph] can be extended with [ext_graph] *)
  let test_extension ~config ker graph ext (sub, already_matched_gids) =
    let unmatched_nodes =
      Pid_map.fold
        (fun pid _ acc -> match pid with Pid.Ext _ -> pid::acc | _ -> acc)
        ext.Request.graph [] in
    let unmatched_edges =
      Pid_map.fold
        (fun pid node acc ->
           match pid with
           | Pid.Ext _ -> acc
           | Pid.Ker _ ->
             Pid_massoc.fold
               (fun acc2 pid_next p_edge -> (pid, p_edge, pid_next) :: acc2)
               acc (P_node.get_next node)
        ) ext.Request.graph [] in
    let new_partial_matching =
      {
        sub;
        unmatched_nodes = unmatched_nodes;
        unmatched_edges = unmatched_edges;
        already_matched_gids;
        check = ext.constraints;
      } in
    match extend_matching ~config (ker.Request.graph, ext.graph) graph new_partial_matching with
    | [] -> false (* fail to extend the matching to ext *)
    | _ -> true

  let whether ~config (extension: Request.basic) request graph matching =
    let already_matched_gids =
      Pid_map.fold 
      (fun pid gid acc -> 
        if P_graph.is_injective pid [request.Request.ker.graph; extension.graph]
        then gid::acc
        else acc
      ) matching.n_match [] in
    test_extension ~config request.Request.ker graph extension (matching, already_matched_gids)

  (* returns true iff the graph verify all structure constraints give in the list *)
  let test_structure_constraints graph = function
    | [] -> true
    | ["is_projective"] -> G_graph.is_projective graph
    | ["is_not_projective"] -> not (G_graph.is_projective graph)
    | l ->
      let dfs = G_graph.depth_first_search graph in
      let rec loop = function
        | [] -> true
        | "is_projective" :: tail ->
          begin
            match G_graph.is_projective graph with
            | false -> false
            | true -> loop tail
          end
        | "is_not_projective" :: tail ->
          begin
            match G_graph.is_projective graph with
            | false -> loop tail
            | true -> false
          end

        | "is_tree" :: tail when dfs.tree -> loop tail
        | "is_tree" :: _ -> false

        | "is_not_tree" :: tail when not dfs.tree -> loop tail
        | "is_not_tree" :: _ -> false

        | "is_forest" :: tail when dfs.forest -> loop tail
        | "is_forest" :: _ -> false

        | "is_not_forest" :: tail when not dfs.forest -> loop tail
        | "is_not_forest" :: _ -> false

        | "is_cyclic" :: tail when dfs.cyclic -> loop tail
        | "is_cyclic" :: _ -> false

        | "is_not_cyclic" :: tail when not dfs.cyclic -> loop tail
        | "is_not_cyclic" :: _ -> false

        | x :: _ -> Error.build "Unknown global requirement \"%s\"" x in
      loop l


  let check_global_constraint glob_list graph =
    let stuct_cst_list = List.fold_left (fun acc (glob,_) -> match glob with Ast.Glob_cst s -> s::acc | _ -> acc) [] glob_list in
    if test_structure_constraints graph stuct_cst_list
    then
      List.for_all
        (function
          | (Ast.Glob_cst _, _) -> true (* these constraints are alredy checked in [test_stucture_constraints] function *)
          | (Ast.Glob_eq_list (key, value_list), _) ->
            begin
              match G_graph.get_meta_opt key graph with
              | Some v -> List.mem v value_list
              | None -> false
            end
          | (Ast.Glob_diff_list (key, value_list), _) ->
            begin
              match G_graph.get_meta_opt key graph with
              | Some v -> not (List.mem v value_list)
              | None -> false
            end
          | (Ast.Glob_absent key, _) ->
            begin
              match G_graph.get_meta_opt key graph with
              | Some _ -> false
              | None -> true
            end
          | (Ast.Glob_regexp (key, re), _) ->
            begin
              match G_graph.get_meta_opt key graph with
              | Some v -> String_.re_match (Str.regexp re) v
              | None -> false
            end
        ) glob_list
    else false

  (*  ---------------------------------------------------------------------- *)
  let search_request_in_graph ~config ?lexicons { Request.global; ker; exts; _ } graph =

    if not (check_global_constraint global graph)
    then []
    else
      (* get the list of partial matching for kernel part of the request *)
      let matching_list =
        extend_matching
          ~config
          (ker.graph,P_graph.empty)
          graph
          (init ?lexicons ker) in

      let filtered_matching_list =
        List.filter
          (fun (sub, already_matched_gids) ->
             List.for_all
               (fun (ext,polarity) ->
                  test_extension ~config ker graph ext (sub, already_matched_gids) = polarity
               ) exts
          ) matching_list in
      List.map fst filtered_matching_list

  let subgraph graph matching depth =
    let gid_list = Pid_map.fold (fun _ gid acc -> gid :: acc) matching.n_match [] in  
  G_graph.subgraph graph gid_list depth







  let search_pid_name request graph matching pid_name =
    let exception Found of Pid.t in
    try
      Pid_map.iter
        (fun pid _ ->
          if P_node.get_name (P_graph.find pid request.Request.ker.graph) = pid_name
          then raise (Found pid)
        ) matching.n_match;
      Error.run "The identifier `%s` is not declared in the positive part of the request" pid_name
    with Found pid -> 
      let gid = Pid_map.find pid matching.n_match in
      let g_node = G_graph.find gid graph in
      (gid, g_node)

  let get_relative_order pid_name_list request graph matching =
    let pid_name_position_list =
      CCList.filter_map
        (fun pid_name ->
          let (_,node) = search_pid_name request graph matching pid_name in
          match G_node.get_position_opt node with
          | None -> None
          | Some pos -> Some (pid_name, pos)
        ) pid_name_list
    |> (List.sort (fun (_,p1) (_,p2) -> Stdlib.compare p1 p2)) in
    match pid_name_position_list with 
    | [] -> None
    | l -> Some (l |> List.map fst |> String.concat " << ")

  let get_link ~config rev pid_name_1 pid_name_2 request graph matching =
    let (gid_1, node_1) = search_pid_name request graph matching pid_name_1 in
    let (gid_2, node_2) = search_pid_name request graph matching pid_name_2 in
    match 
    (
      node_1 |> G_node.get_next |> (Gid_massoc.assoc gid_2) |> (List.filter G_edge.is_fs),
      node_2 |> G_node.get_next |> (Gid_massoc.assoc gid_1) |> (List.filter G_edge.is_fs)
    ) with 
    | ([], []) -> "__none__"
    | ([], _) when not rev -> "__none__"
    | ([direct], []) -> G_edge.to_string_opt ~config direct |> CCOption.get_exn_or "BUG Matching.get_link"
    | ([direct], _) when not rev -> G_edge.to_string_opt ~config direct |> CCOption.get_exn_or "BUG Matching.get_link"
    | ([], [reverse]) -> G_edge.to_string_opt ~config reverse |> CCOption.get_exn_or "BUG Matching.get_link" |> sprintf "-%s" 
    | _ -> "__multi__"

  (* [fold_until fct list] Apply [fct] to each element of the [list]
     until the result is Some v and output Some v
     None it return if [fct] return None of each element of the list *)
  let rec fold_until fct = function
  | [] -> None
  | x::t ->
    match fct x with
    | None -> fold_until fct t
    | v -> v

  let get_feat_value_opt ?(json_label=false) ~config request graph matching (node_or_edge_id, splitted_feature_names) =
    match (String_map.find_opt node_or_edge_id matching.e_match, splitted_feature_names) with
    | (Some (_,edge,_), ["label"]) when json_label ->
      begin
        match G_edge.to_json_opt edge with
        | Some s -> Some (Yojson.Basic.to_string s)
        | None -> Error.bug "[Matching.get_feat_value_opt#1] internal edge %s" (G_edge.dump ~config edge)
      end
    | (Some (_,edge,_), ["label"]) ->
      begin
        match G_edge.to_string_opt ~config edge with
        | Some s -> Some s
        | None -> Error.bug "[Matching.get_feat_value_opt#2] internal edge %s" (G_edge.dump ~config edge)
      end
    | (Some edge, ["length"]) -> string_of_int <$> (G_graph.edge_length_opt edge graph)
    | (Some edge, ["delta"]) -> string_of_int <$> (G_graph.edge_delta_opt edge graph)
    | (Some (_,edge,_), _) ->
      let feat_value_opt = fold_until (fun fn -> G_edge.get_sub_opt fn edge) splitted_feature_names in
      Feature_value.to_string <$> feat_value_opt
    | (None, ["__out__"]) ->
      let (_, node) = search_pid_name request graph matching node_or_edge_id in
      Some (string_of_int (G_node.out_edges node))
    | (None, _) ->
      let (_, node) = search_pid_name request graph matching node_or_edge_id in
      let fs = G_node.get_fs node in
      let feat_value_opt = fold_until (fun fn -> G_fs.get_value_opt fn fs) splitted_feature_names in
      Feature_value.to_string <$> feat_value_opt

  let get_interval request graph matching ((pid_name,feature_name), gap, min_opt, max_opt) =
    let (_, node) = search_pid_name request graph matching pid_name in
    match G_fs.get_value_opt feature_name (G_node.get_fs node) with
    | None
    | Some String _ -> Error.run "[Matching.get_value_opt] feature name `%s` is not a numeric value" feature_name
    | Some Float f ->
      match (min_opt, max_opt) with
      | (Some m, _) when f < m -> sprintf "]-∞, %g[" m
      | (_, Some m) when f >= m -> sprintf "[%g, +∞[" m
      | (Some m, Some ma) -> 
        let i = floor ((f -. m) /. gap) in
        sprintf "[%g, %g[" (m +. i *. gap) (min (m +. (i +. 1.) *. gap) ma)
      | (Some m, None) -> 
        let i = floor ((f -. m) /. gap) in
        sprintf "[%g, %g[" (m +. i *. gap) (m +. (i +. 1.) *. gap)
      | (None,Some ma) -> 
        let i = ceil ((ma -. f) /. gap) in
        sprintf "[%g, %g[" (ma -. i *. gap) (ma -. (i -. 1.) *. gap)
      | (None,_) -> 
        let i = floor (f /. gap) in
        sprintf "[%g, %g[" (i *. gap) ((i +. 1.) *. gap)

  type key = 
    | Rel_order of string list
    | Sym_rel of (string * string)
    | Rel of (string * string)
    | Feat of (string * string list)
    | Continuous of ((string * string) * float * float option * float option)

  let parse_key string_key =
    if CCString.contains string_key '#'
    then Rel_order (string_key |> (Str.split (Str.regexp "#")) |> List.map String.trim)
    else 
      match Str.full_split (Str.regexp "<?->") string_key with 
      | [Str.Text n1; Str.Delim "<->"; Str.Text n2] -> Sym_rel (String.trim n1, String.trim n2)
      | [Str.Text n1; Str.Delim "->"; Str.Text n2] -> Rel (String.trim n1, String.trim n2)
      | _ -> 
        match Str.full_split (Str.regexp "\\[\\|\\]") string_key with 
        
        | [Str.Text feat; Str.Delim "["; Str.Text params; Str.Delim "]" ] ->
          let fs = params
          |> Str.split (Str.regexp " *, *")
          |> List.map (fun param_item -> match Str.split (Str.regexp " *= *") param_item with [f;v] -> (f,v) | _ -> Error.run "Cannot parse cluster key %s" string_key) in
          (
            match (List.assoc_opt "gap" fs, List.assoc_opt "min" fs, List.assoc_opt "max" fs) with
            | (None, _,_) -> Error.run "Missing gap"
            | (Some gap, min_opt, max_opt) ->
              match Str.split (Str.regexp "\\.") (String.trim feat) with
              | [id; fn] -> Continuous ((id, fn), float_of_string gap, float_of_string <$> min_opt, float_of_string <$> max_opt)
              | _ -> Error.run "Cannot parse cluster key %s" string_key
          )
        | [Str.Text k] ->
          begin 
            match Str.split (Str.regexp "\\.") k with
            | [id; feature_name] -> Feat (id, Str.split (Str.regexp "/") feature_name)
            | _ -> Error.run "Cannot parse cluster key %s" string_key
          end
        | _ -> Error.run "Cannot parse cluster key %s" string_key

  let get_value_opt ?(json_label=false) ~config string_key request graph matching =
    match parse_key string_key with
    | Rel_order pid_name_list -> get_relative_order pid_name_list request graph matching
    | Sym_rel (pid_name_1, pid_name_2) -> Some (get_link ~config true pid_name_1 pid_name_2 request graph matching)
    | Rel (pid_name_1, pid_name_2) -> Some (get_link ~config false pid_name_1 pid_name_2 request graph matching)
    | Feat (id, splitted_feature_names) -> get_feat_value_opt ~json_label ~config request graph matching (id, splitted_feature_names)
    | Continuous params -> Some (get_interval request graph matching params)

  let get_clust_value_opt ?(json_label=false) ~config cluster_item request graph matching =
    match cluster_item with
    | Request.Key key -> get_value_opt ~json_label ~config key request graph matching
    | Whether basic -> if whether ~config basic request graph matching then Some "Yes" else Some "No"

end (* module Matching *)

(* ================================================================================ *)
module Rule = struct

  type t = {
    name: string;
    request: Request.t;
    commands: Command.t list;
    lexicons: Lexicons.t;
    loc: Loc.t;
    path: string;
    incr: bool;
  }

  (* the number of rewriting steps is bounded to stop rewriting when the system is not terminating *)
  let max_rules = ref 10_000
  let set_max_rules n = max_rules := n

  let nb_rule_report = 10

  let current_rules = ref 0
  let rule_report_list = ref []

  let max_incr_rules = ref 100
  let current_incr_rules = ref 0

  let reset_rules () = current_rules := 0; rule_report_list := []; current_incr_rules := 0

  let incr_rules rule =
    if rule.incr then incr current_incr_rules;
    if !current_incr_rules > !max_incr_rules
    then Error.run "More than %d rewriting steps which increases graph size: check for loops!" !max_incr_rules;
    incr current_rules;
    if !current_rules > !max_rules - nb_rule_report
    then
      if !current_rules > !max_rules
      then Error.run "More than %d rewriting steps: check for loops or increase max_rules value. Last rules are: […%s]"
          !max_rules
          (String.concat ", " (List.rev !rule_report_list))
      else rule_report_list := rule.name :: !rule_report_list

  let get_nb_rules () = !current_rules

  let get_name t = t.name

  let get_rule_info t =
    (t.path ^ t.name,
     match Loc.get_line_opt t.loc with Some l -> l | None -> 0)

  let get_loc t = t.loc

  let to_json ~config t =
    `Assoc (
      [
        ("request", Request.to_json ~config t.request);
        ("commands", 
        `List (
          List.map 
            (Command.to_json ~config ~base:t.request.ker.graph) t.commands))
      ]
    )

  (* ====================================================================== *)
  let to_dep ~config t =
    let ker_basic = t.request.ker in
    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { scale = 200; }\n";

    let nodes =
      Pid_map.fold
        (fun id node acc ->
           (node, sprintf "  N_%s { word=\"%s\"; subword=\"%s\"}"
              (Pid.to_id id) (P_node.get_name node) (P_fs.to_dep (P_node.get_fs node))
           )
           :: acc
        ) ker_basic.graph [] in

    (* nodes are sorted to appear in the same order in dep picture and in input file *)
    let sorted_nodes = List.sort (fun (n1,_) (n2,_) -> P_node.compare_pos n1 n2) nodes in

    bprintf buff "[WORDS] {\n";
    List.iter
      (fun (_, dep_line) -> bprintf buff "%s\n" dep_line
      ) sorted_nodes;

    List.iteri
      (fun i cst ->
         match cst with
         | Constraint.Cst_out _ | Cst_in _ -> bprintf buff "  C_%d { word=\"*\"}\n" i
         | _ -> ()
      ) ker_basic.constraints;
    bprintf buff "}\n";

    bprintf buff "[EDGES] {\n";

    Pid_map.iter
      (fun src_pid node ->
         Pid_massoc.iter
           (fun tar_pid edge ->
              bprintf buff "  N_%s -> N_%s { label=\"%s\"}\n"
                (Pid.to_id src_pid)
                (Pid.to_id tar_pid)
                (P_edge.to_string ~config edge)
           )
           (P_node.get_next node)
      ) ker_basic.graph;

    List.iteri
      (fun i cst ->
         match cst with
         | Constraint.Cst_out (pid, label_cst) ->
           bprintf buff "  N_%s -> C_%d {label = \"%s\"; style=dot; bottom; color=green;}\n"
             (Pid.to_id pid) i (Label_cst.to_string ~config label_cst)
         | Cst_in (pid, label_cst) ->
           bprintf buff "  C_%d -> N_%s {label = \"%s\"; style=dot; bottom; color=green;}\n"
             i (Pid.to_id pid) (Label_cst.to_string ~config label_cst)
         | _ -> ()
      ) ker_basic.constraints;
    bprintf buff "}\n";
    Buffer.contents buff

  (* ====================================================================== *)
  let commands_of_ast ~config lexicons ker ker_table ast_commands =
    let known_node_ids = Array.to_list ker_table in
    let known_edge_ids = Request.get_edge_ids ker in

    let rec loop (kni,kei) = function
      | [] -> []
      | ast_command :: tail ->
        let (command, (new_kni, new_kei)) =
          Command.of_ast
            ~config
            lexicons
            (kni,kei)
            ker_table
            ast_command in
        command :: (loop (new_kni,new_kei) tail) in
    loop (known_node_ids, known_edge_ids) ast_commands

  (* ====================================================================== *)
  let of_ast ~config rule_ast =
    let lexicons =
      List.fold_left (fun acc (name,lex) ->
          try
            let prev = List.assoc name acc in
            (name, (Lexicon.union prev (Lexicon.of_ast ~loc:rule_ast.Ast.rule_loc rule_ast.Ast.rule_dir lex))) :: (List.remove_assoc name acc)
          with Not_found -> (name, Lexicon.of_ast ~loc:rule_ast.Ast.rule_loc rule_ast.Ast.rule_dir lex) :: acc
        ) [] rule_ast.Ast.lexicon_info in

    let (ker, ker_table, edge_ids) =
      try Request.build_ker_basic ~config lexicons rule_ast.Ast.request.Ast.req_pos
      with P_fs.Fail_unif ->
        Error.build ~loc:rule_ast.Ast.rule_loc
          "[Rule.build] in rule \"%s\": feature structures declared in the `pattern` clauses are inconsistent"
          rule_ast.Ast.rule_id in
    let (exts,_) =
      List.fold_left
        (fun (acc,position) (basic_ast, flag) ->
           try ((Request.build_ext_basic ~config lexicons ker_table edge_ids basic_ast, flag) :: acc, position+1)
           with P_fs.Fail_unif ->
             Error.warning ~loc:rule_ast.Ast.rule_loc "In rule \"%s\", the wihtout number %d cannot be satisfied, it is skipped"
               rule_ast.Ast.rule_id position;
             (acc, position+1)
        ) ([],1) rule_ast.Ast.request.Ast.req_exts in
    let commands = commands_of_ast ~config lexicons ker ker_table rule_ast.Ast.commands in
    {
      name = rule_ast.Ast.rule_id;
      request = { ker; exts; global=rule_ast.Ast.request.Ast.req_glob; table=ker_table; edge_ids };
      commands;
      loc = rule_ast.Ast.rule_loc;
      lexicons;
      path = rule_ast.Ast.rule_path;
      incr = List.exists (fun c -> Command.is_increasing c) commands;
    }

  (*  ---------------------------------------------------------------------- *)
  let string_of_json request commands =
    let open Yojson.Basic.Util in
    let request_string =  Request.string_of_json request in 
    let commands_string = try
      sprintf "\n  commands {%s}" (commands |> to_list |> List.map to_string |> String.concat ";\n") 
    with Type_error _ -> 
      Error.build "[Rule.string_of_json]" in
    request_string ^ commands_string

  (*  ---------------------------------------------------------------------- *)
  let onf_find cnode ?loc (matching, created_nodes) = (* TODO review args order and remove pair *)
    match cnode with
    | Command.Req pid ->
      (try Pid_map.find pid matching.Matching.n_match
       with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name ->
      (try List.assoc name created_nodes
       with Not_found -> Error.run ?loc "Identifier '%s' not found" name)

  (*  ---------------------------------------------------------------------- *)
  type oac_state = {
    graph: G_graph.t;
    effective: bool;
    created_nodes: (string * Gid.t) list;
    e_mapping: (Gid.t * G_edge.t * Gid.t) String_map.t;
  }

  let onf_apply_command ~config matching (command,loc) state =
    let node_find cnode = onf_find ~loc cnode (matching, state.created_nodes) in

    let feature_value_of_item feat_name = function
      | (Command.String_item s, range) -> 
        Feature_value.extract_range ~loc range (Feature_value.parse ~loc feat_name s)
      | (Command.Node_feat (cnode, feat_name), range) ->
        let gid = node_find cnode in
        let node = G_graph.find gid state.graph in
        let fs = G_node.get_fs node in
        begin
          match G_fs.get_value_opt feat_name fs with
          | None -> Error.run ~loc "Node feature named `%s` is undefined" feat_name
          | Some v -> Feature_value.extract_range ~loc range v
        end
      | (Command.Edge_feat (edge_id, feat_name), range) ->
        begin
          match String_map.find_opt edge_id state.e_mapping with
          | None -> Error.bug "Cannot find edge_id %s" edge_id
          | Some (_,edge,_) ->
            if feat_name = "label"
            then
              begin
                match G_edge.to_string_opt ~config edge with
                | Some s -> Feature_value.extract_range ~loc range (String s)
                | None -> Error.run "Cannot use not regular edge label as a concat item"
              end
            else
              match G_edge.get_sub_opt feat_name edge with
              | None -> Error.run ~loc "[onf_apply_command] Edge feature named %s is undefined" feat_name
              | Some fv -> Feature_value.extract_range ~loc range fv
        end
      | (Command.Lexical_field (lex_id, field), range) ->
        begin
          match List.assoc_opt lex_id matching.l_param with
          | None -> Error.run ~loc "Undefined lexicon %s" lex_id
          | Some lexicon ->
            match Lexicon.get_opt field lexicon with
            | None -> Error.bug "Inconsistent lexicon lex_id=%s field=%s" lex_id field
            | Some value -> Feature_value.extract_range ~loc range (Feature_value.parse ~loc feat_name value)
        end in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid state.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE: the edge '%s' already exists" (G_edge.dump ~config edge)
        | None -> state
        | Some new_graph -> {state with graph = new_graph; effective = true}
      end

    | Command.ADD_EDGE_EXPL (src_cn,tar_cn,edge_id) ->
      if String_map.mem edge_id state.e_mapping (* e_mapping contains both pattern edge and previous commande edges *)
      then Error.run ~loc "ADD_EDGE_EXPL: the edge name '%s' already used. Semantic of this command has changed, see [[https://grew.fr/old]]" edge_id
      else
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        begin
          match G_graph.add_edge_opt src_gid G_edge.empty tar_gid state.graph with
          | None when !Global.safe_commands -> Error.run ~loc "ADD_EDGE_EXPL: there is already empty edge here"
          | None -> state
          | Some new_graph ->
            {state with
             graph = new_graph;
             e_mapping = (String_map.add edge_id (src_gid,G_edge.empty,tar_gid) state.e_mapping);
             effective = true
            }
        end

    | Command.ADD_EDGE_ITEMS (src_cn,tar_cn,items) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let direct_items =
        List.map
          (fun (name, value) ->
             match Str.bounded_split (Str.regexp_string ".") value 2
             with
             | [edge_id; feat_name] ->
               begin
                 match String_map.find_opt edge_id state.e_mapping with
                 | None -> Error.run ~loc "ADD_EDGE_ITEMS: undefined edge '%s'" edge_id
                 | Some (_,matched_edge,_) ->
                   match G_edge.get_sub_opt feat_name matched_edge with
                   | Some new_value -> (name, new_value)
                   | None -> Error.run ~loc "ADD_EDGE_ITEMS: no items edge feature name '%s' in matched edge '%s'" feat_name edge_id
               end
             | _ -> (name, Feature_value.parse ~loc name value)
          ) items in
      let edge = G_edge.from_items direct_items in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid state.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE_ITEMS: the edge '%s' already exists" (G_edge.dump ~config edge)
        | None -> state
        | Some new_graph -> {state with graph = new_graph; effective = true}
      end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      (match G_graph.del_edge_opt ~loc src_gid edge tar_gid state.graph with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_EDGE_EXPL: the edge '%s' does not exist" (G_edge.dump ~config edge)
       | None -> state
       | Some new_graph -> {state with graph = new_graph; effective = true}
      )

    | Command.DEL_EDGE_NAME edge_ident ->
      let (src_gid,edge,tar_gid) =
        try String_map.find edge_ident state.e_mapping
        with Not_found -> Error.run ~loc "DEL_EDGE_NAME: The edge identifier '%s' is undefined" edge_ident in
      (match G_graph.del_edge_opt ~loc src_gid edge tar_gid state.graph with
        | None when !Global.safe_commands -> Error.run ~loc "DEL_EDGE_NAME: the edge '%s' does not exist" edge_ident
        | None -> state
        | Some new_graph ->
          { state with
            graph = new_graph;
            effective = true;
            e_mapping = String_map.remove edge_ident state.e_mapping;
          }
      )

    | Command.DEL_EDGE_FEAT (edge_id, feat_name) ->
      begin
        match String_map.find_opt edge_id state.e_mapping with
        | None -> Error.run ~loc "DEL_EDGE_FEAT: The edge identifier '%s' is undefined" edge_id
        | Some (src_gid,old_edge,tar_gid) ->
          begin
            match G_graph.del_edge_feature_opt ~loc edge_id feat_name (src_gid,old_edge,tar_gid) state.graph with
            | None when !Global.safe_commands -> Error.run ~loc "DEL_EDGE_FEAT: the edge feature name '%s' does not exist" feat_name
            | None -> state
            | Some (new_graph, new_edge, _) ->
              {state with
               graph = new_graph;
               effective = true;
               e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) state.e_mapping;
              }
          end
      end

    | Command.UPDATE_EDGE_FEAT (edge_id, feat_name, item_list) ->
      begin
        match String_map.find_opt edge_id state.e_mapping with
        | None -> Error.run ~loc "UPDATE_EDGE_FEAT (LHS) The edge identifier '%s' is undefined" edge_id
        | Some (src_gid,old_edge,tar_gid) ->
          let new_edge =
            match (feat_name, item_list) with
            | ("label", [Command.Edge_feat (src_edge_id, "label"), (None,None)]) -> (* special case of label copy "e.label = f.label" *)
              begin
                match String_map.find_opt src_edge_id state.e_mapping with
                | None -> Error.run ~loc "UPDATE_EDGE_FEAT (RHS) The edge identifier '%s' is undefined" src_edge_id
                | Some (_,edge,_) -> edge
              end
            | _ ->
              let feature_value_list = List.map (feature_value_of_item feat_name) item_list in
              let new_feature_value = Feature_value.concat ~loc feature_value_list in
              match (feat_name, new_feature_value) with
              | ("label", Feature_value.String s) -> G_edge.from_string ~config s
              | ("label", Float _) -> Error.run "Cannot set a edge feature label as numeric"
              | _ -> G_edge.update feat_name new_feature_value old_edge in

          let new_state_opt =
            if new_edge = old_edge
            then None
            else
              match G_graph.del_edge_opt ~loc src_gid old_edge tar_gid state.graph with
              | None -> Error.bug "Inconsistent graph structure in UPDATE_EDGE_FEAT"
              | Some tmp_graph ->
                match G_graph.add_edge_opt src_gid new_edge tar_gid tmp_graph with
                | None -> None
                | Some new_graph -> Some (new_graph, new_edge) in
          begin
            match new_state_opt with
            | None when !Global.safe_commands -> Error.run ~loc "UPDATE_EDGE_FEAT: no changes %s" edge_id
            | None -> state
            | Some (new_graph, new_edge) ->
              { state with
                graph = new_graph;
                effective = true;
                e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) state.e_mapping;
              }
          end
      end

    | Command.DEL_NODE node_cn ->
      let node_gid = node_find node_cn in
      (match G_graph.del_node_opt node_gid state.graph with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_NODE: the node does not exist"
       | None -> state
       | Some new_graph ->
         { state with
           graph = new_graph;
           effective = true;
           e_mapping = String_map.filter (fun _ (s,_,t) -> s<>node_gid && t<>node_gid) state.e_mapping;
         }
      )

    | Command.UPDATE_FEAT (tar_cn, tar_feat_name, item_list) ->
      let tar_gid = node_find tar_cn in
      let feature_value_list = List.map (feature_value_of_item tar_feat_name) item_list in
      let new_feature_value = Feature_value.concat ~loc feature_value_list in
      let new_graph = G_graph.update_feat state.graph tar_gid tar_feat_name new_feature_value in
      {state with graph = new_graph; effective = true}

    | Command.CONCAT_FEATS (side, src_cn, tar_cn, regexp, separator) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      (match G_graph.concat_feats_opt state.graph side src_gid tar_gid separator regexp with
       | None when !Global.safe_commands -> Error.run ~loc "CONCAT_FEATS uneffective"
       | None -> state
       | Some (new_graph,_) -> {state with graph = new_graph; effective = true}
      )

    | Command.UNORDER (node_cn) ->
      let node_gid = node_find node_cn in
      (match G_graph.unorder_opt node_gid state.graph with
       | None when !Global.safe_commands -> Error.run ~loc "UNORDER: the node is not ordered"
       | None -> state
       | Some new_graph ->
         { state with
           graph = new_graph;
           effective = true;
         }
      )

    | Command.INSERT_BEFORE (inserted_cn, site_cn) ->
      let inserted_gid = node_find inserted_cn in
      let site_gid = node_find site_cn in
      let new_graph = G_graph.insert_before inserted_gid site_gid state.graph in
      { state with
        graph=new_graph;
        effective = true;
      }

    | Command.INSERT_AFTER (inserted_cn, site_cn) ->
      let inserted_gid = node_find inserted_cn in
      let site_gid = node_find site_cn in
      let new_graph = G_graph.insert_after inserted_gid site_gid state.graph in
      { state with
        graph=new_graph;
        effective = true;
      }

    | Command.DEL_FEAT (tar_cn,feat_name) ->
      let tar_gid = node_find tar_cn in
      (match G_graph.del_feat_opt state.graph tar_gid feat_name with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_FEAT the feat does not exist"
       | None -> state
       | Some new_graph -> {state with graph = new_graph; effective = true}
      )
    (* TODO: an update feat is always considered as effective! is it OK? *)

    | Command.SHIFT_IN (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, de, ae) = G_graph.shift_in ~config loc src_gid tar_gid (Matching.test_locality matching state.created_nodes) label_cst state.graph in
      {state with graph = new_graph; effective = state.effective || de <> [] || ae <> []}

    | Command.SHIFT_OUT (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, de, ae) = G_graph.shift_out ~config loc src_gid tar_gid (Matching.test_locality matching state.created_nodes) label_cst state.graph in
      {state with graph = new_graph; effective = state.effective || de <> [] || ae <> []}

    | Command.SHIFT_EDGE (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, de, ae) = G_graph.shift_edges ~config loc src_gid tar_gid (Matching.test_locality matching state.created_nodes) label_cst state.graph in
      {state with graph = new_graph; effective = state.effective || de <> [] || ae <> []}

    | Command.NEW_AFTER (created_name,base_cn) ->
      let base_gid = node_find base_cn in
      let (new_gid,new_graph) = G_graph.add_after base_gid state.graph in
      { state with
        graph=new_graph;
        created_nodes = (created_name,new_gid) :: state.created_nodes;
        effective = true;
      }

    | Command.NEW_BEFORE (created_name,base_cn) ->
      let base_gid = node_find base_cn in
      let (new_gid,new_graph) = G_graph.add_before base_gid state.graph in
      { state with
        graph=new_graph;
        created_nodes = (created_name,new_gid) :: state.created_nodes;
        effective = true;
      }

    | Command.NEW_NODE (created_name) ->
      let (new_gid,new_graph) = G_graph.add_unordered state.graph in
      { state with
        graph=new_graph;
        created_nodes = (created_name,new_gid) :: state.created_nodes;
        effective = true;
      }


  let onf_apply_opt ~config rule graph =
    try
      let { Request.global; Request.ker; exts; _} = rule.request in
      match Matching.check_global_constraint global graph with
      | false -> None
      | true ->
        (* get the list of partial matching for kernel part of the request *)
        let matching_list =
          Matching.extend_matching
            ~config
            (ker.graph,P_graph.empty)
            graph
            (Matching.init ~lexicons:rule.lexicons ker) in
       match List.find_opt
                (fun (sub, already_matched_gids) ->
                   List.for_all
                     (fun (ext, polarity) ->
                        Matching.test_extension ~config ker graph ext (sub, already_matched_gids) = polarity
                     ) exts
               ) matching_list with
       | None -> None
       | Some (first_matching_where_all_witout_are_fulfilled,_) ->
          let final_state =
            List.fold_left
              (fun state command -> onf_apply_command ~config first_matching_where_all_witout_are_fulfilled command state)
              { graph;
                created_nodes = [];
                effective = false;
                e_mapping = first_matching_where_all_witout_are_fulfilled.e_match;
              }
             rule.commands in
          if final_state.effective
          then
            begin
              Timeout.check ();
              incr_rules rule;
              let up = Matching.build_deco rule.request first_matching_where_all_witout_are_fulfilled in
              let down = Matching.down_deco (final_state.e_mapping,first_matching_where_all_witout_are_fulfilled, final_state.created_nodes) rule.commands in
              Some (G_graph.track up (get_rule_info rule) down graph final_state.graph)
            end
         else None
    with Error.Run (msg,_) -> Error.run ~loc:rule.loc "%s" msg

  let find cnode ?loc gwh matching =
    match cnode with
    | Command.Req pid ->
      (try Pid_map.find pid matching.Matching.n_match
       with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name -> List.assoc name gwh.Graph_with_history.added_gids

  let gwh_apply_command ~config (command,loc) matching gwh =
    let node_find cnode = find ~loc cnode gwh matching in

    let feature_value_list_of_item feat_name = function
      | (Command.String_item s, range) -> [Feature_value.extract_range ~loc range (Feature_value.parse ~loc feat_name s)]
      | (Command.Node_feat (cnode, feat_name), range) ->
        let gid = node_find cnode in
        let node = G_graph.find gid gwh.graph in
        let fs = G_node.get_fs node in
        begin
          match G_fs.get_value_opt feat_name fs with
          | None -> Error.run ~loc "Node feature named `%s` is undefined" feat_name
          | Some v -> [Feature_value.extract_range ~loc range v]
        end
      | (Command.Edge_feat (edge_id, feat_name), range) ->
        begin
          let (_,edge,_) =
            match String_map.find_opt edge_id gwh.e_mapping with
            | Some e -> e
            | None -> Error.run ~loc "The edge identifier '%s' is undefined" edge_id in
          match G_edge.get_sub_opt feat_name edge with
          | None -> Error.run ~loc "[gwh_apply_command] Edge feature named %s is undefined" feat_name
          | Some fv -> [Feature_value.extract_range ~loc range fv]
        end
      | (Command.Lexical_field (lex_id, field), range) ->
        begin
          match List.assoc_opt lex_id matching.l_param with
          | None -> Error.run ~loc "Undefined lexicon %s" lex_id
          | Some lexicon -> List.map (fun x -> Feature_value.extract_range ~loc range (Feature_value.parse ~loc feat_name x)) (Lexicon.read_all field lexicon)
        end in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid gwh.Graph_with_history.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE: the edge '%s' already exists" (G_edge.dump ~config edge)
        | None -> Graph_with_history_set.singleton gwh
        | Some new_graph ->
          Graph_with_history_set.singleton
            {gwh with
             Graph_with_history.graph = new_graph;
             delta = Delta.add_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
            }
      end

    | Command.ADD_EDGE_EXPL (src_cn,tar_cn,edge_ident) ->
      if String_map.mem edge_ident matching.e_match
      then Error.run ~loc "ADD_EDGE_EXPL: the edge name '%s' already used. Semantic of this command has changed, see [[https://grew.fr/old]]" edge_ident
      else
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        begin
          match G_graph.add_edge_opt src_gid G_edge.empty tar_gid gwh.Graph_with_history.graph with
          | None when !Global.safe_commands -> Error.run ~loc "ADD_EDGE_EXPL: there is already empty edge here"
          | None -> Graph_with_history_set.singleton gwh
          | Some new_graph ->
            Graph_with_history_set.singleton
              {gwh with
               Graph_with_history.graph = new_graph;
               delta = Delta.add_edge src_gid G_edge.empty tar_gid gwh.Graph_with_history.delta;
               e_mapping = String_map.add edge_ident (src_gid,G_edge.empty,tar_gid) gwh.e_mapping;
              }
        end

    | Command.ADD_EDGE_ITEMS (src_cn,tar_cn,items) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let direct_items = List.map (fun (name, value) ->
          match Str.bounded_split (Str.regexp_string ".") value 2
          with
          | [edge_id; feat_name] ->
            begin
              match String_map.find_opt edge_id gwh.e_mapping with
              | None -> Error.run ~loc "ADD_EDGE_ITEMS: undefined edge '%s'" edge_id
              | Some (_,matched_edge,_) ->
                match G_edge.get_sub_opt feat_name matched_edge with
                | Some new_value -> (name, new_value)
                | None -> Error.run ~loc "ADD_EDGE_ITEMS: no items edge feature name '%s' in matched edge '%s'" feat_name edge_id
            end
          | _ -> (name, Feature_value.parse ~loc name value)
        ) items in
      let edge = G_edge.from_items direct_items in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid gwh.Graph_with_history.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE_ITEMS: the edge '%s' already exists" (G_edge.dump ~config edge)
        | None -> Graph_with_history_set.singleton gwh
        | Some new_graph -> Graph_with_history_set.singleton
                              {gwh with
                               Graph_with_history.graph = new_graph;
                               delta = Delta.add_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
                              }
      end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      (match G_graph.del_edge_opt ~loc src_gid edge tar_gid gwh.Graph_with_history.graph with
       | None when !Global.safe_commands ->
         Error.run ~loc "DEL_EDGE_EXPL: the edge '%s' does not exist" (G_edge.dump ~config edge)
       | None -> Graph_with_history_set.singleton gwh
       | Some new_graph -> Graph_with_history_set.singleton
                             {gwh with
                              Graph_with_history.graph = new_graph;
                              delta = Delta.del_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
                             })

    | Command.DEL_EDGE_NAME edge_ident ->
      let (src_gid,edge,tar_gid) =
        try String_map.find edge_ident gwh.e_mapping
        with Not_found -> Error.run ~loc "The edge identifier '%s' is undefined" edge_ident in
      (match G_graph.del_edge_opt ~loc src_gid edge tar_gid gwh.Graph_with_history.graph with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_EDGE_NAME: the edge '%s' does not exist" edge_ident
       | None -> Graph_with_history_set.singleton gwh
       | Some new_graph -> Graph_with_history_set.singleton
                             {gwh with
                              Graph_with_history.graph = new_graph;
                              delta = Delta.del_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
                              e_mapping = String_map.remove edge_ident gwh.e_mapping;
                             })

    | Command.DEL_NODE node_cn ->
      let node_gid = node_find node_cn in
      (match G_graph.del_node_opt node_gid gwh.Graph_with_history.graph with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_NODE: the node does not exist"
       | None -> Graph_with_history_set.singleton gwh
       | Some new_graph -> Graph_with_history_set.singleton
                             { gwh with
                               Graph_with_history.graph = new_graph;
                               delta = Delta.del_node node_gid gwh.Graph_with_history.delta;
                               e_mapping = String_map.filter (fun _ (s,_,t) -> s<>node_gid && t<>node_gid) gwh.e_mapping;
                             }
      )




    | Command.UPDATE_FEAT (tar_cn, tar_feat_name, item_list) ->
      let tar_gid = node_find tar_cn in

      (* not deterministic because of non functional lexicons *)
      let new_feature_value_list_list =
        item_list
        |> List.map (feature_value_list_of_item tar_feat_name)
        |> CCList.cartesian_product in

      let new_feature_value_list = List.map Feature_value.concat new_feature_value_list_list in

      let new_graphs = List.fold_left
          (fun acc new_feature_value ->
             let new_graph = G_graph.update_feat gwh.Graph_with_history.graph tar_gid tar_feat_name new_feature_value in
             Graph_with_history_set.add
               { gwh with
                 Graph_with_history.graph = new_graph;
                 delta = Delta.set_feat gwh.Graph_with_history.seed tar_gid tar_feat_name (Some new_feature_value) gwh.Graph_with_history.delta;
               }
               acc
          ) Graph_with_history_set.empty new_feature_value_list in

      if (Graph_with_history_set.is_empty new_graphs) && !Global.safe_commands
      then Error.run ~loc "UPDATE_FEAT: no changes"
      else new_graphs


    | Command.DEL_FEAT (tar_cn,feat_name) ->
      let tar_gid = node_find tar_cn in
      (match G_graph.del_feat_opt gwh.Graph_with_history.graph tar_gid feat_name with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_FEAT: the feat does not exist"
       | None -> Graph_with_history_set.singleton gwh
       | Some new_graph ->
         Graph_with_history_set.singleton
           { gwh with
             Graph_with_history.graph = new_graph;
             delta = Delta.set_feat gwh.Graph_with_history.seed tar_gid feat_name None gwh.Graph_with_history.delta;
           }
      )




    | Command.UPDATE_EDGE_FEAT (edge_id, feat_name, item_list) ->
      begin
        let (src_gid,old_edge,tar_gid) =
          match String_map.find_opt edge_id gwh.e_mapping with
          | Some e -> e
          | None -> Error.run ~loc "The edge identifier '%s' is undefined" edge_id in

        let new_edges =
          match (feat_name, item_list) with
          | ("label", [Command.Edge_feat (src_edge_id, "label"), (None,None)]) -> (* special case of label copy "e.label = f.label" *)

            let src_edge =
              match String_map.find_opt src_edge_id gwh.e_mapping with
              | Some (_,e,_) -> e
              | None -> Error.run ~loc "The edge identifier '%s' is undefined" edge_id in
            [src_edge]

          | _ ->
            (* not deterministic because of non functional lexicons *)
            let new_feature_value_list_list =
              item_list
              |> List.map (feature_value_list_of_item feat_name)
              |> CCList.cartesian_product in

            let new_feature_value_list = List.map Feature_value.concat new_feature_value_list_list in
            List.fold_left
              (fun acc new_feature_value ->
                 let test_new_edge =
                   match (feat_name, new_feature_value) with
                   | ("label", Feature_value.String s) -> G_edge.from_string ~config s
                   | ("label", Float _) -> Error.run "Cannot set a edge feature label as numeric"
                   | _ -> G_edge.update feat_name new_feature_value old_edge in
                 if test_new_edge = old_edge
                 then acc
                 else test_new_edge :: acc
              ) [] new_feature_value_list in

        begin
          match new_edges with
          | [] when !Global.safe_commands -> Error.run ~loc "UPDATE_EDGE_FEAT: no changes"
          | [] -> Graph_with_history_set.singleton gwh
          | _ ->
            let new_graphs =
              List.fold_left
                (fun acc new_edge ->
                   match G_graph.del_edge_opt ~loc src_gid old_edge tar_gid gwh.Graph_with_history.graph with
                   | None -> Error.bug "Inconsistent graph structure in UPDATE_EDGE_FEAT"
                   | Some tmp_graph ->
                     match G_graph.add_edge_opt src_gid new_edge tar_gid tmp_graph with
                     | None when !Global.safe_commands -> Error.run ~loc "UPDATE_EDGE_FEAT: the new edge alredy exists"
                     | None -> (* Case of edge capture: TODO doc + warn *)
                       Graph_with_history_set.add
                         { gwh with
                           Graph_with_history.graph = tmp_graph;
                           delta = Delta.del_edge src_gid old_edge tar_gid gwh.Graph_with_history.delta;
                           e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) gwh.e_mapping;
                         }
                         acc
                     | Some new_graph ->
                       Graph_with_history_set.add
                         { gwh with
                           Graph_with_history.graph = new_graph;
                           delta = gwh.Graph_with_history.delta
                                   |> Delta.del_edge src_gid old_edge tar_gid
                                   |> Delta.add_edge src_gid new_edge tar_gid;
                           e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) gwh.e_mapping;
                         }
                         acc
                ) Graph_with_history_set.empty new_edges in
            new_graphs
        end
      end
    | Command.SHIFT_IN (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, del_edges, add_edges) =
        G_graph.shift_in ~config loc src_gid tar_gid (Matching.test_locality matching gwh.added_gids_in_rule) label_cst gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton { gwh with
                                         Graph_with_history.graph = new_graph;
                                         delta = gwh.Graph_with_history.delta
                                                 |> (List.fold_right (fun (s,e,t) -> Delta.del_edge s e t) del_edges)
                                                 |> (List.fold_right (fun (s,e,t) -> Delta.add_edge s e t) add_edges)
                                       }

    | Command.SHIFT_OUT (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, del_edges, add_edges) =
        G_graph.shift_out ~config loc src_gid tar_gid (Matching.test_locality matching gwh.added_gids_in_rule) label_cst gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton { gwh with
                                         Graph_with_history.graph = new_graph;
                                         delta = gwh.Graph_with_history.delta
                                                 |> (List.fold_right (fun (s,e,t) -> Delta.del_edge s e t) del_edges)
                                                 |> (List.fold_right (fun (s,e,t) -> Delta.add_edge s e t) add_edges)
                                       }

    | Command.SHIFT_EDGE (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, del_edges, add_edges) =
        G_graph.shift_edges ~config loc src_gid tar_gid (Matching.test_locality matching gwh.added_gids_in_rule) label_cst gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton { gwh with
                                         Graph_with_history.graph = new_graph;
                                         delta = gwh.Graph_with_history.delta
                                                 |> (List.fold_right (fun (s,e,t) -> Delta.del_edge s e t) del_edges)
                                                 |> (List.fold_right (fun (s,e,t) -> Delta.add_edge s e t) add_edges)
                                       }

    | Command.NEW_AFTER (created_name,base_cn) ->
      let base_gid = node_find base_cn in
      let (new_gid,new_graph) = G_graph.add_after base_gid gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton { gwh with
                                         Graph_with_history.graph = new_graph;
                                         added_gids = (created_name, new_gid) :: gwh.Graph_with_history.added_gids;
                                         added_gids_in_rule = (created_name,new_gid) :: gwh.added_gids_in_rule;
                                       }

    | Command.NEW_BEFORE (created_name,base_cn) ->
      let base_gid = node_find base_cn in
      let (new_gid,new_graph) = G_graph.add_before base_gid gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton { gwh with
                                         Graph_with_history.graph = new_graph;
                                         added_gids = (created_name, new_gid) :: gwh.Graph_with_history.added_gids;
                                         added_gids_in_rule = (created_name,new_gid) :: gwh.added_gids_in_rule;
                                       }

    | Command.NEW_NODE (created_name) ->
      let (new_gid,new_graph) = G_graph.add_unordered gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton { gwh with
                                         Graph_with_history.graph = new_graph;
                                         added_gids = (created_name, new_gid) :: gwh.Graph_with_history.added_gids;
                                         added_gids_in_rule = (created_name,new_gid) :: gwh.added_gids_in_rule;
                                       }

    | Command.DEL_EDGE_FEAT (edge_id, feat_name) ->
      begin
        match String_map.find_opt edge_id gwh.e_mapping with
        | None -> Error.run ~loc "The edge identifier '%s' is undefined" edge_id
        | Some (src_gid,old_edge,tar_gid) ->
          begin
            match G_graph.del_edge_feature_opt ~loc edge_id feat_name (src_gid,old_edge,tar_gid) gwh.Graph_with_history.graph with
            | None when !Global.safe_commands -> Error.run ~loc "DEL_EDGE_FEAT: the edge feature name '%s' does not exist" feat_name
            | None -> Graph_with_history_set.singleton gwh
            | Some (new_graph, new_edge, capture) ->
              let delta = 
                if capture 
                then
                  gwh.Graph_with_history.delta
                  |> Delta.del_edge src_gid old_edge tar_gid
                else
                  gwh.Graph_with_history.delta
                  |> Delta.del_edge src_gid old_edge tar_gid
                  |> Delta.add_edge src_gid new_edge tar_gid in
              Graph_with_history_set.singleton
                {gwh with
                 Graph_with_history.graph = new_graph;
                 delta;
                 e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) gwh.e_mapping;
                }
          end
      end

    | Command.CONCAT_FEATS (side, src_cn, tar_cn, regexp, separator) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      begin
        match G_graph.concat_feats_opt gwh.Graph_with_history.graph side src_gid tar_gid separator regexp with
        | None when !Global.safe_commands -> Error.run ~loc "CONCAT_FEATS uneffective"
        | None -> Graph_with_history_set.singleton gwh
        | Some (new_graph, updated_edges) ->
          Graph_with_history_set.singleton { gwh with
                                             Graph_with_history.graph = new_graph;
                                             delta = List.fold_left
                                                 (fun acc (feature_name, value) ->
                                                    Delta.set_feat gwh.Graph_with_history.seed tar_gid feature_name (Some value) acc)
                                                 gwh.Graph_with_history.delta updated_edges;
                                           }
      end
    | Command.UNORDER node_cn ->
      let node_gid = node_find node_cn in
      (match G_graph.unorder_opt node_gid gwh.Graph_with_history.graph with
       | None when !Global.safe_commands -> Error.run ~loc "UNORDER: the node is not ordered"
       | None -> Graph_with_history_set.singleton gwh
       | Some new_graph -> Graph_with_history_set.singleton
                             { gwh with
                               Graph_with_history.graph = new_graph;
                               delta = Delta.unorder node_gid gwh.Graph_with_history.delta;
                             }
      )

    | Command.INSERT_BEFORE (inserted_cn, site_cn) ->
      let inserted_gid = node_find inserted_cn in
      let site_gid = node_find site_cn in
      let new_graph = G_graph.insert_before inserted_gid site_gid gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton
        { gwh with
          Graph_with_history.graph = new_graph;
          delta = Delta.insert_before inserted_gid site_gid gwh.Graph_with_history.delta;
        }

    | Command.INSERT_AFTER (inserted_cn, site_cn) ->
      let inserted_gid = node_find inserted_cn in
      let site_gid = node_find site_cn in
      let new_graph = G_graph.insert_after inserted_gid site_gid gwh.Graph_with_history.graph in
      Graph_with_history_set.singleton
        { gwh with
          Graph_with_history.graph = new_graph;
          delta = Delta.insert_after inserted_gid site_gid gwh.Graph_with_history.delta;
        }

  (*  ---------------------------------------------------------------------- *)
  (** [apply_rule graph_with_history matching rule] returns a new graph_with_history after the application of the rule *)
  let gwh_apply_rule ~config graph_with_history matching rule =
    Timeout.check ();
    incr_rules rule;
    let init = Graph_with_history_set.singleton
        { graph_with_history with
          e_mapping = matching.Matching.e_match;
          added_gids_in_rule = [];
        } in

    CCList.foldi
      (fun gwh_set cmp_nb cmd ->
         Graph_with_history_set.fold
           (fun gwh acc ->
              let new_graphs = gwh_apply_command ~config cmd matching gwh in
              let new_graphs_with_tracking =
                if !Global.track_history
                then Graph_with_history_set.map
                    (fun g ->
                       let up = Matching.build_deco rule.request matching in
                       let down = Matching.down_deco (g.e_mapping, matching, g.added_gids_in_rule) (CCList.take (cmp_nb+1) rule.commands) in
                       {g with graph = G_graph.track up (get_rule_info rule) down graph_with_history.graph g.graph}
                    ) new_graphs
                else new_graphs in

              Graph_with_history_set.union new_graphs_with_tracking acc
           ) gwh_set Graph_with_history_set.empty
      ) init rule.commands


  let gwh_apply ~config rule graph_with_history =
    try
      let matching_list = Matching.search_request_in_graph ~config ~lexicons:rule.lexicons rule.request graph_with_history.Graph_with_history.graph in
      List.fold_left
        (fun acc matching ->
           Graph_with_history_set.union (gwh_apply_rule ~config graph_with_history matching rule) acc
        ) Graph_with_history_set.empty matching_list
    with Error.Run (msg,_) -> Error.run ~loc:rule.loc "%s" msg



  exception Dead_lock
  let owh_apply_opt ~config rule gwh =
    let graph = gwh.Graph_with_history.graph in
    let { Request.global; ker; exts; _} = rule.request in

    if not (Matching.check_global_constraint global graph)
      then None
      else
        (* get the list of matching for kernel part of the request *)
        let matching_list = Matching.extend_matching ~config (ker.graph,P_graph.empty) graph (Matching.init ~lexicons:rule.lexicons ker) in
          
        let rec loop_matching = function
          | [] -> None
          | (sub, already_matched_gids) :: tail ->
            if List.for_all
                (fun (ext,polarity) ->
                   Matching.test_extension ~config ker graph ext (sub, already_matched_gids) = polarity
                ) exts
            then (* all exts are fulfilled *)
              let init_gwh = { gwh with e_mapping = sub.Matching.e_match; added_gids_in_rule = [] } in
              let rec loop_command acc_gwh = function
                | [] -> acc_gwh
                | command :: tail_commands ->
                  let set = gwh_apply_command ~config command sub acc_gwh in
                  match Graph_with_history_set.choose_opt set with
                  | None -> raise Dead_lock
                  | Some next_gwh -> loop_command next_gwh tail_commands in
              try
                let new_gwh = loop_command init_gwh rule.commands in
                Timeout.check ();
                incr_rules rule;
                let up = Matching.build_deco rule.request sub in
                let down = Matching.down_deco (new_gwh.e_mapping, sub, new_gwh.added_gids_in_rule) rule.commands in
                Some {new_gwh with graph = G_graph.track up (get_rule_info rule) down graph new_gwh.graph }
              with Dead_lock -> loop_matching tail (* failed to apply all commands -> move to the next matching *)
            else loop_matching tail (* some ext part prevents rule app -> move to the next matching *)
        in loop_matching matching_list
end (* module Rule *)
