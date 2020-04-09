(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Log
open Printf

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain
open Grew_edge
open Grew_fs
open Grew_node
open Grew_command
open Grew_graph


module Pattern = struct

  type const =
    | Cst_out of Pid.t * Label_cst.t
    | Cst_in of Pid.t * Label_cst.t
    | Features_eq of Pid.t * string * Pid.t * string
    | Features_diseq of Pid.t * string * Pid.t * string
    (* *)
    | Feature_eq_cst of Pid.t * string * string
    | Feature_diff_cst of Pid.t * string * string
    (* *)
    | Feature_eq_lex of Pid.t * string * (string * string)
    | Feature_diff_lex of Pid.t * string * (string * string)
    (* *)
    | Feature_eq_float of Pid.t * string * float
    | Feature_diff_float of Pid.t * string * float
    (* *)
    | Feature_eq_regexp of Pid.t * string * string
    (* *)
    | Features_ineq of Ast.ineq * Pid.t * string * Pid.t * string
    | Feature_ineq_cst of Ast.ineq * Pid.t * string * float
    (* *)
    | Edge_features_ineq of Ast.ineq * string * string * string * string
    (* *)
    | Filter of Pid.t * P_fs.t (* used when a without impose a fs on a node defined by the match basic *)
    (* *)
    | Node_large_prec of Pid.t * Pid.t
    (* *)
    | Id_prec of Pid.t * Pid.t
    (* *)
    | Label_equal of string * string
    | Label_disequal of string * string
    (* *)
    | Edge_included of string * string
    | Edge_disjoint of string * string
    | Edge_crossing of string * string
    (* *)
    | Covered of Pid.t * string (* node_id, edge_id *)

  let const_to_json ?domain = function
    | Cst_out (pid, label_cst) -> `Assoc ["cst_out", Label_cst.to_json ?domain label_cst]
    | Cst_in (pid, label_cst) -> `Assoc ["cst_in", Label_cst.to_json ?domain label_cst]
    | Features_eq (pid1,fn1,pid2,fn2) ->
      `Assoc ["features_eq",
              `Assoc [
                ("id1", `String (Pid.to_string pid1));
                ("feature_name_1", `String fn1);
                ("id2", `String (Pid.to_string pid2));
                ("feature_name_2", `String fn2);
              ]
             ]
    | Features_diseq (pid1,fn1,pid2,fn2) ->
      `Assoc ["features_diseq",
              `Assoc [
                ("id1", `String (Pid.to_string pid1));
                ("feature_name_1", `String fn1);
                ("id2", `String (Pid.to_string pid2));
                ("feature_name_2", `String fn2);
              ]
             ]
    | Feature_eq_cst (pid,fn,value) ->
      `Assoc ["feature_eq_cst",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("feature_name_", `String fn);
                ("value", `String value);
              ]
             ]
    | Feature_diff_cst (pid,fn,value) ->
      `Assoc ["feature_diff_cst",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("feature_name_", `String fn);
                ("value", `String value);
              ]
             ]
    | Feature_eq_lex (pid,fn,(lex,field)) ->
      `Assoc ["feature_eq_lex",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("feature_name_", `String fn);
                ("lexicon", `String lex);
                ("field", `String field);
              ]
             ]
    | Feature_diff_lex (pid,fn,(lex,field)) ->
      `Assoc ["feature_diff_lex",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("feature_name_", `String fn);
                ("lexicon", `String lex);
                ("field", `String field);
              ]
             ]


    | Feature_eq_float (pid,fn,value) ->
      `Assoc ["feature_eq_float",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("feature_name_", `String fn);
                ("value", `String (string_of_float value));
              ]
             ]
    | Feature_diff_float (pid,fn,value) ->
      `Assoc ["feature_diff_float",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("feature_name", `String fn);
                ("value", `String (string_of_float value));
              ]
             ]
    | Feature_eq_regexp (pid,fn,regexp) ->
      `Assoc ["feature_eq_regexp",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("feature_name", `String fn);
                ("regexp", `String regexp);
              ]
             ]
    | Features_ineq (ineq,pid1,fn1,pid2,fn2) ->
      `Assoc ["features_ineq",
              `Assoc [
                ("ineq", `String (Ast.string_of_ineq ineq));
                ("id1", `String (Pid.to_string pid1));
                ("feature_name_1", `String fn1);
                ("id2", `String (Pid.to_string pid2));
                ("feature_name_2", `String fn2);
              ]
             ]
    | Feature_ineq_cst (ineq,pid,fn,value) ->
      `Assoc ["feature_ineq_cst",
              `Assoc [
                ("ineq", `String (Ast.string_of_ineq ineq));
                ("id", `String (Pid.to_string pid));
                ("feature_name", `String fn);
                ("value", `String (string_of_float value));
              ]
             ]
    | Edge_features_ineq (ineq,id1,fn1,id2,fn2) ->
      `Assoc ["features_ineq",
              `Assoc [
                ("ineq", `String (Ast.string_of_ineq ineq));
                ("id1", `String (id1));
                ("feature_name_1", `String fn1);
                ("id2", `String (id2));
                ("feature_name_2", `String fn2);
              ]
             ]
    | Filter (pid, p_fs) ->
      `Assoc ["filter",
              `Assoc [
                ("id", `String (Pid.to_string pid));
                ("fs", P_fs.to_json ?domain p_fs);
              ]
             ]
    | Node_large_prec (pid1, pid2) ->
      `Assoc ["node_large_prec",
              `Assoc [
                ("id1", `String (Pid.to_string pid1));
                ("id2", `String (Pid.to_string pid2));
              ]
             ]
    | Id_prec (pid1, pid2) ->
      `Assoc ["id_prec",
              `Assoc [
                ("id1", `String (Pid.to_string pid1));
                ("id2", `String (Pid.to_string pid2));
              ]
             ]
    | Label_equal (eid1, eid2) ->
      `Assoc ["label_equal",
              `Assoc [
                ("id1", `String eid1);
                ("id2", `String eid2);
              ]
             ]
    | Label_disequal (eid1, eid2) ->
      `Assoc ["label_disequal",
              `Assoc [
                ("id1", `String eid1);
                ("id2", `String eid2);
              ]
             ]
    | Edge_included (eid1, eid2) ->
      `Assoc ["edge_included",
              `Assoc [
                ("id1", `String eid1);
                ("id2", `String eid2);
              ]
             ]
    | Edge_disjoint (eid1, eid2) ->
      `Assoc ["edge_disjoint",
              `Assoc [
                ("id1", `String eid1);
                ("id2", `String eid2);
              ]
             ]
    | Edge_crossing (eid1, eid2) ->
      `Assoc ["edge_crossing",
              `Assoc [
                ("id1", `String eid1);
                ("id2", `String eid2);
              ]
             ]
    | Covered (pid1, eid2) -> `Assoc ["covered",`Assoc [("id1", `String (Pid.to_string pid1)); ("id2", `String eid2)]]


  let build_pos_constraint ?domain lexicons pos_table const =
    let pid_of_name loc node_name = Pid.Pos (Id.build ~loc node_name pos_table) in
    match const with
    | (Ast.Cst_out (id,label_cst), loc) ->
      Cst_out (pid_of_name loc id, Label_cst.build ~loc ?domain label_cst)
    | (Ast.Cst_in (id,label_cst), loc) ->
      Cst_in (pid_of_name loc id, Label_cst.build ~loc ?domain label_cst)

    | (Ast.Features_eq ((node_name1, feat_name1), (node_name2, feat_name2)), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name1;
      Domain.check_feature_name ?domain ~loc feat_name2;
      Features_eq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

    | (Ast.Features_diseq ((node_name1, feat_name1), (node_name2, feat_name2)), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name1;
      Domain.check_feature_name ?domain ~loc feat_name2;
      Features_diseq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

    | (Ast.Features_ineq (ineq, (id1, feat_name1), (id2, feat_name2)), loc) ->
      begin
        match (Id.build_opt id1 pos_table, Id.build_opt id2 pos_table) with
        | (None,None) -> Edge_features_ineq (ineq, id1, feat_name1, id2, feat_name2)
        | (Some x, Some y) -> Features_ineq (ineq, Pid.Pos x, feat_name1, Pid.Pos y, feat_name2)
        | _ -> Error.build "Operator < can be used only with two nodes or two edges"
      end

    | (Ast.Feature_ineq_cst (ineq, (node_name1, feat_name1), constant), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name1;
      Feature_ineq_cst (ineq, pid_of_name loc node_name1, feat_name1, constant)

    | (Ast.Feature_eq_regexp ((node_name, feat_name), regexp), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_regexp (pid_of_name loc node_name, feat_name, regexp)

    | (Ast.Feature_eq_cst ((node_name, feat_name), string), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_cst (pid_of_name loc node_name, feat_name, string)
    | (Ast.Feature_diff_cst ((node_name, feat_name), string), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_diff_cst (pid_of_name loc node_name, feat_name, string)

    (* | (Ast.Feature_eq_lex ((node_name, feat_name), lf), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_lex (pid_of_name loc node_name, feat_name, lf) *)
    | (Ast.Feature_diff_lex ((node_name, feat_name), lf), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_diff_lex (pid_of_name loc node_name, feat_name, lf)

    | (Ast.Feature_eq_float ((node_name, feat_name), float), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_float (pid_of_name loc node_name, feat_name, float)
    | (Ast.Feature_diff_float ((node_name, feat_name), float), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_diff_float (pid_of_name loc node_name, feat_name, float)

    (* WARNING: the ast Command [Large_prec] can be translated as:
       - Node_large_prec if arguments are nodes id
       - Edge_included if arguments are edges id
       - Covered if args are node and edge
    *)
    | (Ast.Large_prec (id1, id2), loc) ->
      begin
        match (Id.build_opt id1 pos_table, Id.build_opt id2 pos_table) with
        | (None,None) -> Edge_included (id1, id2)
        | (Some x, Some y) -> Node_large_prec (Pid.Pos x, Pid.Pos y)
        | (Some x, None) -> Covered (Pid.Pos x, id2)
        | _ -> Error.build "Operator << can be used only with two nodes if two edges"
      end

    | (Ast.Label_equal (eid1, eid2), loc) ->
      Label_equal (eid1, eid2)

    | (Ast.Label_disequal (eid1, eid2), loc) ->
      Label_disequal (eid1, eid2)

    | (Ast.Feature_eq_lex_or_fs ((node_name, feat_name),(node_or_lex, fn_or_field)), loc) ->
      begin
        match Id.build_opt node_or_lex pos_table with
        | None ->
          Lexicons.check ~loc node_or_lex fn_or_field lexicons;
          Feature_eq_lex (pid_of_name loc node_name, feat_name, (node_or_lex, fn_or_field))
        | _ ->  Features_eq (pid_of_name loc node_name, feat_name, pid_of_name loc node_or_lex, fn_or_field)
      end

    | (Ast.Feature_diff_lex_or_fs ((node_name, feat_name),(node_or_lex, fn_or_field)), loc) ->
      begin
        match Id.build_opt node_or_lex pos_table with
        | None ->
          Lexicons.check ~loc node_or_lex fn_or_field lexicons;
          Feature_diff_lex (pid_of_name loc node_name, feat_name, (node_or_lex, fn_or_field))
        | _ ->  Features_diseq (pid_of_name loc node_name, feat_name, pid_of_name loc node_or_lex, fn_or_field)
      end
    | (Ast.Id_prec (id1, id2), loc) ->
      Id_prec (pid_of_name loc id1, pid_of_name loc id2)

    | (Ast.Edge_disjoint (eid1, eid2), loc) ->
      Edge_disjoint (eid1, eid2)
    | (Ast.Edge_crossing (eid1, eid2), loc) ->
      Edge_crossing (eid1, eid2)

  type basic = {
    graph: P_graph.t;
    constraints: const list;
  }

  let basic_to_json ?domain basic =
    `Assoc [
      ("graph", P_graph.to_json ?domain basic.graph);
      ("constraints", `List (List.map (const_to_json ?domain) basic.constraints));
    ]

  let build_pos_basic ?domain lexicons basic_ast =
    let (graph, pos_table) =
      P_graph.build ?domain lexicons basic_ast in
    (
      {
        graph = graph;
        constraints = List.map (build_pos_constraint ?domain lexicons pos_table) basic_ast.Ast.pat_const
      },
      pos_table
    )

  (* the neg part *)
  let build_neg_constraint ?domain lexicons pos_table neg_table const =
    let pid_of_name loc node_name =
      match Id.build_opt node_name pos_table with
      | Some i -> Pid.Pos i
      | None -> Pid.Neg (Id.build ~loc node_name neg_table) in
    let pid_of_name_opt node_name =
      match (Id.build_opt node_name pos_table, Id.build_opt node_name neg_table) with
      | (Some i, _) -> Some (Pid.Pos i)
      | (_, Some i) -> Some (Pid.Neg i)
      | _ -> None in
    match const with
    | (Ast.Cst_out (id,label_cst), loc) ->
      Cst_out (pid_of_name loc id, Label_cst.build ~loc ?domain label_cst)
    | (Ast.Cst_in (id,label_cst), loc) ->
      Cst_in (pid_of_name loc id, Label_cst.build ~loc ?domain label_cst)

    | (Ast.Features_eq (feat_id1, feat_id2), loc) ->
      let (node_name1, feat_name1) = feat_id1
      and (node_name2, feat_name2) = feat_id2 in
      Domain.check_feature_name ?domain ~loc feat_name1;
      Domain.check_feature_name ?domain ~loc feat_name2;
      Features_eq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

    | (Ast.Features_diseq (feat_id1, feat_id2), loc) ->
      let (node_name1, feat_name1) = feat_id1
      and (node_name2, feat_name2) = feat_id2 in
      Domain.check_feature_name ?domain ~loc feat_name1;
      Domain.check_feature_name ?domain ~loc feat_name2;
      Features_diseq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

    | (Ast.Features_ineq (ineq, (id1, feat_name1), (id2, feat_name2)), loc) ->
      begin
        match (pid_of_name_opt id1, pid_of_name_opt id2) with
        | (Some pid1, Some pid2) -> Features_ineq (ineq, pid1, feat_name1, pid2, feat_name2)
        | (None, None) ->
        printf "### Edge_features_ineq (_,%s,%s,%s,%s)\n%!" id1 feat_name1 id2 feat_name2;
        Edge_features_ineq (ineq, id1, feat_name1, id2, feat_name2)
        | _ -> Error.build "Operator < can be used only with two nodes or two edges"
      end

    | (Ast.Feature_ineq_cst (ineq, feat_id1, constant), loc) ->
      let (node_name1, feat_name1) = feat_id1 in
      Domain.check_feature_name ?domain ~loc feat_name1;
      Feature_ineq_cst (ineq, pid_of_name loc node_name1, feat_name1, constant)

    | (Ast.Feature_eq_regexp (feat_id, regexp), loc) ->
      let (node_name, feat_name) = feat_id in
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_regexp (pid_of_name loc node_name, feat_name, regexp)

    | (Ast.Feature_eq_cst ((node_name, feat_name), string), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_cst (pid_of_name loc node_name, feat_name, string)
    | (Ast.Feature_diff_cst ((node_name, feat_name), string), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_diff_cst (pid_of_name loc node_name, feat_name, string)

    (* | (Ast.Feature_eq_lex ((node_name, feat_name), lf), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_lex (pid_of_name loc node_name, feat_name, lf) *)
    | (Ast.Feature_diff_lex ((node_name, feat_name), lf), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_diff_lex (pid_of_name loc node_name, feat_name, lf)

    | (Ast.Feature_eq_float ((node_name, feat_name), float), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_eq_float (pid_of_name loc node_name, feat_name, float)
    | (Ast.Feature_diff_float ((node_name, feat_name), float), loc) ->
      Domain.check_feature_name ?domain ~loc feat_name;
      Feature_diff_float (pid_of_name loc node_name, feat_name, float)

    (* WARNING: the ast Command [Large_prec] can be translated as:
       - Node_large_prec if arguments are nodes id
       - Edge_included if arguments are edges id
       - Covered if args are node and edge
    *)
    | (Ast.Large_prec (id1, id2), loc) ->
      begin
        match (Id.build_opt id1 pos_table, Id.build_opt id1 neg_table, Id.build_opt id2 pos_table, Id.build_opt id2 neg_table) with
        | (None,None,None,None) -> Edge_included (id1, id2)
        | (_,_,None,None) -> Covered (pid_of_name loc id1, id2)
        | (None,None,_,_) -> Error.build "Operator << can be used only with two nodes if two edges"
        | _ -> Node_large_prec  (pid_of_name loc id1, pid_of_name loc id2)
      end

    | (Ast.Label_equal (eid1, eid2), loc) ->
      Label_equal (eid1, eid2)

    | (Ast.Label_disequal (eid1, eid2), loc) ->
      Label_disequal (eid1, eid2)

    | (Ast.Feature_eq_lex_or_fs ((node_name, feat_name),(node_or_lex, fn_or_field)), loc) ->
      begin
        match (Id.build_opt node_or_lex pos_table, Id.build_opt node_or_lex neg_table) with
        | (None, None) ->
          Lexicons.check ~loc node_or_lex fn_or_field lexicons;
          Feature_eq_lex (pid_of_name loc node_name, feat_name, (node_or_lex, fn_or_field))
        | _ ->  Features_eq (pid_of_name loc node_name, feat_name, pid_of_name loc node_or_lex, fn_or_field)
      end

    | (Ast.Feature_diff_lex_or_fs ((node_name, feat_name),(node_or_lex, fn_or_field)), loc) ->
      begin
        match (Id.build_opt node_or_lex pos_table, Id.build_opt node_or_lex neg_table) with
        | (None, None) -> Feature_diff_lex (pid_of_name loc node_name, feat_name, (node_or_lex, fn_or_field))
        | _ ->  Features_diseq (pid_of_name loc node_name, feat_name, pid_of_name loc node_or_lex, fn_or_field)
      end

    | (Ast.Id_prec (id1, id2), loc) ->
      Id_prec (pid_of_name loc id1, pid_of_name loc id2)

    | (Ast.Edge_disjoint (eid1, eid2), loc) ->
      Edge_disjoint (eid1, eid2)
    | (Ast.Edge_crossing (eid1, eid2), loc) ->
      Edge_crossing (eid1, eid2)


  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_neg_basic ?domain lexicons pos_table basic_ast =
    let (extension, neg_table) =
      P_graph.build_extension ?domain lexicons pos_table basic_ast.Ast.pat_nodes basic_ast.Ast.pat_edges in

    let filters = Pid_map.fold (fun id node acc -> Filter (id, P_node.get_fs node) :: acc) extension.P_graph.old_map [] in
    {
      graph = extension.P_graph.ext_map;
      constraints = filters @ List.map (build_neg_constraint ?domain lexicons pos_table neg_table) basic_ast.Ast.pat_const ;
    }

  let get_edge_ids basic =
    Pid_map.fold
      (fun _ node acc ->
         Massoc_pid.fold
           (fun acc2 _ edge -> match P_edge.get_id_opt edge with Some id -> id::acc2 | None -> acc2)
           acc (P_node.get_next node)
      ) basic.graph []

  (* a [pattern] is described by the positive basic and a list of negative basics. *)
  type t = {
    global: Ast.glob list;
    pos: basic;
    negs: basic list;
  }

  let pid_name_list pattern = P_graph.pid_name_list pattern.pos.graph

  let build ?domain ?(lexicons=[]) pattern_ast =
    let n_pattern = Ast.normalize_pattern pattern_ast in
    let (pos, pos_table) =
      try build_pos_basic ?domain lexicons n_pattern.Ast.pat_pos
      with P_fs.Fail_unif -> Error.build "feature structures declared in the \"match\" clause are inconsistent " in
    let negs =
      List_.try_map
        P_fs.Fail_unif (* Skip the without parts that are incompatible with the match part *)
        (fun basic_ast -> build_neg_basic ?domain lexicons pos_table basic_ast)
        n_pattern.Ast.pat_negs in
    { pos; negs; global=pattern_ast.pat_glob; }


end (* module Pattern *)

(* ================================================================================ *)
module Matching = struct

  (* ====================================================================== *)
  type t = {
    n_match: Gid.t Pid_map.t;                      (* partial fct: pattern nodes |--> graph nodes *)
    e_match: (Gid.t*G_edge.t*Gid.t) String_map.t;  (* edge matching: edge ident  |--> (src,label,tar) *)
    l_param: Lexicons.t;                           (* *)
  }

  let empty ?(lexicons=[]) () =
    { n_match = Pid_map.empty; e_match = String_map.empty; l_param = lexicons;}

  let intern s = String_.re_match (Str.regexp "__.*__") s

  let to_json ?(all_edges=false) pattern graph m =
    let node_name gid = G_node.get_name gid (G_graph.find gid graph) in
    let nodes = Pid_map.fold (fun pid gid acc ->
        let pnode = P_graph.find pid pattern.Pattern.pos.graph in
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

  let node_matching pattern graph { n_match } =
    Pid_map.fold
      (fun pid gid acc ->
         let pnode = P_graph.find pid pattern.Pattern.pos.graph in
         let gnode = G_graph.find gid graph in
         (P_node.get_name pnode, G_node.get_name gid gnode) :: acc
      ) n_match []

  exception Found of Pid.t
  let get_pid_by_name pattern name n_match =
    try
      Pid_map.iter
        (fun pid _ ->
           if P_node.get_name (P_graph.find pid pattern.Pattern.pos.graph) = name
           then raise (Found pid)
        ) n_match;
      None
    with Found pid -> Some pid

  (* return the value of a feature or an edge label *)
  let get_value_opt request pattern graph matching =
    match Str.split (Str.regexp "\\.") request with
    | [edge_id] ->
      begin
        match String_map.find_opt edge_id matching.e_match with
        | None -> Error.run "[Matching.get_value_opt] unknown edge_id %s" edge_id
        | Some (_,edge,_) ->
          match G_edge.to_string_opt edge with
          | Some s -> Some s
          | None -> Error.bug "[Matching.get_value_opt] internal edge %s" (G_edge.dump edge)
      end
    | [node_or_edge_id; feature_name] ->
      begin
        match String_map.find_opt node_or_edge_id matching.e_match with
        | Some (_,edge,_) ->
          begin
            match G_edge.get_sub_opt feature_name edge with
            | Some e -> Some e
            | None -> Error.bug "[Matching.get_value_opt] internal edge %s" (G_edge.dump edge)
          end
        | None ->
          begin
            match get_pid_by_name pattern node_or_edge_id matching.n_match with (* TODO: edge feature "e.deep" *)
            | None -> Error.run "[Matching.get_value_opt] unknown id %s" node_or_edge_id
            | Some pid ->
              let gid = Pid_map.find pid matching.n_match in
              let node = G_graph.find gid graph in
              let fs = G_node.get_fs node in
              G_fs.get_string_atom_opt feature_name fs
          end
      end
    | _ -> Error.run "[Matching.get_value_opt] unable to handled request %s" request

  let e_match_add ?pos edge_id new_edge matching =
    if String_map.mem edge_id matching.e_match
    then Error.run "The edge identifier '%s' is binded twice in the same pattern" edge_id
    else { matching with e_match = String_map.add edge_id new_edge matching.e_match }

  let match_deco pattern matching =
    { G_deco.nodes =
        Pid_map.fold
          (fun pid gid acc ->
             let pnode = P_graph.find pid pattern.Pattern.pos.graph in
             let pattern_feat_list = P_fs.feat_list (P_node.get_fs pnode) in
             (gid, (P_node.get_name pnode, pattern_feat_list)) :: acc
          ) matching.n_match [];
      G_deco.edges = String_map.fold (fun _ edge acc -> edge::acc) matching.e_match [];
    }

  let find cnode ?loc (matching, created_nodes) =
    match cnode with
    | Command.Pat pid ->
      (try Pid_map.find pid matching.n_match
       with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name ->
      (try List.assoc name created_nodes
       with Not_found -> Error.run ?loc "Identifier '%s' not found" name)

  let down_deco (matching,created_nodes) commands =
    let feat_to_highlight = List.fold_left
        (fun acc -> function
           | (Command.UPDATE_FEAT (tar_cn,feat_name,_),loc) ->
             (* | (Command.SHIFT_EDGE (_,tar_cn),loc) *)
             let gid = find tar_cn (matching, created_nodes) in
             let old_feat_list = try Gid_map.find gid acc with Not_found -> [] in
             Gid_map.add gid (feat_name :: old_feat_list) acc
           | _ -> acc
        ) Gid_map.empty commands in

    {
      G_deco.nodes = List.map (fun (gid,feat_list) ->
          (gid, ("", (List.map (fun x -> (x,None)) feat_list)))
        ) (Gid_map.bindings feat_to_highlight);
      G_deco.edges = List.fold_left (fun acc -> function
          | (Command.ADD_EDGE (src_cn,tar_cn,edge),loc) ->
            (find src_cn (matching, created_nodes), edge, find tar_cn (matching, created_nodes)) :: acc
          | _ -> acc
        ) [] commands;
    }

  exception Fail
  type partial = {
    sub: t;
    unmatched_nodes: Pid.t list;
    unmatched_edges: (Pid.t * P_edge.t * Pid.t) list;
    already_matched_gids: Gid.t list; (* to ensure injectivity *)
    check: Pattern.const list (* constraints to verify at the end of the matching *)
  }

  (* PREREQUISITES:
     - all partial matching have the same ?domain
     - the ?domain of the pattern P is the disjoint union of ?domain([sub]) and [unmatched_nodes]
  *)
  (*  ---------------------------------------------------------------------- *)
  let init ?lexicons basic =
    let roots = P_graph.roots basic.Pattern.graph in

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

  type edge_relative_position =
    | Included
    | Contained
    | Disjoint
    | Crossing
    | Non_ordered

  let get_relative_position (l1,_,r1) (l2,_,r2) graph =
    match (G_graph.find_opt l1 graph, G_graph.find_opt r1 graph, G_graph.find_opt l2 graph, G_graph.find_opt r2 graph) with
    | (Some nl1, Some nr1, Some nl2, Some nr2) ->
      begin
        match (G_node.get_position_opt nl1, G_node.get_position_opt nr1, G_node.get_position_opt nl2, G_node.get_position_opt nr2) with
        | (Some pl1, Some pr1, Some pl2, Some pr2) when pr1 <= pl2 || pr2 <= pl1 -> Disjoint
        | (Some pl1, Some pr1, Some pl2, Some pr2) when pl1 <= pl2 && pr2 <= pr1 -> Contained
        | (Some pl1, Some pr1, Some pl2, Some pr2) when pl2 <= pl1 && pr1 <= pr2 -> Included
        | (Some _, Some _, Some _, Some _) -> Crossing
        | _ -> Non_ordered
      end
    | _ -> Error.bug "[get_relative_position]"

  (*  ---------------------------------------------------------------------- *)
  let apply_cst ?domain graph matching cst =
    let get_node pid = G_graph.find (Pid_map.find pid matching.n_match) graph in
    let get_string_feat_opt pid feat_name = G_fs.get_string_atom_opt feat_name (G_node.get_fs (get_node pid)) in
    let get_float_feat_opt pid feat_name = G_fs.get_float_feat_opt feat_name (G_node.get_fs (get_node pid)) in
    printf "### 000\n%!";

    match cst with
    | Pattern.Cst_out (pid,label_cst) ->
      let gid = Pid_map.find pid matching.n_match in
      if G_graph.edge_out graph gid label_cst
      then matching
      else raise Fail
    | Cst_in (pid,label_cst) ->
      let gid = Pid_map.find pid matching.n_match in
      if G_graph.node_exists
          (fun node ->
             List.exists (fun e -> Label_cst.match_ ?domain label_cst e) (Massoc_gid.assoc gid (G_node.get_next node))
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
    | Features_eq (pid1, feat_name1, pid2, feat_name2) ->
      begin
        match (get_string_feat_opt pid1 feat_name1, get_string_feat_opt pid2 feat_name2) with
        | Some fv1, Some fv2 when fv1 = fv2 -> matching
        | _ -> raise Fail
      end
    | Feature_eq_cst (pid1, feat_name1, value) ->
      begin
        match get_string_feat_opt pid1 feat_name1 with
        | Some fv1 when fv1 = value -> matching
        | _ -> raise Fail
      end
    | Feature_diff_cst (pid1, feat_name1, value) ->
      begin
        match get_string_feat_opt pid1 feat_name1 with
        | Some fv1 when fv1 <> value -> matching
        | _ -> raise Fail
      end
    | Feature_eq_float (pid1, feat_name1, float) ->
      begin
        match get_float_feat_opt pid1 feat_name1 with
        | Some fv1 when fv1 = float -> matching
        | _ -> raise Fail
      end
    | Feature_diff_float (pid1, feat_name1, float) ->
      begin
        match get_float_feat_opt pid1 feat_name1 with
        | Some fv1 when fv1 <> float -> matching
        | _ -> raise Fail
      end
    | Features_diseq (pid1, feat_name1, pid2, feat_name2) ->
      begin
        match (get_string_feat_opt pid1 feat_name1, get_string_feat_opt pid2 feat_name2) with
        | Some fv1, Some fv2 when fv1 <> fv2 -> matching
        | _ -> raise Fail
      end
    | Features_ineq (ineq, pid1, feat_name1, pid2, feat_name2) ->
      begin
        match (ineq, get_float_feat_opt pid1 feat_name1, get_float_feat_opt pid2 feat_name2) with
        | (Ast.Lt, Some fv1, Some fv2) when fv1 < fv2 -> matching
        | (Ast.Gt, Some fv1, Some fv2) when fv1 > fv2 -> matching
        | (Ast.Le, Some fv1, Some fv2) when fv1 <= fv2 -> matching
        | (Ast.Ge, Some fv1, Some fv2) when fv1 >= fv2 -> matching
        | _ -> raise Fail
      end
    | Feature_ineq_cst (ineq, pid1, feat_name1, constant) ->
      begin
        match (ineq, get_float_feat_opt pid1 feat_name1) with
        | (Ast.Lt, Some fv1) when fv1 < constant -> matching
        | (Ast.Gt, Some fv1) when fv1 > constant -> matching
        | (Ast.Le, Some fv1) when fv1 <= constant -> matching
        | (Ast.Ge, Some fv1) when fv1 >= constant -> matching
        | _ -> raise Fail
      end
    | Edge_features_ineq (ineq, eid1, feat_name1, eid2, feat_name2) ->
      begin
        printf "### +++ Edge_features_ineq (_,%s,%s,%s,%s)\n%!" eid1 feat_name1 eid2 feat_name2;
        match (String_map.find_opt eid1 matching.e_match, String_map.find_opt eid2 matching.e_match) with
        | (Some (_,l1,_), Some (_,l2,_)) ->
          begin
            printf "### 111\n%!";
            match (G_edge.get_sub_opt feat_name1 l1, G_edge.get_sub_opt feat_name2 l2) with
            | (Some v1, Some v2) ->
              begin
                printf "### 222\n%!";
                match (float_of_string_opt v1, float_of_string_opt v2) with
                | (Some f1, Some f2) ->
                  if Ast.check_ineq f1 ineq f2
                  then (printf "### 333\n%!"; matching)
                  else (printf "### 444\n%!"; raise Fail)
                | _ -> Error.build "Cannot compare edge feature values %s and %s" v1 v2
              end
            | (None, _) -> Error.build "Unknown feature %s for edge %s" feat_name1 eid1
            | (_, None) -> Error.build "Unknown feature %s for edge %s" feat_name2 eid2
          end
        | (None, _) -> Error.build "Unknown identifier %s" eid1
        | (_, None) -> Error.build "Unknown identifier %s" eid2
      end

    | Feature_eq_regexp (pid, feat_name, regexp) ->
      begin
        match get_string_feat_opt pid feat_name with
        | None -> raise Fail
        | Some string_feat ->
          let re = Str.regexp regexp in
          if String_.re_match re string_feat then matching else raise Fail
      end

    | Node_large_prec (pid1, pid2) ->
      let gnode1 = G_graph.find (Pid_map.find pid1 matching.n_match) graph in
      let gnode2 = G_graph.find (Pid_map.find pid2 matching.n_match) graph in
      begin
        match (G_node.get_position_opt gnode1, G_node.get_position_opt gnode2) with
        | Some i1, Some i2 when i1 < i2 -> matching
        | _ -> raise Fail
      end

    | Id_prec (pid1, pid2) ->
      let gid1 = Pid_map.find pid1 matching.n_match in
      let gid2 = Pid_map.find pid2 matching.n_match in
      if gid1 < gid2
      then matching
      else raise Fail

    | Label_equal (eid1, eid2) ->
      begin
        match (String_map.find_opt eid1 matching.e_match, String_map.find_opt eid2 matching.e_match) with
        | (Some (_,e1,_), Some (_,e2,_)) when e1 = e2 -> matching
        | (Some (_,e1,_), Some (_,e2,_)) -> raise Fail
        | (None, Some _) -> Error.build "Edge identifier '%s' not found" eid1;
        | (Some _, None) -> Error.build "Edge identifier '%s' not found" eid2;
        | (None, None) -> Error.build "Edge identifiers '%s' and '%s' not found" eid1 eid2;
      end

    | Label_disequal (eid1, eid2) ->
      begin
        match (String_map.find_opt eid1 matching.e_match, String_map.find_opt eid2 matching.e_match) with
        | (Some (_,e1,_), Some (_,e2,_)) when e1 <> e2 -> matching
        | (Some (_,e1,_), Some (_,e2,_)) -> raise Fail
        | (None, Some _) -> Error.build "Edge identifier '%s' not found" eid1;
        | (Some _, None) -> Error.build "Edge identifier '%s' not found" eid2;
        | (None, None) -> Error.build "Edge identifiers '%s' and '%s' not found" eid1 eid2;
      end

    | Feature_eq_lex (pid, feature_name, (lexicon,field)) ->
      begin
        match get_string_feat_opt pid feature_name with
        | None -> raise Fail
        | Some v ->
          let old_lex = List.assoc lexicon matching.l_param in
          match Lexicon.select_opt field v old_lex with
          | None -> raise Fail
          | Some new_lex -> {matching with l_param = (lexicon, new_lex) :: (List.remove_assoc lexicon matching.l_param) }
      end

    | Feature_diff_lex (pid, feature_name, (lexicon,field)) ->
      begin
        match get_string_feat_opt pid feature_name with
        | None -> raise Fail
        | Some v ->
          let old_lex = List.assoc lexicon matching.l_param in
          match Lexicon.unselect_opt field v old_lex with
          | None -> raise Fail
          | Some new_lex -> {matching with l_param = (lexicon, new_lex) :: (List.remove_assoc lexicon matching.l_param) }
      end

    | Edge_included (eid1, eid2) ->
      begin
        match (String_map.find_opt eid1 matching.e_match, String_map.find_opt eid2 matching.e_match) with
        | (Some e1, Some e2) when get_relative_position e1 e2 graph = Included -> matching
        | (Some _, Some _) -> raise Fail
        | (None, Some _) -> Error.build "Edge identifier '%s' not found" eid1;
        | (Some _, None) -> Error.build "Edge identifier '%s' not found" eid2;
        | (None, None) -> Error.build "Edge identifiers '%s' and '%s' not found" eid1 eid2;
      end

    | Edge_disjoint (eid1, eid2) ->
      begin
        match (String_map.find_opt eid1 matching.e_match, String_map.find_opt eid2 matching.e_match) with
        | (Some e1, Some e2) when get_relative_position e1 e2 graph = Disjoint -> matching
        | (Some _, Some _) -> raise Fail
        | (None, Some _) -> Error.build "Edge identifier '%s' not found" eid1;
        | (Some _, None) -> Error.build "Edge identifier '%s' not found" eid2;
        | (None, None) -> Error.build "Edge identifiers '%s' and '%s' not found" eid1 eid2;
      end

    | Edge_crossing (eid1, eid2) ->
      begin
        match (String_map.find_opt eid1 matching.e_match, String_map.find_opt eid2 matching.e_match) with
        | (Some e1, Some e2) when get_relative_position e1 e2 graph = Crossing -> matching
        | (Some _, Some _) -> raise Fail
        | (None, Some _) -> Error.build "Edge identifier '%s' not found" eid1;
        | (Some _, None) -> Error.build "Edge identifier '%s' not found" eid2;
        | (None, None) -> Error.build "Edge identifiers '%s' and '%s' not found" eid1 eid2;
      end

    | Covered (pid, eid) ->
      begin
        let gnode = G_graph.find (Pid_map.find pid matching.n_match) graph in
        match (String_map.find_opt eid matching.e_match) with
        | (Some edge) when G_graph.covered gnode edge graph -> matching
        | Some _ -> raise Fail
        | (None) -> Error.build "Edge identifier '%s' not found" eid;
      end

  (*  ---------------------------------------------------------------------- *)
  (* returns all extension of the partial input matching *)
  let rec extend_matching ?domain (positive,neg) (graph:G_graph.t) (partial:partial) =
    match (partial.unmatched_edges, partial.unmatched_nodes) with
    | [], [] ->
      begin
        try
          let new_matching =
            List.fold_left
              (fun acc const ->
                 apply_cst ?domain graph acc const
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
            let g_edges = Massoc_gid.assoc tar_gid (G_node.get_next src_gnode) in

            match P_edge.match_list ?domain p_edge g_edges with
            | P_edge.Fail -> (* no good edge in graph for this pattern edge -> stop here *)
              []
            | P_edge.Pass -> [ {partial with unmatched_edges = tail_ue } ]
            | P_edge.Binds (id,labels) -> (* n edges in the graph match the identified p_edge -> make copies of the [k] matchings (and returns n*k matchings) *)
              List.map
                (fun label ->
                   {partial with sub = e_match_add id (src_gid,label,tar_gid) partial.sub; unmatched_edges = tail_ue }
                ) labels
          in List_.flat_map (extend_matching ?domain (positive,neg) graph) new_partials
        with Not_found -> (* p_edge goes to an unmatched node *)
          let candidates = (* candidates (of type (gid, matching)) for m(tar_pid) = gid) with new partial matching m *)
            let (src_gid : Gid.t) = Pid_map.find src_pid partial.sub.n_match in
            let src_gnode = G_graph.find src_gid graph in
            Massoc_gid.fold
              (fun acc gid_next g_edge ->
                 match P_edge.match_ ?domain p_edge g_edge with
                 | P_edge.Fail -> (* g_edge does not fit, no new candidate *)
                   acc
                 | P_edge.Pass -> (gid_next, partial.sub) :: acc
                 | P_edge.Binds (id,[label]) -> (* g_edge fits with an extended matching *)
                   (gid_next, e_match_add id (src_gid, label, gid_next) partial.sub) :: acc
                 | _ -> Error.bug "P_edge.match_ must return exactly one label"
              ) [] (G_node.get_next src_gnode) in
          List_.flat_map
            (fun (gid_next, matching) ->
               extend_matching_from ?domain (positive,neg) graph tar_pid gid_next
                 {partial with sub=matching; unmatched_edges = tail_ue}
            ) candidates
      end
    | [], pid :: _ ->
      G_graph.fold_gid
        (fun gid acc ->
           (extend_matching_from ?domain (positive,neg) graph pid gid partial) @ acc
        ) graph []

  (*  ---------------------------------------------------------------------- *)
  and extend_matching_from ?domain (positive,neg) (graph:G_graph.t) pid (gid : Gid.t) partial =
    if List.mem gid partial.already_matched_gids
    then [] (* the required association pid -> gid is not injective *)
    else
      let p_node =
        try P_graph.find pid positive
        with Not_found ->
        try P_graph.find pid neg
        with Not_found -> Error.bug "[Grew_rule.extend_matching_from] cannot find node" in

      let g_node = try G_graph.find gid graph with Not_found -> Error.bug "[extend_matching_from] cannot find gid in graph" in

      try
        let new_lex_set = P_node.match_ ~lexicons:partial.sub.l_param p_node g_node in
        (* add all out-edges from pid in pattern *)
        let new_unmatched_edges =
          Massoc_pid.fold
            (fun acc pid_next p_edge -> (pid, p_edge, pid_next) :: acc
            ) partial.unmatched_edges (P_node.get_next p_node) in

        let new_partial =
          { partial with
            unmatched_nodes = (try List_.rm pid partial.unmatched_nodes with Not_found -> Error.bug "[extend_matching_from] cannot find pid in unmatched_nodes");
            unmatched_edges = new_unmatched_edges;
            already_matched_gids = gid :: partial.already_matched_gids;
            sub = {partial.sub with n_match = Pid_map.add pid gid partial.sub.n_match; l_param = new_lex_set};
          } in
        extend_matching ?domain (positive,neg) graph new_partial
      with P_fs.Fail -> []

  (*  [test_locality matching created_nodes gid] checks if [gid] is a "local" node:
      either it belongs to the codomain of [matching] or it is one of the [created_nodes] *)
  let test_locality matching created_nodes gid =
    (Pid_map.exists (fun _ id -> id=gid) matching.n_match) || (List.exists (fun (_,id) -> id=gid) created_nodes)


  (*  ---------------------------------------------------------------------- *)
  let update_partial without (sub, already_matched_gids) =
    let neg_graph = without.Pattern.graph in
    let unmatched_nodes =
      Pid_map.fold
        (fun pid _ acc -> match pid with Pid.Neg _ -> pid::acc | _ -> acc)
        neg_graph [] in
    let unmatched_edges =
      Pid_map.fold
        (fun pid node acc ->
           match pid with
           | Pid.Neg _ -> acc
           | Pid.Pos i ->
             Massoc_pid.fold
               (fun acc2 pid_next p_edge -> (pid, p_edge, pid_next) :: acc2)
               acc (P_node.get_next node)
        ) neg_graph [] in
    {
      sub;
      unmatched_nodes = unmatched_nodes;
      unmatched_edges = unmatched_edges;
      already_matched_gids;
      check = without.constraints;
    }


  (*  ---------------------------------------------------------------------- *)
  let fulfill ?domain (pos_graph,neg_graph) graph new_partial_matching =
    match extend_matching ?domain (pos_graph, neg_graph) graph new_partial_matching with
    | [] -> true (* the without pattern in not found -> OK *)
    | _ -> false

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

        | x :: tail -> Error.build "Unknown global requirement \"%s\"" x in
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
              | Some v -> false
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
  let match_in_graph ?domain ?lexicons { Pattern.global; pos; negs } graph =
    let casted_graph = G_graph.cast ?domain graph in

    if not (check_global_constraint global graph)
    then []
    else
      let pos_graph = pos.graph in

      (* get the list of partial matching for positive part of the pattern *)
      let matching_list =
        extend_matching
          ?domain
          (pos_graph,P_graph.empty)
          casted_graph
          (init ?lexicons pos) in

      let filtered_matching_list =
        List.filter
          (fun (sub, already_matched_gids) ->
             List.for_all
               (fun without ->
                  let neg_graph = without.Pattern.graph in
                  let new_partial_matching = update_partial without (sub, already_matched_gids) in
                  fulfill ?domain (pos_graph,neg_graph) graph new_partial_matching
               ) negs
          ) matching_list in
      List.map fst filtered_matching_list
end (* module Matching *)

(* ================================================================================ *)
module Rule = struct
  (* the number of rewriting steps is bounded to stop rewriting when the system is not terminating *)
  let max_rules = ref 10000
  let current_rules = ref 0

  let set_max_rules n = max_rules := n
  let reset_rules () = current_rules := 0
  let incr_rules () =
    incr current_rules;
    if !current_rules > !max_rules
    then Error.run "More than %d rewriting steps: ckeck for loops or increase max_rules value" !max_rules


  type t = {
    name: string;
    pattern: Pattern.t;
    commands: Command.t list;
    lexicons: Lexicons.t;
    loc: Loc.t;
    path: string;
  }

  let get_name t = t.name

  let get_long_name t = t.path ^ t.name

  let get_loc t = t.loc

  let to_json ?domain t =
    `Assoc
      ([
        ("rule_name", `String t.name);
        ("pattern", Pattern.basic_to_json ?domain t.pattern.pos);
        ("without", `List (List.map (Pattern.basic_to_json ?domain) t.pattern.negs));
        ("commands", `List (List.map (Command.to_json ?domain) t.commands))
      ]
      )

  (* ====================================================================== *)
  let to_dep ?domain t =
    let pos_basic = t.pattern.pos in
    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { scale = 200; }\n";

    let nodes =
      Pid_map.fold
        (fun id node acc ->
           (node, sprintf "  N_%s { word=\"%s\"; subword=\"%s\"}"
              (Pid.to_id id) (P_node.get_name node) (P_fs.to_dep (P_node.get_fs node))
           )
           :: acc
        ) pos_basic.graph [] in

    (* nodes are sorted to appear in the same order in dep picture and in input file *)
    let sorted_nodes = List.sort (fun (n1,_) (n2,_) -> P_node.compare_pos n1 n2) nodes in

    bprintf buff "[WORDS] {\n";
    List.iter
      (fun (_, dep_line) -> bprintf buff "%s\n" dep_line
      ) sorted_nodes;

    List.iteri
      (fun i cst ->
         match cst with
         | Pattern.Cst_out _ | Cst_in _ -> bprintf buff "  C_%d { word=\"*\"}\n" i
         | _ -> ()
      ) pos_basic.constraints;
    bprintf buff "}\n";

    bprintf buff "[EDGES] {\n";

    Pid_map.iter
      (fun src_pid node ->
         Massoc_pid.iter
           (fun tar_pid edge ->
              bprintf buff "  N_%s -> N_%s { label=\"%s\"}\n"
                (Pid.to_id src_pid)
                (Pid.to_id tar_pid)
                (P_edge.to_string ?domain edge)
           )
           (P_node.get_next node)
      ) pos_basic.graph;

    List.iteri
      (fun i cst ->
         match cst with
         | Pattern.Cst_out (pid, label_cst) ->
           bprintf buff "  N_%s -> C_%d {label = \"%s\"; style=dot; bottom; color=green;}\n"
             (Pid.to_id pid) i (Label_cst.to_string ?domain label_cst)
         | Cst_in (pid, label_cst) ->
           bprintf buff "  C_%d -> N_%s {label = \"%s\"; style=dot; bottom; color=green;}\n"
             i (Pid.to_id pid) (Label_cst.to_string ?domain label_cst)
         | _ -> ()
      ) pos_basic.constraints;
    bprintf buff "}\n";
    Buffer.contents buff

  (* ====================================================================== *)
  let build_commands ?domain lexicons pos pos_table ast_commands =
    let known_node_ids = Array.to_list pos_table in
    let known_edge_ids = Pattern.get_edge_ids pos in

    let rec loop (kni,kei) = function
      | [] -> []
      | ast_command :: tail ->
        let (command, (new_kni, new_kei)) =
          Command.build
            ?domain
            lexicons
            (kni,kei)
            pos_table
            ast_command in
        command :: (loop (new_kni,new_kei) tail) in
    loop (known_node_ids, known_edge_ids) ast_commands

  (* ====================================================================== *)
  let build ?domain rule_ast =
    let lexicons =
      List.fold_left (fun acc (name,lex) ->
          try
            let prev = List.assoc name acc in
            (name, (Lexicon.union prev (Lexicon.build ~loc:rule_ast.Ast.rule_loc lex))) :: (List.remove_assoc name acc)
          with Not_found -> (name, Lexicon.build ~loc:rule_ast.Ast.rule_loc lex) :: acc
        ) [] rule_ast.Ast.lexicon_info in

    let pattern = Ast.normalize_pattern rule_ast.Ast.pattern in
    let (pos, pos_table) =
      try Pattern.build_pos_basic ?domain lexicons pattern.Ast.pat_pos
      with P_fs.Fail_unif ->
        Error.build ~loc:rule_ast.Ast.rule_loc
          "[Rule.build] in rule \"%s\": feature structures declared in the \"match\" clause are inconsistent"
          rule_ast.Ast.rule_id in
    let (negs,_) =
      List.fold_left
        (fun (acc,pos) basic_ast ->
           try ((Pattern.build_neg_basic ?domain lexicons pos_table basic_ast) :: acc, pos+1)
           with P_fs.Fail_unif ->
             Error.warning ~loc:rule_ast.Ast.rule_loc "In rule \"%s\", the wihtout number %d cannot be satisfied, it is skipped"
               rule_ast.Ast.rule_id pos;
             (acc, pos+1)
        ) ([],1) pattern.Ast.pat_negs in
    {
      name = rule_ast.Ast.rule_id;
      pattern = { pos; negs; global=pattern.Ast.pat_glob; };
      commands = build_commands ?domain lexicons pos pos_table rule_ast.Ast.commands;
      loc = rule_ast.Ast.rule_loc;
      lexicons;
      path = rule_ast.Ast.rule_path;
    }













  (*  ---------------------------------------------------------------------- *)
  let onf_find cnode ?loc (matching, created_nodes) = (* TODO review args order and remove pair *)
    match cnode with
    | Command.Pat pid ->
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

  let onf_apply_command ?domain matching (command,loc) state =
    let node_find cnode = onf_find ~loc cnode (matching, state.created_nodes) in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid state.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE: the edge '%s' already exists" (G_edge.dump edge)
        | None -> state
        | Some new_graph -> {state with graph = new_graph; effective = true}
      end

    | Command.ADD_EDGE_EXPL (src_cn,tar_cn,edge_id) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (_,edge,_) =
        try String_map.find edge_id state.e_mapping
        with Not_found -> Error.run ~loc "The edge identifier '%s' is undefined" edge_id in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid state.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE_EXPL: the edge '%s' already exists" (G_edge.dump edge)
        | None -> state
        | Some new_graph -> {state with graph = new_graph; effective = true}
      end

    | Command.ADD_EDGE_ITEMS (src_cn,tar_cn,items) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let direct_items = List.map (fun (name, value) ->
          match Str.bounded_split (Str.regexp_string ".") value 2
          with
          | [edge_id; feat_name] ->
            begin
              match String_map.find_opt edge_id state.e_mapping with
              | None -> (name, value)
              | Some (_,matched_edge,_) ->
                match G_edge.get_sub_opt feat_name matched_edge with
                | Some new_value -> (name, new_value)
                | None -> Error.run "ADD_EDGE_ITEMS: no items edge feature name '%s' in matched edge '%s'" feat_name edge_id
            end
          | _ -> (name, value)
        ) items in
      let edge = G_edge.from_items direct_items in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid state.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE_ITEMS: the edge '%s' already exists" (G_edge.dump edge)
        | None -> state
        | Some new_graph -> {state with graph = new_graph; effective = true}
      end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      (match G_graph.del_edge_opt ~loc src_gid edge tar_gid state.graph with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_EDGE_EXPL: the edge '%s' does not exist" (G_edge.dump edge)
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
         {state with
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
            | Some (new_graph, new_edge) ->
              {state with
               graph = new_graph;
               effective = true;
               e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) state.e_mapping;
              }
          end
      end

    | Command.UPDATE_EDGE_FEAT (edge_ident, feat_name, new_value) ->
      let (src_gid,edge,tar_gid) =
        try String_map.find edge_ident state.e_mapping
        with Not_found -> Error.run ~loc "The edge identifier '%s' is undefined" edge_ident in
      (match G_graph.update_edge_feature_opt ~loc edge_ident feat_name new_value (src_gid,edge,tar_gid) state.graph with
       | None when !Global.safe_commands -> Error.run ~loc "UPDATE_EDGE_FEAT: no changes %s" edge_ident
       | None -> state
       | Some (new_graph, new_edge) ->
         { state with
           graph = new_graph;
           effective = true;
           e_mapping = String_map.add edge_ident (src_gid,new_edge,tar_gid) state.e_mapping;
         }
      )

    | Command.DEL_NODE node_cn ->
      let node_gid = node_find node_cn in
      (match G_graph.del_node_opt node_gid state.graph with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_NODE: the node does not exist"
       | None -> state
       | Some new_graph ->
         { state with
           graph = new_graph;
           effective = true;
           e_mapping = String_map.filter (fun _ (s,_,t) -> s=node_gid || t=node_gid) state.e_mapping;
         }
      )

    | Command.UPDATE_FEAT (tar_cn, tar_feat_name, item_list) ->
      let tar_gid = node_find tar_cn in
      let rule_items = List.map
          (function
            | Command.Feat (cnode, feat_name) -> Concat_item.Feat (node_find cnode, feat_name)
            | Command.String s -> Concat_item.String s
            | Command.Lexical_field (lex_name, field) ->
              (try
                 let lexicon = List.assoc lex_name matching.l_param in
                 let v = Lexicon.get field lexicon in
                 Concat_item.String v
               with
               | Not_found -> Error.run ~loc "UPDATE_FEAT: the lexicon '%s' does not exist" lex_name
              )
          ) item_list in
      let (new_graph, new_feature_value) =
        G_graph.update_feat ~loc state.graph tar_gid tar_feat_name rule_items in
      {state with graph = new_graph; effective = true}

    | Command.APPEND_FEATS (src_cn, tar_cn, regexp, separator) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      (match G_graph.append_feats_opt state.graph src_gid tar_gid separator regexp with
       | None when !Global.safe_commands -> Error.run ~loc "APPEND_FEATS uneffective"
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
      let (new_graph, de, ae) = G_graph.shift_in loc src_gid tar_gid (Matching.test_locality matching state.created_nodes) label_cst state.graph in
      {state with graph = new_graph; effective = state.effective || de <> [] || ae <> []}

    | Command.SHIFT_OUT (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, de, ae) = G_graph.shift_out loc src_gid tar_gid (Matching.test_locality matching state.created_nodes) label_cst state.graph in
      {state with graph = new_graph; effective = state.effective || de <> [] || ae <> []}

    | Command.SHIFT_EDGE (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, de, ae) = G_graph.shift_edges loc src_gid tar_gid (Matching.test_locality matching state.created_nodes) label_cst state.graph in
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


  let onf_apply_opt ?domain rule graph =
    let {Pattern.pos; negs} = rule.pattern in
    (* get the list of partial matching for positive part of the pattern *)
    let matching_list =
      Matching.extend_matching
        ?domain
        (pos.graph,P_graph.empty)
        graph
        (Matching.init ~lexicons:rule.lexicons pos) in
    match List.find_opt
            (fun (sub, already_matched_gids) ->
               List.for_all
                 (fun neg ->
                    let new_partial_matching = Matching.update_partial neg (sub, already_matched_gids) in
                    Matching.fulfill ?domain (pos.graph,neg.graph) graph new_partial_matching
                 ) negs
            ) matching_list with
    | None -> None
    | Some (first_matching_where_all_witout_are_fulfilled,_) ->
      let final_state =
        List.fold_left
          (fun state command -> onf_apply_command ?domain first_matching_where_all_witout_are_fulfilled command state)
          { graph;
            created_nodes = [];
            effective = false;
            e_mapping = first_matching_where_all_witout_are_fulfilled.e_match;
          }
          rule.commands in
      if final_state.effective
      then (Timeout.check (); incr_rules(); Some (G_graph.push_rule (get_long_name rule) final_state.graph))
      else None

  let rec wrd_apply_opt ?domain rule (graph, big_step_opt) =
    let {Pattern.pos; negs} = rule.pattern in
    (* get the list of partial matching for positive part of the pattern *)
    let matching_list =
      Matching.extend_matching
        ?domain
        (pos.graph,P_graph.empty)
        graph
        (Matching.init ~lexicons:rule.lexicons pos) in
    match List.find_opt
            (fun (sub, already_matched_gids) ->
               List.for_all
                 (fun neg ->
                    let new_partial_matching = Matching.update_partial neg (sub, already_matched_gids) in
                    Matching.fulfill ?domain (pos.graph,neg.graph) graph new_partial_matching
                 ) negs
            ) matching_list with
    | None -> None
    | Some (first_matching_where_all_witout_are_fulfilled,_) ->
      let final_state =
        List.fold_left
          (fun state command -> onf_apply_command ?domain first_matching_where_all_witout_are_fulfilled command state)
          { graph;
            created_nodes = [];
            effective = false;
            e_mapping = first_matching_where_all_witout_are_fulfilled.e_match;
          }
          rule.commands in

      let rule_app = {
        Libgrew_types.rule_name = rule.name;
        up = Matching.match_deco rule.pattern first_matching_where_all_witout_are_fulfilled;
        down = Matching.down_deco (first_matching_where_all_witout_are_fulfilled,final_state.created_nodes) rule.commands
      } in

      let new_big_step = match big_step_opt with
        | None -> {Libgrew_types.small_step = []; first=rule_app}
        | Some {Libgrew_types.small_step; first} -> {Libgrew_types.small_step = (graph,rule_app) :: small_step; first} in

      if final_state.effective
      then (Timeout.check (); incr_rules(); Some (final_state.graph, new_big_step))
      else None









  let find cnode ?loc gwh matching =
    match cnode with
    | Command.Pat pid ->
      (try Pid_map.find pid matching.Matching.n_match
       with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name -> List.assoc name gwh.Graph_with_history.added_gids

  let gwh_apply_command ?domain (command,loc) matching gwh =
    let node_find cnode = find ~loc cnode gwh matching in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid gwh.Graph_with_history.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE: the edge '%s' already exists" (G_edge.dump edge)
        | None -> Graph_with_history_set.singleton gwh
        | Some new_graph ->
          Graph_with_history_set.singleton
            {gwh with
             Graph_with_history.graph = new_graph;
             delta = Delta.add_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
            }
      end

    | Command.ADD_EDGE_EXPL (src_cn,tar_cn,edge_ident) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (_,edge,_) =
        try String_map.find edge_ident gwh.e_mapping
        with Not_found -> Error.run ~loc "ADD_EDGE_EXPL: the edge identifier '%s' is undefined" edge_ident in

      begin
        match G_graph.add_edge_opt src_gid edge tar_gid gwh.Graph_with_history.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE_EXPL: the edge '%s' already exists" (G_edge.dump edge)
        | None -> Graph_with_history_set.singleton gwh
        | Some new_graph -> Graph_with_history_set.singleton
                              {gwh with
                               Graph_with_history.graph = new_graph;
                               delta = Delta.add_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
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
              | None -> (name, value)
              | Some (_,matched_edge,_) ->
                match G_edge.get_sub_opt feat_name matched_edge with
                | Some new_value -> (name, new_value)
                | None -> Error.run "ADD_EDGE_ITEMS: no items edge feature name '%s' in matched edge '%s'" feat_name edge_id
            end
          | _ -> (name, value)
        ) items in
      let edge = G_edge.from_items direct_items in
      begin
        match G_graph.add_edge_opt src_gid edge tar_gid gwh.Graph_with_history.graph with
        | None when !Global.safe_commands ->
          Error.run ~loc "ADD_EDGE_ITEMS: the edge '%s' already exists" (G_edge.dump edge)
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
         Error.run ~loc "DEL_EDGE_EXPL: the edge '%s' does not exist" (G_edge.dump edge)
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
                               e_mapping = String_map.filter (fun _ (s,_,t) -> s=node_gid || t=node_gid) gwh.e_mapping;
                             }
      )

    | Command.UPDATE_FEAT (tar_cn, tar_feat_name, item_list) ->
      let tar_gid = node_find tar_cn in

      (* a list of possible rule_items is produced: there can be more than one in case of non functional lexicons *)
      let rule_items_list =
        List.fold_right
          (fun item acc -> match item with
             | Command.Feat (cnode, feat_name) -> List.map (fun x -> Concat_item.Feat (node_find cnode, feat_name)::x) acc
             | Command.String s -> List.map (fun x -> (Concat_item.String s) :: x) acc
             | Command.Lexical_field (lex_name, field) ->
               try
                 let lexicon = List.assoc lex_name matching.l_param in
                 let values = Lexicon.read_all field lexicon in
                 List.fold_left
                   (fun acc2 value ->
                      (List.map (fun x -> (Concat_item.String value) :: x) acc) @ acc2
                   ) [] values
               with
               | Not_found -> Error.run ~loc "UPDATE_FEAT: the lexicon '%s' does not exist" lex_name
               | Lexicon.Not_functional_lexicon -> Error.run ~loc "UPDATE_FEAT: the lexicon is not functional" lex_name
          ) item_list [[]] in

      let new_graphs = List.fold_left
          (fun acc rule_items ->
             let (new_graph, new_feature_value) = (* TODO: take value type into account in update_feat *)
               G_graph.update_feat ~loc gwh.Graph_with_history.graph tar_gid tar_feat_name rule_items in
             let new_value =
               if Domain.is_num ?domain tar_feat_name
               then Float (float_of_string new_feature_value)
               else String new_feature_value in
             Graph_with_history_set.add
               { gwh with
                 Graph_with_history.graph = new_graph;
                 delta = Delta.set_feat gwh.Graph_with_history.seed tar_gid tar_feat_name (Some new_value) gwh.Graph_with_history.delta;
               }
               acc
          ) Graph_with_history_set.empty rule_items_list in
      new_graphs

    | Command.DEL_FEAT (tar_cn,feat_name) ->
      let tar_gid = node_find tar_cn in
      (match G_graph.del_feat_opt gwh.Graph_with_history.graph tar_gid feat_name with
       | None when !Global.safe_commands -> Error.run ~loc "DEL_FEAT: the feat does not exist"
       | None -> Graph_with_history_set.singleton gwh
       | Some new_graph -> Graph_with_history_set.singleton { gwh with
                                                              Graph_with_history.graph = new_graph;
                                                              delta = Delta.set_feat gwh.Graph_with_history.seed tar_gid feat_name None gwh.Graph_with_history.delta;
                                                            }
      )

    | Command.UPDATE_EDGE_FEAT (edge_id, feat_name, new_value) ->
      let (src_gid,old_edge,tar_gid) =
        try String_map.find edge_id gwh.e_mapping
        with Not_found -> Error.run ~loc "The edge identifier '%s' is undefined" edge_id in
      (match G_graph.update_edge_feature_opt ~loc edge_id feat_name new_value (src_gid,old_edge,tar_gid) gwh.Graph_with_history.graph with
       | None when !Global.safe_commands -> Error.run ~loc "UPDATE_EDGE_FEAT: no changes %s" edge_id
       | None -> Graph_with_history_set.singleton gwh
       | Some (new_graph, new_edge) ->
         Graph_with_history_set.singleton
           { gwh with
             Graph_with_history.graph = new_graph;
             delta = gwh.Graph_with_history.delta
                     |> Delta.del_edge src_gid old_edge tar_gid
                     |> Delta.add_edge src_gid new_edge tar_gid;
             e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) gwh.e_mapping;
           }
      )

    | Command.SHIFT_IN (src_cn,tar_cn,label_cst) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      let (new_graph, del_edges, add_edges) =
        G_graph.shift_in loc src_gid tar_gid (Matching.test_locality matching gwh.added_gids_in_rule) label_cst gwh.Graph_with_history.graph in
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
        G_graph.shift_out loc src_gid tar_gid (Matching.test_locality matching gwh.added_gids_in_rule) label_cst gwh.Graph_with_history.graph in
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
        G_graph.shift_edges loc src_gid tar_gid (Matching.test_locality matching gwh.added_gids_in_rule) label_cst gwh.Graph_with_history.graph in
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
            | Some (new_graph, new_edge) -> Graph_with_history_set.singleton
                                              {gwh with
                                               Graph_with_history.graph = new_graph;
                                               e_mapping = String_map.add edge_id (src_gid,new_edge,tar_gid) gwh.e_mapping;
                                              }
          end
      end

    | Command.APPEND_FEATS (src_cn, tar_cn, regexp, separator) ->
      let src_gid = node_find src_cn in
      let tar_gid = node_find tar_cn in
      begin
        match G_graph.append_feats_opt gwh.Graph_with_history.graph src_gid tar_gid separator regexp with
        | None when !Global.safe_commands -> Error.run ~loc "APPEND_FEATS uneffective"
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



  (*  ---------------------------------------------------------------------- *)
  (** [apply_rule graph_with_history matching rule] returns a new graph_with_history after the application of the rule *)
  let gwh_apply_rule ?domain graph_with_history matching rule =
    Timeout.check (); incr_rules ();
    let init = Graph_with_history_set.singleton
        { graph_with_history with
          e_mapping = matching.Matching.e_match;
          added_gids_in_rule = [];
        } in

    List.fold_left
      (fun gwh_set cmd ->
         Graph_with_history_set.fold
           (fun gwh acc ->
              Graph_with_history_set.union (gwh_apply_command ?domain cmd matching gwh) acc
           ) gwh_set Graph_with_history_set.empty
      ) init rule.commands

  let gwh_apply ?domain rule graph_with_history =
    let matching_list = Matching.match_in_graph ?domain ~lexicons:rule.lexicons rule.pattern graph_with_history.Graph_with_history.graph in
    List.fold_left
      (fun acc matching ->
         Graph_with_history_set.union (gwh_apply_rule ?domain graph_with_history matching rule) acc
      ) Graph_with_history_set.empty matching_list

  exception Dead_lock
  let owh_apply_opt ?domain rule gwh =
    let graph = gwh.Graph_with_history.graph in
    let {Pattern.pos; negs} = rule.pattern in

    (* get the list of matching for positive part of the pattern *)
    let matching_list = Matching.extend_matching ?domain (pos.graph,P_graph.empty) graph (Matching.init ~lexicons:rule.lexicons pos) in

    let rec loop_matching = function
      | [] -> None
      | (sub, already_matched_gids) :: tail ->
        if List.for_all
            (fun neg ->
               let new_partial_matching = Matching.update_partial neg (sub, already_matched_gids) in
               Matching.fulfill ?domain (pos.graph,neg.graph) graph new_partial_matching
            ) negs
        then (* all negs part are fulfilled *)
          let init_gwh = { gwh with e_mapping = sub.Matching.e_match; added_gids_in_rule = []; } in
          let rec loop_command acc_gwh = function
            | [] -> acc_gwh
            | command :: tail_commands ->
              let set = gwh_apply_command ?domain command sub acc_gwh in
              match Graph_with_history_set.choose_opt set with
              | None -> raise Dead_lock
              | Some next_gwh -> loop_command next_gwh tail_commands in
          try
            let new_gwh = loop_command init_gwh rule.commands in
            Timeout.check (); incr_rules();
            Some {new_gwh with graph = G_graph.push_rule (get_long_name rule) new_gwh.graph }
          with Dead_lock -> loop_matching tail (* failed to apply all commands -> move to the next matching *)
        else loop_matching tail (* some neg part prevents rule app -> move to the next matching *)
    in loop_matching matching_list
end (* module Rule *)
