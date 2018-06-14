(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
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

(* ================================================================================ *)
module Instance = struct
  type t = {
    graph: G_graph.t;
    history: Command.h list;
    rules: string list;
    big_step: Libgrew_types.big_step option;
  }

  let swap t =
    match t.big_step with
    | None -> t
    | Some bs -> {t with big_step = Some (Libgrew_types.swap bs) }

  let empty = {graph = G_graph.empty; rules=[]; history=[]; big_step=None; }

  let from_graph graph = {empty with graph}

  let rev_steps t =
    { t with big_step = match t.big_step with
      | None -> None
      | Some bs -> Some {bs with Libgrew_types.small_step = List.rev bs.Libgrew_types.small_step }
    }

  let refresh t = { empty with graph=t.graph }

  (* comparison is done on the list of commands *)
  (* only graph rewritten from the same init graph can be "compared" *)
  let compare t1 t2 = Pervasives.compare t1.history t2.history

  let to_gr t = G_graph.to_gr t.graph

  let to_conll_string t = G_graph.to_conll_string t.graph

  let save_dot_png ?filter ?main_feat base t =
    ignore (Dot.to_png_file (G_graph.to_dot ?main_feat t.graph) (base^".png"))
end (* module Instance *)

(* ================================================================================ *)
module Instance_set = Set.Make (Instance)

(* ================================================================================ *)
module Rule = struct

  (* the rewriting depth is bounded to stop rewriting when the system is not terminating *)
  let max_depth_det = ref 2000
  let max_depth_non_det = ref 100
  let debug_loop = ref false

  let set_max_depth_det value = max_depth_det := value
  let set_max_depth_non_det value = max_depth_non_det := value
  let set_debug_loop () = debug_loop := true

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
    | Filter of Pid.t * P_fs.t (* used when a without impose a fs on a node defined by the match basic *)
    (* *)
    | Immediate_prec of Pid.t * Pid.t
    | Large_prec of Pid.t * Pid.t

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
  | Filter (pid, p_fs) ->
    `Assoc ["filter",
      `Assoc [
        ("id", `String (Pid.to_string pid));
        ("fs", P_fs.to_json ?domain p_fs);
      ]
    ]
  | Immediate_prec (pid1, pid2) ->
    `Assoc ["immediate_prec",
      `Assoc [
        ("id1", `String (Pid.to_string pid1));
        ("id2", `String (Pid.to_string pid2));
      ]
    ]
  | Large_prec (pid1, pid2) ->
    `Assoc ["large_prec",
      `Assoc [
        ("id1", `String (Pid.to_string pid1));
        ("id2", `String (Pid.to_string pid2));
      ]
    ]

  let build_pos_constraint ?domain ?lexicons pos_table const =
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

      | (Ast.Features_ineq (ineq, (node_name1, feat_name1), (node_name2, feat_name2)), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name1;
        Domain.check_feature_name ?domain ~loc feat_name2;
        Features_ineq (ineq, pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

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

      | (Ast.Feature_eq_lex ((node_name, feat_name), lf), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_eq_lex (pid_of_name loc node_name, feat_name, lf)
      | (Ast.Feature_diff_lex ((node_name, feat_name), lf), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_diff_lex (pid_of_name loc node_name, feat_name, lf)

      | (Ast.Feature_eq_float ((node_name, feat_name), float), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_eq_float (pid_of_name loc node_name, feat_name, float)
      | (Ast.Feature_diff_float ((node_name, feat_name), float), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_diff_float (pid_of_name loc node_name, feat_name, float)

      | (Ast.Immediate_prec (id1, id2), loc) ->
        Immediate_prec (pid_of_name loc id1, pid_of_name loc id2)

      | (Ast.Large_prec (id1, id2), loc) ->
        Large_prec (pid_of_name loc id1, pid_of_name loc id2)

      | (Ast.Feature_eq_lex_or_fs (s1,s2), loc) -> failwith "TODO"
      | (Ast.Feature_diff_lex_or_fs (s1,s2), loc) -> failwith "TODO"


  type basic = {
    graph: P_graph.t;
    constraints: const list;
  }

  let basic_to_json ?domain basic =
    `Assoc [
      ("graph", P_graph.to_json ?domain basic.graph);
      ("constraints", `List (List.map (const_to_json ?domain) basic.constraints));
    ]

  let build_pos_basic ?domain ?lexicons ?pat_vars basic_ast =
    let (graph, pos_table) =
      P_graph.build ?domain ?pat_vars basic_ast.Ast.pat_nodes basic_ast.Ast.pat_edges in
    (
      {
        graph = graph;
        constraints = List.map (build_pos_constraint ?domain ?lexicons pos_table) basic_ast.Ast.pat_const
      },
      pos_table
    )

  (* the neg part *)
  let build_neg_constraint ?domain pos_table neg_table const =
    let pid_of_name loc node_name =
      match Id.build_opt node_name pos_table with
        | Some i -> Pid.Pos i
        | None -> Pid.Neg (Id.build ~loc node_name neg_table) in
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

      | (Ast.Features_ineq (ineq, feat_id1, feat_id2), loc) ->
        let (node_name1, feat_name1) = feat_id1
        and (node_name2, feat_name2) = feat_id2 in
        Domain.check_feature_name ?domain ~loc feat_name1;
        Domain.check_feature_name ?domain ~loc feat_name2;
        Features_ineq (ineq, pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

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

      | (Ast.Feature_eq_lex ((node_name, feat_name), lf), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_eq_lex (pid_of_name loc node_name, feat_name, lf)
      | (Ast.Feature_diff_lex ((node_name, feat_name), lf), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_diff_lex (pid_of_name loc node_name, feat_name, lf)

      | (Ast.Feature_eq_float ((node_name, feat_name), float), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_eq_float (pid_of_name loc node_name, feat_name, float)
      | (Ast.Feature_diff_float ((node_name, feat_name), float), loc) ->
        Domain.check_feature_name ?domain ~loc feat_name;
        Feature_diff_float (pid_of_name loc node_name, feat_name, float)

      | (Ast.Immediate_prec (id1, id2), loc) ->
        Immediate_prec (pid_of_name loc id1, pid_of_name loc id2)

      | (Ast.Large_prec (id1, id2), loc) ->
        Large_prec (pid_of_name loc id1, pid_of_name loc id2)

      | (Ast.Feature_eq_lex_or_fs (s1,s2), loc) -> failwith "TODO"
      | (Ast.Feature_diff_lex_or_fs (s1,s2), loc) -> failwith "TODO"


  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_neg_basic ?domain ?pat_vars pos_table basic_ast =
    let (extension, neg_table) =
      P_graph.build_extension ?domain ?pat_vars pos_table basic_ast.Ast.pat_nodes basic_ast.Ast.pat_edges in

    let filters = Pid_map.fold (fun id node acc -> Filter (id, P_node.get_fs node) :: acc) extension.P_graph.old_map [] in
    {
      graph = extension.P_graph.ext_map;
      constraints = filters @ List.map (build_neg_constraint ?domain pos_table neg_table) basic_ast.Ast.pat_const ;
    }

  let get_edge_ids basic =
    Pid_map.fold
      (fun _ node acc ->
        Massoc_pid.fold
          (fun acc2 _ edge -> (P_edge.get_id edge)::acc2)
          acc (P_node.get_next node)
      ) basic.graph []

  (* a [pattern] is described by the positive basic and a list of negative basics. *)
  type pattern = basic * basic list

  let pid_name_list (pos,_) = P_graph.pid_name_list pos.graph

  type t = {
      name: string;
      pattern: pattern;
      commands: Command.t list;
      param: Lex_par.t * string list; (* ([],[]) if None *)
      lexicons: Lexicons.t;
      loc: Loc.t;
    }

  let get_name t = t.name

  let get_loc t = t.loc

  let to_json ?domain t =
    let param_json = match t.param with
    | ([],[]) -> []
    | (lex_par, param_names) -> [
      ("pattern_param", `List (List.map (fun x -> `String x) (param_names)));
      ("lex_par", Lex_par.to_json lex_par);
    ] in
    `Assoc
    ([
      ("rule_name", `String t.name);
      ("match", basic_to_json ?domain (fst t.pattern));
      ("without", `List (List.map (basic_to_json ?domain) (snd t.pattern)));
      ("commands", `List (List.map (Command.to_json ?domain) t.commands))
    ] @ param_json
    )

  (* ====================================================================== *)
  let to_dep ?domain t =
    let pos_basic = fst t.pattern in
    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { scale = 200; }\n";

    let nodes =
      Pid_map.fold
        (fun id node acc ->
          (node, sprintf "  N_%s { word=\"%s\"; subword=\"%s\"}"
            (Pid.to_id id) (P_node.get_name node) (P_fs.to_dep (snd t.param) (P_node.get_fs node))
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
          | Cst_out _ | Cst_in _ -> bprintf buff "  C_%d { word=\"*\"}\n" i
          | _ -> ()
      ) pos_basic.constraints;
    bprintf buff "}\n";

    bprintf buff "[EDGES] {\n";

    Pid_map.iter
      (fun id_src node ->
        Massoc_pid.iter
          (fun id_tar edge ->
            bprintf buff "  N_%s -> N_%s { label=\"%s\"}\n"
              (Pid.to_id id_src)
              (Pid.to_id id_tar)
              (P_edge.to_string ?domain edge)
          )
          (P_node.get_next node)
      ) pos_basic.graph;

    List.iteri
      (fun i cst ->
        match cst with
          | Cst_out (pid, label_cst) ->
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
  let build_commands ?domain ?param lexicon_names pos pos_table ast_commands =
    let known_node_ids = Array.to_list pos_table in
    let known_edge_ids = get_edge_ids pos in

    let rec loop (kni,kei) = function
      | [] -> []
      | ast_command :: tail ->
          let (command, (new_kni, new_kei)) =
            Command.build
              ?domain
              ?param
              lexicon_names
              (kni,kei)
              pos_table
              ast_command in
          command :: (loop (new_kni,new_kei) tail) in
    loop (known_node_ids, known_edge_ids) ast_commands

  let build_lex loc = function
  | Ast.File filename -> Lexicon.load filename
  | Ast.Final (line_list) -> Lexicon.build loc line_list


  (* ====================================================================== *)
  let build ?domain deprecated_dir rule_ast =

    let dir = match rule_ast.Ast.rule_dir with
    | Some d -> d
    | None -> deprecated_dir in

    let lexicons = List.fold_left (fun acc (name,lex) ->
        try
          let prev = List.assoc name acc in
          (name, (Lexicon.union prev (build_lex rule_ast.Ast.rule_loc lex))) :: (List.remove_assoc name acc)
        with
          Not_found -> (name, build_lex rule_ast.Ast.rule_loc lex) :: acc
      ) [] rule_ast.Ast.lexicon_info in

    let lexicon_names = List.map fst lexicons in

    let (param, pat_vars) =
      match rule_ast.Ast.param with
      | None -> ([],[])
      | Some (files,vars) ->
          let nb_var = List.length vars in

          (* first: load lexical parameters given in the same file at the end of the rule definition *)
          let local_param = match rule_ast.Ast.lex_par with
          | None -> []
          | Some lines -> Lex_par.from_lines ~loc:rule_ast.Ast.rule_loc nb_var lines in

          (* second: load lexical parameters given in external files *)
          let full_param = List.fold_left
            (fun acc file ->
              match acc with
              | [] -> Lex_par.load ~loc:rule_ast.Ast.rule_loc dir nb_var file
              | lp -> Lex_par.append (Lex_par.load ~loc:rule_ast.Ast.rule_loc dir nb_var file) lp
            ) local_param files in

          (full_param, vars) in

    (match (param, pat_vars) with
      | ([], _::_) -> Error.build ~loc:rule_ast.Ast.rule_loc "[Rule.build] Missing lexical parameters in rule \"%s\"" rule_ast.Ast.rule_id
      | _ -> ()
    );

    let pattern = Ast.normalize_pattern rule_ast.Ast.pattern in
    let (pos, pos_table) =
      try build_pos_basic ?domain ~lexicons:lexicons ~pat_vars pattern.Ast.pat_pos
      with P_fs.Fail_unif ->
        Error.build ~loc:rule_ast.Ast.rule_loc
          "[Rule.build] in rule \"%s\": feature structures declared in the \"match\" clause are inconsistent"
          rule_ast.Ast.rule_id in
    let (negs,_) =
      List.fold_left
      (fun (acc,pos) basic_ast ->
        try ((build_neg_basic ?domain ~pat_vars pos_table basic_ast) :: acc, pos+1)
        with P_fs.Fail_unif ->
          Log.fwarning "In rule \"%s\" [%s], the wihtout number %d cannot be satisfied, it is skipped"
            rule_ast.Ast.rule_id (Loc.to_string rule_ast.Ast.rule_loc) pos;
          (acc, pos+1)
      ) ([],1) pattern.Ast.pat_negs in
    {
      name = rule_ast.Ast.rule_id;
      pattern = (pos, negs);
      commands = build_commands ?domain ~param:pat_vars lexicon_names pos pos_table rule_ast.Ast.commands;
      loc = rule_ast.Ast.rule_loc;
      lexicons;
      param = (param, pat_vars);
    }

  let build_pattern ?domain ?lexicons pattern_ast =
    let n_pattern = Ast.normalize_pattern pattern_ast in
    let (pos, pos_table) =
      try build_pos_basic ?domain ?lexicons n_pattern.Ast.pat_pos
      with P_fs.Fail_unif -> Error.build "feature structures declared in the \"match\" clause are inconsistent " in
    let negs =
      List_.try_map
        P_fs.Fail_unif (* Skip the without parts that are incompatible with the match part *)
        (fun basic_ast -> build_neg_basic ?domain pos_table basic_ast)
        n_pattern.Ast.pat_negs in
    (pos, negs)

  (* ====================================================================== *)
  type matching = {
      n_match: Gid.t Pid_map.t;                     (* partial fct: pattern nodes |--> graph nodes *)
      e_match: (string*(Gid.t*Label.t*Gid.t)) list; (* edge matching: edge ident  |--> (src,label,tar) *)
      m_param: Lex_par.t option;
      l_param: Lexicons.t;

    }

  let to_python pattern graph m =
    let node_name gid = G_node.get_name gid (G_graph.find gid graph) in
    let nodes = Pid_map.fold (fun pid gid acc ->
      let pnode = P_graph.find pid (fst pattern).graph in
        (P_node.get_name pnode, `String (node_name gid))::acc
      ) m.n_match [] in
    let edges = List.map (fun (id, (src,lab,tar)) ->
      (id, `String (sprintf "%s/%s/%s" (node_name src) (Label.to_string lab) (node_name tar)))
      ) m.e_match in
    `Assoc ( nodes @ edges)

  let node_matching pattern graph { n_match } =
    Pid_map.fold
      (fun pid gid acc ->
        let pnode = P_graph.find pid (fst pattern).graph in
        let gnode = G_graph.find gid graph in
        (P_node.get_name pnode, G_node.get_float gnode) :: acc
      ) n_match []

  let empty_matching ?(lexicons=[]) param = { n_match = Pid_map.empty; e_match = []; m_param = param; l_param = lexicons;}

  let e_comp (e1,_) (e2,_) = compare e1 e2

  let e_match_add ?pos edge_id matching =
    match List_.usort_insert ~compare:e_comp edge_id matching.e_match with
    | Some new_e_match -> { matching with e_match = new_e_match }
    | None -> Error.bug "The edge identifier '%s' is binded twice in the same pattern" (fst edge_id)

  let match_deco pattern matching =
    { G_deco.nodes =
        Pid_map.fold
          (fun pid gid acc ->
            let pnode = P_graph.find pid (fst pattern).graph in
            (gid, (P_node.get_name pnode, P_fs.feat_list (P_node.get_fs pnode))) ::acc
          ) matching.n_match [];
      G_deco.edges = List.fold_left (fun acc (_,edge) -> edge::acc) [] matching.e_match;
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
     G_deco.nodes = List.map (fun (gid,feat_list) -> (gid, ("",feat_list))) (Gid_map.bindings feat_to_highlight);
     G_deco.edges = List.fold_left
       (fun acc -> function
         | (Command.ADD_EDGE (src_cn,tar_cn,edge),loc) ->
             (find src_cn (matching, created_nodes), edge, find tar_cn (matching, created_nodes)) :: acc
         | _ -> acc
       ) [] commands
   }

  exception Fail
  type partial = {
      sub: matching;
      unmatched_nodes: Pid.t list;
      unmatched_edges: (Pid.t * P_edge.t * Pid.t) list;
      already_matched_gids: Gid.t list; (* to ensure injectivity *)
      check: const list (* constraints to verify at the end of the matching *)
    }

        (* PREREQUISITES:
           - all partial matching have the same ?domain
           - the ?domain of the pattern P is the disjoint union of ?domain([sub]) and [unmatched_nodes]
         *)
  (*  ---------------------------------------------------------------------- *)
  let init ?lexicons param basic =
    let roots = P_graph.roots basic.graph in

    let node_list = Pid_map.fold (fun pid _ acc -> pid::acc) basic.graph [] in

    (* put all roots in the front of the list to speed up the algo *)
    let sorted_node_list =
      List.sort
        (fun n1 n2 -> match (List.mem n1 roots, List.mem n2 roots) with
        | true, false -> -1
        | false, true -> 1
        | _ -> 0) node_list in

    { sub = empty_matching ?lexicons param;
      unmatched_nodes = sorted_node_list;
      unmatched_edges = [];
      already_matched_gids = [];
      check = basic.constraints;
    }

  (*  ---------------------------------------------------------------------- *)
  let apply_cst ?domain graph matching cst =
    let get_node pid = G_graph.find (Pid_map.find pid matching.n_match) graph in
    let get_string_feat pid = function
      | "position" ->
        begin
          match G_node.get_position (get_node pid) with
          | G_node.Ordered f -> Some (sprintf "%g" f)
          | _ -> Error.run "Cannot read position of an unordered node"
        end
      | feat_name -> G_fs.get_string_atom feat_name (G_node.get_fs (get_node pid)) in
    let get_float_feat pid = function
      | "position" ->
        begin
          match G_node.get_position (get_node pid) with
          | G_node.Ordered f -> Some f
          | _ -> Error.run "Cannot read position of an unordered node"
        end
      | feat_name -> G_fs.get_float_feat feat_name (G_node.get_fs (get_node pid)) in

    match cst with
      | Cst_out (pid,label_cst) ->
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
          match (get_string_feat pid1 feat_name1, get_string_feat pid2 feat_name2) with
            | Some fv1, Some fv2 when fv1 = fv2 -> matching
            | _ -> raise Fail
        end
      | Feature_eq_cst (pid1, feat_name1, value) ->
        begin
          match get_string_feat pid1 feat_name1 with
            | Some fv1 when fv1 = value -> matching
            | _ -> raise Fail
        end
      | Feature_diff_cst (pid1, feat_name1, value) ->
        begin
          match get_string_feat pid1 feat_name1 with
            | Some fv1 when fv1 <> value -> matching
            | _ -> raise Fail
        end
      | Feature_eq_float (pid1, feat_name1, float) ->
        begin
          match get_float_feat pid1 feat_name1 with
            | Some fv1 when fv1 = float -> matching
            | _ -> raise Fail
        end
      | Feature_diff_float (pid1, feat_name1, float) ->
        begin
          match get_float_feat pid1 feat_name1 with
            | Some fv1 when fv1 <> float -> matching
            | _ -> raise Fail
        end
      | Features_diseq (pid1, feat_name1, pid2, feat_name2) ->
        begin
          match (get_string_feat pid1 feat_name1, get_string_feat pid2 feat_name2) with
            | Some fv1, Some fv2 when fv1 <> fv2 -> matching
            | _ -> raise Fail
        end
      | Features_ineq (ineq, pid1, feat_name1, pid2, feat_name2) ->
        begin
          match (ineq, get_float_feat pid1 feat_name1, get_float_feat pid2 feat_name2) with
            | (Ast.Lt, Some fv1, Some fv2) when fv1 < fv2 -> matching
            | (Ast.Gt, Some fv1, Some fv2) when fv1 > fv2 -> matching
            | (Ast.Le, Some fv1, Some fv2) when fv1 <= fv2 -> matching
            | (Ast.Ge, Some fv1, Some fv2) when fv1 >= fv2 -> matching
            | _ -> raise Fail
          end
      | Feature_ineq_cst (ineq, pid1, feat_name1, constant) ->
        begin
          match (ineq, get_float_feat pid1 feat_name1) with
            | (Ast.Lt, Some fv1) when fv1 < constant -> matching
            | (Ast.Gt, Some fv1) when fv1 > constant -> matching
            | (Ast.Le, Some fv1) when fv1 <= constant -> matching
            | (Ast.Ge, Some fv1) when fv1 >= constant -> matching
            | _ -> raise Fail
          end
      | Feature_eq_regexp (pid, feat_name, regexp) ->
        begin
          match get_string_feat pid feat_name with
          | None -> raise Fail
          | Some string_feat ->
            let re = Str.regexp regexp in
            if String_.re_match re string_feat then matching else raise Fail
        end
      | Immediate_prec (pid1, pid2) ->
          let gid1 = Pid_map.find pid1 matching.n_match in
          let gid2 = Pid_map.find pid2 matching.n_match in
          let gnode1 = G_graph.find gid1 graph in
          if G_node.get_succ gnode1 = Some gid2
          then matching
          else  raise Fail
      | Large_prec (pid1, pid2) ->
          let gnode1 = G_graph.find (Pid_map.find pid1 matching.n_match) graph in
          let gnode2 = G_graph.find (Pid_map.find pid2 matching.n_match) graph in
          if G_node.get_position gnode1 < G_node.get_position gnode2
          then matching
          else raise Fail
      | Feature_eq_lex (pid, feature_name, (lexicon,field)) ->
        begin
          Printf.printf "### Feature_eq_lex\n%!";
          match get_string_feat pid feature_name with
          | None -> raise Fail
          | Some v ->
              let old_lex = List.assoc lexicon matching.l_param in
              match Lexicon.select field v old_lex with
              | None -> raise Fail
              | Some new_lex -> {matching with l_param = (lexicon, new_lex) :: (List.remove_assoc lexicon matching.l_param) }
        end

      | Feature_diff_lex (pid, feature_name, (lexicon,field)) ->
        begin
          match get_string_feat pid feature_name with
          | None -> raise Fail
          | Some v ->
              let old_lex = List.assoc lexicon matching.l_param in
              match Lexicon.unselect field v old_lex with
              | None -> raise Fail
              | Some new_lex -> {matching with l_param = (lexicon, new_lex) :: (List.remove_assoc lexicon matching.l_param) }
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
              | P_edge.Binds (id,labels) -> (* n edges in the graph match the identified p_edge -> make copies of the [k] matchings (and returns n*k matchings) *)
                  List.map
                    (fun label ->
                      {partial with sub = e_match_add (id,(src_gid,label,tar_gid)) partial.sub; unmatched_edges = tail_ue }
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
                  | P_edge.Binds (id,[label]) -> (* g_edge fits with an extended matching *)
                      (gid_next, e_match_add (id, (src_gid, label, gid_next)) partial.sub) :: acc
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

      (* let p_node =  *)
      (*   if pid >= 0  *)
      (*   then try P_graph.find pid positive with Not_found -> failwith "POS" *)
      (*   else try P_graph.find pid neg with Not_found -> failwith "NEG" in *)
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
  (** [apply_command instance matching created_nodes command] returns [(new_instance, new_created_nodes)] *)
  let apply_command ?domain (command,loc) instance matching created_nodes =
    let node_find cnode = find ~loc cnode (matching, created_nodes) in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        begin
          match G_graph.add_edge instance.Instance.graph src_gid edge tar_gid with
          | Some new_graph ->
              (
               {instance with
                Instance.graph = new_graph;
                history = List_.sort_insert (Command.H_ADD_EDGE (src_gid,tar_gid,edge)) instance.Instance.history
              },
               created_nodes
              )
          | None when !Global.safe_commands ->
              Error.run "ADD_EDGE: the edge '%s' already exists %s" (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> (instance, created_nodes)
        end

    | Command.ADD_EDGE_EXPL (src_cn,tar_cn,edge_ident) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (_,edge,_) =
          try List.assoc edge_ident matching.e_match
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in

        begin
          match G_graph.add_edge instance.Instance.graph src_gid edge tar_gid with
          | Some new_graph ->
              (
               {instance with
                Instance.graph = new_graph;
                history = List_.sort_insert (Command.H_ADD_EDGE_EXPL (src_gid,tar_gid,edge_ident)) instance.Instance.history
              },
               created_nodes
              )
          | None when !Global.safe_commands ->
              Error.run "ADD_EDGE_EXPL: the edge '%s' already exists %s" (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> (instance, created_nodes)

        end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (match G_graph.del_edge loc instance.Instance.graph src_gid edge tar_gid with
          | None when !Global.safe_commands -> Error.run "DEL_EDGE_EXPL: the edge '%s' does not exist %s" (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> (instance, created_nodes)
          | Some new_graph ->
          (
            {instance with
              Instance.graph = new_graph;
              history = List_.sort_insert (Command.H_DEL_EDGE_EXPL (src_gid,tar_gid,edge)) instance.Instance.history
              },
              created_nodes
              )
        )

    | Command.DEL_EDGE_NAME edge_ident ->
        let (src_gid,edge,tar_gid) =
          try List.assoc edge_ident matching.e_match
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in
          (match G_graph.del_edge ~edge_ident loc instance.Instance.graph src_gid edge tar_gid with
          | None -> Error.bug "DEL_EDGE_NAME"
          | Some new_graph ->
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_DEL_EDGE_EXPL (src_gid,tar_gid,edge)) instance.Instance.history
        },
         created_nodes
        ))

    | Command.DEL_NODE node_cn ->
        let node_gid = node_find node_cn in
        (match G_graph.del_node instance.Instance.graph node_gid with
        | None when !Global.safe_commands -> Error.run "DEL_NODE: the node does not exist %s" (Loc.to_string loc)
        | None -> (instance, created_nodes)
        | Some new_graph ->
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_DEL_NODE node_gid) instance.Instance.history
        },
         created_nodes
        )
        )
    | Command.UPDATE_FEAT (tar_cn,tar_feat_name, item_list) ->
        let tar_gid = node_find tar_cn in
        let rule_items = List.map
            (function
              | Command.Feat (cnode, feat_name) -> Concat_item.Feat (node_find cnode, feat_name)
              | Command.String s -> Concat_item.String s
              | Command.Lexical_field _ -> failwith "TODOLEX1"
              | Command.Param index ->
                  (match matching.m_param with
                  | None -> Error.bug "Cannot apply a UPDATE_FEAT command without parameter"
                  | Some param -> Concat_item.String (Lex_par.get_param_value index param))
            ) item_list in

        let (new_graph, new_feature_value) =
          G_graph.update_feat ~loc instance.Instance.graph tar_gid tar_feat_name rule_items in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_UPDATE_FEAT (tar_gid,tar_feat_name,new_feature_value)) instance.Instance.history
        },
         created_nodes
        )

    | Command.DEL_FEAT (tar_cn,feat_name) ->
        let tar_gid = node_find tar_cn in
        (match G_graph.del_feat instance.Instance.graph tar_gid feat_name with
        | None when !Global.safe_commands -> Error.run "DEL_FEAT: the feat does not exist %s" (Loc.to_string loc)
        | None ->
          Log.fwarning "DEL_FEAT: the feat does not exist %s" (Loc.to_string loc);
          (instance, created_nodes)
        | Some new_graph ->
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_DEL_FEAT (tar_gid,feat_name)) instance.Instance.history
        },
         created_nodes
        ))

    | Command.NEW_AFTER (created_name,base_cn) ->
        let base_gid = node_find base_cn in
        let (new_gid,new_graph) = G_graph.add_after base_gid instance.Instance.graph in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_NEW_AFTER (created_name,base_gid)) instance.Instance.history;
        },
         (created_name,new_gid) :: created_nodes
        )

    | Command.NEW_NODE (created_name) ->
        let (new_gid,new_graph) = G_graph.add_unordered instance.Instance.graph in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_NEW_NODE created_name) instance.Instance.history;
        },
         (created_name,new_gid) :: created_nodes
        )

    | Command.NEW_BEFORE (created_name,base_cn) ->
        let base_gid = node_find base_cn in
        let (new_gid,new_graph) = G_graph.add_before base_gid instance.Instance.graph in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_NEW_BEFORE (created_name,base_gid)) instance.Instance.history;
        },
         (created_name,new_gid) :: created_nodes
        )

    | Command.SHIFT_IN (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, _, _) = G_graph.shift_in loc src_gid tar_gid (test_locality matching created_nodes) label_cst instance.Instance.graph in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_SHIFT_IN (src_gid,tar_gid)) instance.Instance.history
        },
         created_nodes
        )

    | Command.SHIFT_OUT (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, _, _) = G_graph.shift_out loc src_gid tar_gid (test_locality matching created_nodes) label_cst instance.Instance.graph in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_SHIFT_OUT (src_gid,tar_gid)) instance.Instance.history
        },
         created_nodes
        )

    | Command.SHIFT_EDGE (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, _, _) = G_graph.shift_edges loc src_gid tar_gid (test_locality matching created_nodes) label_cst instance.Instance.graph in
        (
          {instance with
            Instance.graph = new_graph;
            history = List_.sort_insert (Command.H_SHIFT_EDGE (src_gid,tar_gid)) instance.Instance.history
          },
          created_nodes
        )

  (*  ---------------------------------------------------------------------- *)
  (** [apply_rule instance matching rule] returns a new instance after the application of the rule *)
  let apply_rule ?domain instance matching rule =

    (* Timeout check *)
    (try Timeout.check () with Timeout.Stop -> Error.run "Time out");

    let (new_instance, created_nodes) =
      List.fold_left
        (fun (instance, created_nodes) command ->
          apply_command ?domain command instance matching created_nodes
        )
        (instance, [])
        rule.commands in

    let rule_app = {
      Libgrew_types.rule_name = rule.name;
      up = match_deco rule.pattern matching;
      down = down_deco (matching,created_nodes) rule.commands
    } in

    {new_instance with
      Instance.rules = rule.name :: new_instance.Instance.rules;
      big_step = match new_instance.Instance.big_step with
        | None -> Some { Libgrew_types.first = rule_app; small_step = [] }
        | Some bs -> Some { bs with Libgrew_types.small_step = (instance.Instance.graph, rule_app) :: bs.Libgrew_types.small_step }
    }

  (*  ---------------------------------------------------------------------- *)
  let update_partial pos_graph without (sub, already_matched_gids) =
    let neg_graph = without.graph in
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
          (* if pid < 0  *)
          (* then acc *)
          (* else  *)
              Massoc_pid.fold
                (fun acc2 pid_next p_edge -> (pid, p_edge, pid_next) :: acc2)
                acc (P_node.get_next node)

            (* Massoc.fold_left  *)
            (*   (fun acc2 pid_next p_edge -> (pid, p_edge, pid_next) :: acc2) *)
            (*   acc (P_node.get_next node) *)
        ) neg_graph [] in
    {
     sub = sub;
     unmatched_nodes = unmatched_nodes;
     unmatched_edges = unmatched_edges;
     already_matched_gids = already_matched_gids;
     check = without.constraints;
   }


  (*  ---------------------------------------------------------------------- *)
  let fulfill ?domain (pos_graph,neg_graph) graph new_partial_matching =
    match extend_matching ?domain (pos_graph, neg_graph) graph new_partial_matching with
    | [] -> true (* the without pattern in not found -> OK *)
    | _ -> false

  (*  ---------------------------------------------------------------------- *)
  let match_in_graph ?domain ?lexicons ?param (pos, negs) graph =
    let casted_graph = G_graph.cast ?domain graph in
    let pos_graph = pos.graph in

    (* get the list of partial matching for positive part of the pattern *)
    let matching_list =

      extend_matching
        ?domain
        (pos_graph,P_graph.empty)
        casted_graph
        (init ?lexicons param pos) in

    let filtered_matching_list =
      List.filter
        (fun (sub, already_matched_gids) ->
          List.for_all
            (fun without ->
              let neg_graph = without.graph in
              let new_partial_matching = update_partial pos_graph without (sub, already_matched_gids) in
              fulfill ?domain (pos_graph,neg_graph) graph new_partial_matching
            ) negs
        ) matching_list in

    List.map fst filtered_matching_list

  (*  ---------------------------------------------------------------------- *)
  (** [one_step ?domain instance rules] computes the list of one-step reduct with rules *)
  let one_step ?domain instance rules =

    (* limit the rewriting depth to avoid looping rewriting *)
    if List.length instance.Instance.rules >= !max_depth_non_det
    then
      if !debug_loop
      then Instance_set.empty
      else Error.run "max depth %d reached, last rules applied: …, %s"
           !max_depth_non_det (List_.rev_to_string (fun x->x) ", " (List_.cut 5 instance.Instance.rules))
    else
      List.fold_left
        (fun acc rule ->
          let matching_list = match_in_graph ?domain ~lexicons:rule.lexicons ~param:(fst rule.param) rule.pattern instance.Instance.graph in
          List.fold_left
            (fun acc1 matching ->
              Instance_set.add (apply_rule ?domain instance matching rule) acc1
            ) acc matching_list
        ) Instance_set.empty rules





  (*  ---------------------------------------------------------------------- *)
  (** [conf_one_step ?domain modul_name instance rules] computes one Some (one-step reduct) with rules, None if no rule apply *)
  let rec conf_one_step ?domain (instance : Instance.t) rules =

    (* limit the rewriting depth to avoid looping rewriting *)
    if List.length instance.Instance.rules >= !max_depth_det
    then
      if !debug_loop
      then None
      else Error.run "max depth %d reached, last rules applied: …, %s"
           !max_depth_det (List_.rev_to_string (fun x->x) ", " (List_.cut 5 instance.Instance.rules))
    else
    match rules with
      | [] -> None
      | rule::rule_tail ->
        let (pos,negs) = rule.pattern in
        (* get the list of partial matching for positive part of the pattern *)
        let matching_list =
          extend_matching
            ?domain
            (pos.graph,P_graph.empty)
            instance.Instance.graph
            (init ~lexicons:rule.lexicons (Some (fst rule.param)) pos) in
        try
          let (first_matching_where_all_witout_are_fulfilled,_) =
            List.find
              (fun (sub, already_matched_gids) ->
                List.for_all
                  (fun neg ->
                    let new_partial_matching = update_partial pos.graph neg (sub, already_matched_gids) in
                    fulfill ?domain (pos.graph,neg.graph) instance.Instance.graph new_partial_matching
                  ) negs
              ) matching_list in
          Some (apply_rule ?domain instance first_matching_where_all_witout_are_fulfilled rule)
        with Not_found -> (* try another rule *) conf_one_step ?domain instance rule_tail

  (* ---------------------------------------------------------------------- *)
  (** filter nfs being equal *)
  let rec filter_equal_nfs nfs =
    Instance_set.fold
      (fun nf acc ->
        if Instance_set.exists (fun e -> G_graph.equals e.Instance.graph nf.Instance.graph) acc
        then acc
        else Instance_set.add nf acc
      ) nfs Instance_set.empty

  (* ---------------------------------------------------------------------- *)
  (** normalize [t] according to the [rules]. [t] is a raw graph
    Info about the commands applied on [t] are kept *)
  (* type: Instance.t -> t list -> Instance_set.t *)
  let normalize_instance ?domain modul_name instance rules =
    let rec loop to_do_set nf_set =
      if to_do_set = Instance_set.empty
      then nf_set
      else
        let (new_to_do_set,new_nf_set) =
          Instance_set.fold
            (fun v (to_do_set_acc,nf_set_acc) ->
              match Instance_set.elements (one_step ?domain v rules) with
                | [] -> (to_do_set_acc,Instance_set.add (Instance.rev_steps v) nf_set_acc)
                | step_of_v -> (List.fold_left (fun acc v1 -> Instance_set.add v1 acc) to_do_set_acc step_of_v, nf_set_acc)
            )
            to_do_set (Instance_set.empty,nf_set) in
        loop new_to_do_set new_nf_set in

    let nfs = loop (Instance_set.singleton instance) Instance_set.empty in
    let reduced_nfs = filter_equal_nfs nfs in

    let reduced_nfs_card = Instance_set.cardinal reduced_nfs in
    let nfs_card = Instance_set.cardinal nfs in
    if reduced_nfs_card < nfs_card
    then Log.fwarning "In module \"%s\", %d nf are produced, only %d different ones" modul_name nfs_card reduced_nfs_card;
    reduced_nfs

  (* ---------------------------------------------------------------------- *)
  let rec conf_normalize ?domain instance rules =
    match conf_one_step ?domain instance rules with
    | Some new_instance -> conf_normalize ?domain new_instance rules
    | None -> Instance.rev_steps instance

  (* ---------------------------------------------------------------------- *)
  let normalize ?domain modul_name ?(deterministic=false) rules instance =
    if deterministic
    then Instance_set.singleton (conf_normalize ?domain instance rules)
    else normalize_instance ?domain modul_name instance rules



  let apply ?domain rule instance =
  (* limit the rewriting depth to avoid looping rewriting *)
  if List.length instance.Instance.rules >= !max_depth_non_det
  then
    if !debug_loop
    then Instance_set.empty
    else Error.run "max depth %d reached, last rules applied: …, %s"
      !max_depth_non_det (List_.rev_to_string (fun x->x) ", " (List_.cut 5 instance.Instance.rules))
  else
    let matching_list = match_in_graph ?domain ~lexicons:rule.lexicons ?param:(Some (fst rule.param)) rule.pattern instance.Instance.graph in
    List.fold_left
      (fun acc matching ->
        Instance_set.add (apply_rule ?domain instance matching rule) acc
      ) Instance_set.empty matching_list


  let rec det_apply ?domain rule instance =
    (* limit the rewriting depth to avoid looping rewriting *)
    if List.length instance.Instance.rules >= !max_depth_det
      then
        if !debug_loop
        then None
        else Error.run "max depth %d reached, last rules applied: …, %s"
          !max_depth_det (List_.rev_to_string (fun x->x) ", " (List_.cut 5 instance.Instance.rules))
        else
          let (pos,negs) = rule.pattern in
          (* get the list of partial matching for positive part of the pattern *)
          let matching_list =
            extend_matching
              ?domain
              (pos.graph,P_graph.empty)
              instance.Instance.graph
              (init ~lexicons:rule.lexicons (Some (fst rule.param)) pos) in

          try
            let (first_matching_where_all_witout_are_fulfilled,_) =
              List.find
                (fun (sub, already_matched_gids) ->
                  List.for_all
                    (fun neg ->
                      let new_partial_matching = update_partial pos.graph neg (sub, already_matched_gids) in
                      fulfill ?domain (pos.graph,neg.graph) instance.Instance.graph new_partial_matching
                    ) negs
                ) matching_list in
            Some (apply_rule ?domain instance first_matching_where_all_witout_are_fulfilled rule)
          with Not_found -> None






  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)
  (*  ---------------------------------------------------------------------- *)










  let onf_find cnode ?loc (matching, created_nodes) =
    match cnode with
    | Command.Pat pid ->
        (try Pid_map.find pid matching.n_match
        with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name ->
        (try List.assoc name created_nodes
        with Not_found -> Error.run ?loc "Identifier '%s' not found" name)

  (*  ---------------------------------------------------------------------- *)
  (** [onf_apply_command eff ?domain command graph matching created_nodes]
      returns [(new_graph, new_created_nodes, new_eff)] *)
  let onf_apply_command eff ?domain (command,loc) graph matching created_nodes =
    let node_find cnode = onf_find ~loc cnode (matching, created_nodes) in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        begin
          match G_graph.add_edge graph src_gid edge tar_gid with
          | None when !Global.safe_commands ->
              Error.run "ADD_EDGE: the edge '%s' already exists %s" (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> (graph, created_nodes, eff)
          | Some new_graph -> (new_graph, created_nodes, true)
        end

    | Command.ADD_EDGE_EXPL (src_cn,tar_cn,edge_ident) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (_,edge,_) =
          try List.assoc edge_ident matching.e_match
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in
        begin
          match G_graph.add_edge graph src_gid edge tar_gid with
          | None when !Global.safe_commands ->
              Error.run "ADD_EDGE_EXPL: the edge '%s' already exists %s" (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> (graph, created_nodes, eff)
          | Some new_graph -> (new_graph, created_nodes, true)
        end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (match G_graph.del_edge loc graph src_gid edge tar_gid with
          | None when !Global.safe_commands -> Error.run "DEL_EDGE_EXPL: the edge '%s' does not exist %s" (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> (graph, created_nodes, eff)
          | Some new_graph -> (new_graph, created_nodes, true)
        )

    | Command.DEL_EDGE_NAME edge_ident ->
        let (src_gid,edge,tar_gid) =
          try List.assoc edge_ident matching.e_match
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in
          (match G_graph.del_edge ~edge_ident loc graph src_gid edge tar_gid with
            | None when !Global.safe_commands -> Error.run "DEL_EDGE_NAME: the edge '%s' does not exist %s" edge_ident (Loc.to_string loc)
            | None -> (graph, created_nodes, eff)
            | Some new_graph -> (new_graph, created_nodes, true)
          )

    | Command.DEL_NODE node_cn ->
        let node_gid = node_find node_cn in
        (match G_graph.del_node graph node_gid with
          | None when !Global.safe_commands -> Error.run "DEL_NODE: the node does not exist %s" (Loc.to_string loc)
          | None -> (graph, created_nodes, eff)
          | Some new_graph -> (new_graph, created_nodes, true)
        )

    | Command.UPDATE_FEAT (tar_cn,tar_feat_name, item_list) ->
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
              | Command.Param index ->
                  (match matching.m_param with
                  | None -> Error.bug "Cannot apply a UPDATE_FEAT command without parameter"
                  | Some param -> Concat_item.String (Lex_par.get_param_value index param))
            ) item_list in

        let (new_graph, new_feature_value) =
          G_graph.update_feat ~loc graph tar_gid tar_feat_name rule_items in
          (new_graph, created_nodes, true)

    | Command.DEL_FEAT (tar_cn,feat_name) ->
        let tar_gid = node_find tar_cn in
        (match G_graph.del_feat graph tar_gid feat_name with
          | None when !Global.safe_commands -> Error.run "XXX"
          | None -> (graph, created_nodes, eff)
          | Some new_graph -> (new_graph, created_nodes, true)
        )

    | Command.SHIFT_IN (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, de, ae) = G_graph.shift_in loc src_gid tar_gid (test_locality matching created_nodes) label_cst graph in
        (new_graph, created_nodes, eff || de <> [] || ae <> [])

    | Command.SHIFT_OUT (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, de, ae) = G_graph.shift_out loc src_gid tar_gid (test_locality matching created_nodes) label_cst graph in
        (new_graph, created_nodes, eff || de <> [] || ae <> [])

    | Command.SHIFT_EDGE (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, de, ae) = G_graph.shift_edges loc src_gid tar_gid (test_locality matching created_nodes) label_cst graph in
        (new_graph, created_nodes, eff || de <> [] || ae <> [])

    | Command.NEW_AFTER (created_name,base_cn) ->
        let base_gid = node_find base_cn in
        let (new_gid,new_graph) = G_graph.add_after base_gid graph in
        (new_graph, (created_name,new_gid) :: created_nodes, true)

    | Command.NEW_BEFORE (created_name,base_cn) ->
        let base_gid = node_find base_cn in
        let (new_gid,new_graph) = G_graph.add_before base_gid graph in
        (new_graph, (created_name,new_gid) :: created_nodes, true)

    | Command.NEW_NODE (created_name) ->
        let (new_gid,new_graph) = G_graph.add_unordered graph in
        (new_graph, (created_name,new_gid) :: created_nodes, true)

  let onf_apply ?domain rule graph =
    let (pos,negs) = rule.pattern in
    (* get the list of partial matching for positive part of the pattern *)
      let matching_list =
        extend_matching
          ?domain
          (pos.graph,P_graph.empty)
          graph
          (init ~lexicons:rule.lexicons (Some (fst rule.param)) pos) in
          try
            let (first_matching_where_all_witout_are_fulfilled,_) =
              List.find
                (fun (sub, already_matched_gids) ->
                  List.for_all
                    (fun neg ->
                      let new_partial_matching = update_partial pos.graph neg (sub, already_matched_gids) in
                      fulfill ?domain (pos.graph,neg.graph) graph new_partial_matching
                    ) negs
                ) matching_list in

            let (new_graph, created_nodes, eff) =
              List.fold_left
                (fun (graph, created_nodes, eff) command ->
                  onf_apply_command eff ?domain command graph first_matching_where_all_witout_are_fulfilled created_nodes
                )
                (graph, [], false)
                rule.commands in
            if eff
            then Some new_graph
            else None
          with Not_found -> (* raised by List.find, no matching apply *) None











  let rec wrd_apply ?domain rule (graph, big_step_opt) =
    let (pos,negs) = rule.pattern in
    (* get the list of partial matching for positive part of the pattern *)
      let matching_list =
        extend_matching
          ?domain
          (pos.graph,P_graph.empty)
          graph
          (init ~lexicons:rule.lexicons (Some (fst rule.param)) pos) in
          try
            let (first_matching_where_all_witout_are_fulfilled,_) =
              List.find
                (fun (sub, already_matched_gids) ->
                  List.for_all
                    (fun neg ->
                      let new_partial_matching = update_partial pos.graph neg (sub, already_matched_gids) in
                      fulfill ?domain (pos.graph,neg.graph) graph new_partial_matching
                    ) negs
                ) matching_list in

            let (new_graph, created_nodes, eff) =
              List.fold_left
                (fun (graph, created_nodes, eff) command ->
                  onf_apply_command eff ?domain command graph first_matching_where_all_witout_are_fulfilled created_nodes
                )
                (graph, [], false)
                rule.commands in

            let rule_app = {
              Libgrew_types.rule_name = rule.name;
              up = match_deco rule.pattern first_matching_where_all_witout_are_fulfilled;
              down = down_deco (first_matching_where_all_witout_are_fulfilled,created_nodes) rule.commands
            } in

            let new_big_step = match big_step_opt with
              | None -> {Libgrew_types.small_step = []; first=rule_app}
              | Some {Libgrew_types.small_step; first} -> {Libgrew_types.small_step = (graph,rule_app) :: small_step; first} in

            if eff
            then Some (new_graph, new_big_step)
            else None
          with Not_found -> (* raised by List.find, no matching apply *) None














  let find cnode ?loc gwh matching =
    match cnode with
    | Command.Pat pid ->
        (try Pid_map.find pid matching.n_match
        with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name -> List.assoc name gwh.Graph_with_history.added_gids


  let gwh_apply_command ?domain (command,loc) gwh matching =
    let node_find cnode = find ~loc cnode gwh matching in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        begin
          match G_graph.add_edge gwh.Graph_with_history.graph src_gid edge tar_gid with
          | None when !Global.safe_commands ->
            Error.run "ADD_EDGE: the edge '%s' already exists %s"
            (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> gwh
          | Some new_graph ->
              {gwh with
                Graph_with_history.graph = new_graph;
                delta = Delta.add_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
              }
        end

    | Command.ADD_EDGE_EXPL (src_cn,tar_cn,edge_ident) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (_,edge,_) =
          try List.assoc edge_ident matching.e_match
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in

        begin
          match G_graph.add_edge gwh.Graph_with_history.graph src_gid edge tar_gid with
          | None when !Global.safe_commands ->
            Error.run "ADD_EDGE_EXPL: the edge '%s' already exists %s"
            (G_edge.to_string ?domain edge) (Loc.to_string loc)
          | None -> gwh
          | Some new_graph ->
              {gwh with
                Graph_with_history.graph = new_graph;
                delta = Delta.add_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
              }
        end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (match G_graph.del_edge loc gwh.Graph_with_history.graph src_gid edge tar_gid with
        | None when !Global.safe_commands ->
          Error.run "DEL_EDGE_EXPL: the edge '%s' does not exist %s"
          (G_edge.to_string ?domain edge) (Loc.to_string loc)
        | None -> gwh
        | Some new_graph ->
         {gwh with
            Graph_with_history.graph = new_graph;
            delta = Delta.del_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
        })

    | Command.DEL_EDGE_NAME edge_ident ->
        let (src_gid,edge,tar_gid) =
          try List.assoc edge_ident matching.e_match
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in
          (match G_graph.del_edge ~edge_ident loc gwh.Graph_with_history.graph src_gid edge tar_gid with
        | None when !Global.safe_commands -> Error.run "DEL_EDGE_NAME: the edge '%s' does not exist %s" edge_ident (Loc.to_string loc)
        | None -> gwh
        | Some new_graph ->
         {gwh with
            Graph_with_history.graph = new_graph;
            delta = Delta.del_edge src_gid edge tar_gid gwh.Graph_with_history.delta;
        })

    | Command.DEL_NODE node_cn ->
        let node_gid = node_find node_cn in
        (match G_graph.del_node gwh.Graph_with_history.graph node_gid with
          | None when !Global.safe_commands -> Error.run "DEL_NODE the node does not exist %s" (Loc.to_string loc)
          | None -> gwh
          | Some new_graph ->
            { gwh with
              Graph_with_history.graph = new_graph;
              delta = Delta.del_node node_gid gwh.Graph_with_history.delta;
            }
        )

    | Command.UPDATE_FEAT (tar_cn,tar_feat_name, item_list) ->
        let tar_gid = node_find tar_cn in
        let rule_items = List.map
            (function
              | Command.Feat (cnode, feat_name) -> Concat_item.Feat (node_find cnode, feat_name)
              | Command.String s -> Concat_item.String s
              | Command.Lexical_field (lex_name, field) ->
                  (try
                    let lexicon = List.assoc lex_name matching.l_param in
                    let v = Lexicon.read field lexicon in
                    Concat_item.String v
                   with
                    | Not_found -> Error.run ~loc "UPDATE_FEAT: the lexicon '%s' does not exist" lex_name
                    | Lexicon.Not_functional_lexicon -> Error.run ~loc "UPDATE_FEAT: the lexicon is not functional" lex_name
                    )

              | Command.Param index ->
                  (match matching.m_param with
                  | None -> Error.bug "Cannot apply a UPDATE_FEAT command without parameter"
                  | Some param -> Concat_item.String (Lex_par.get_command_value index param))
            ) item_list in

        let (new_graph, new_feature_value) = (* TODO: take value type into account in update_feat *)
          G_graph.update_feat ~loc gwh.Graph_with_history.graph tar_gid tar_feat_name rule_items in
        let new_value =
          if Domain.is_num ?domain tar_feat_name
          then Float (float_of_string new_feature_value)
          else String new_feature_value in
          { gwh with
            Graph_with_history.graph = new_graph;
            delta = Delta.set_feat gwh.Graph_with_history.seed tar_gid tar_feat_name (Some new_value) gwh.Graph_with_history.delta;
          }

    | Command.DEL_FEAT (tar_cn,feat_name) ->
        let tar_gid = node_find tar_cn in
        (match G_graph.del_feat gwh.Graph_with_history.graph tar_gid feat_name with
          | None when !Global.safe_commands -> Error.run "DEL_FEAT the feat does not exist %s" (Loc.to_string loc)
          | None -> gwh
          | Some new_graph -> { gwh with
            Graph_with_history.graph = new_graph;
            delta = Delta.set_feat gwh.Graph_with_history.seed tar_gid feat_name None gwh.Graph_with_history.delta;
          }
        )

    | Command.SHIFT_IN (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, del_edges, add_edges) =
        G_graph.shift_in loc src_gid tar_gid (test_locality matching []) label_cst gwh.Graph_with_history.graph in
          { gwh with
            Graph_with_history.graph = new_graph;
            delta = gwh.Graph_with_history.delta
            |> (List.fold_right (fun (s,e,t) -> Delta.del_edge s e t) del_edges)
            |> (List.fold_right (fun (s,e,t) -> Delta.add_edge s e t) add_edges)
          }

    | Command.SHIFT_OUT (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, del_edges, add_edges) =
        G_graph.shift_out loc src_gid tar_gid (test_locality matching []) label_cst gwh.Graph_with_history.graph in
          { gwh with
            Graph_with_history.graph = new_graph;
            delta = gwh.Graph_with_history.delta
            |> (List.fold_right (fun (s,e,t) -> Delta.del_edge s e t) del_edges)
            |> (List.fold_right (fun (s,e,t) -> Delta.add_edge s e t) add_edges)
          }

    | Command.SHIFT_EDGE (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        let (new_graph, del_edges, add_edges) =
        G_graph.shift_edges loc src_gid tar_gid (test_locality matching []) label_cst gwh.Graph_with_history.graph in
          { gwh with
            Graph_with_history.graph = new_graph;
            delta = gwh.Graph_with_history.delta
            |> (List.fold_right (fun (s,e,t) -> Delta.del_edge s e t) del_edges)
            |> (List.fold_right (fun (s,e,t) -> Delta.add_edge s e t) add_edges)
          }

    | Command.NEW_AFTER (created_name,base_cn) ->
        let base_gid = node_find base_cn in
        let (new_gid,new_graph) = G_graph.add_after base_gid gwh.Graph_with_history.graph in
          { gwh with
            Graph_with_history.graph = new_graph;
            added_gids = (created_name, new_gid) :: gwh.Graph_with_history.added_gids
          }

    | Command.NEW_BEFORE (created_name,base_cn) ->
        let base_gid = node_find base_cn in
        let (new_gid,new_graph) = G_graph.add_before base_gid gwh.Graph_with_history.graph in
          { gwh with
            Graph_with_history.graph = new_graph;
            added_gids = (created_name, new_gid) :: gwh.Graph_with_history.added_gids
          }

    | Command.NEW_NODE (created_name) ->
        let (new_gid,new_graph) = G_graph.add_unordered gwh.Graph_with_history.graph in
          { gwh with
            Graph_with_history.graph = new_graph;
            added_gids = (created_name, new_gid) :: gwh.Graph_with_history.added_gids
          }

  (*  ---------------------------------------------------------------------- *)
  (** [apply_rule graph_with_history matching rule] returns a new graph_with_history after the application of the rule *)
  let gwh_apply_rule ?domain graph_with_history matching rule =
(*
    (* Timeout check *)
    (try Timeout.check () with Timeout.Stop -> Error.run "Time out"); *)

    let new_graph_with_history =
      List.fold_left
        (fun gwh command ->
          gwh_apply_command ?domain command gwh matching
        )
        graph_with_history
        rule.commands in
      new_graph_with_history
(*
    let rule_app = {
      Libgrew_types.rule_name = rule.name;
      up = match_deco rule.pattern matching;
      down = down_deco (matching,created_nodes) rule.commands
    } in
 *)












  let gwh_apply ?domain rule graph_with_history =
(*
  (* limit the rewriting depth to avoid looping rewriting *)
  if List.length instance.Instance.rules >= !max_depth_non_det
  then
    if !debug_loop
    then Instance_set.empty
    else Error.run "max depth %d reached, last rules applied: …, %s"
      !max_depth_non_det (List_.rev_to_string (fun x->x) ", " (List_.cut 5 instance.Instance.rules))
  else
*)
    let matching_list = match_in_graph ?domain ~lexicons:rule.lexicons ?param:(Some (fst rule.param)) rule.pattern graph_with_history.Graph_with_history.graph in
    List.fold_left
      (fun acc matching ->
        Graph_with_history_set.add (gwh_apply_rule ?domain graph_with_history matching rule) acc
      ) Graph_with_history_set.empty matching_list








end (* module Rule *)
