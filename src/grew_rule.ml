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

IFDEF DEP2PICT THEN
open Dep2pict
ENDIF

open Grew_base
open Grew_types

open Grew_ast
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
    free_index: int;
    activated_node: Gid.t list;
  }

  let empty = {graph = G_graph.empty; rules=[]; history=[]; big_step=None; free_index=0; activated_node=[];}

  let from_graph graph =
    {empty with
      graph = graph;
      free_index = (G_graph.max_binding graph) + 1;
    }

  let rev_steps t =
    { t with big_step = match t.big_step with
      | None -> None
      | Some bs -> Some {bs with Libgrew_types.small_step = List.rev bs.Libgrew_types.small_step }
    }

  let flatten t =
    (* [mapping] is list of couple (node_id, node_id) used to flatten the graph *)
    let (mapping, new_free) = List.fold_left
      (fun (acc_map, next_free) node_id ->
        (
          (node_id, Gid.Old next_free) :: acc_map,
          next_free + 1
        )
      ) ([], t.free_index) t.activated_node in
    { empty with graph = G_graph.rename mapping t.graph; free_index = new_free }

  (* comparison is done on the list of commands *)
  (* only graph rewritten from the same init graph can be "compared" *)
  let compare t1 t2 = Pervasives.compare t1.history t2.history

  let to_gr label_domain t = G_graph.to_gr label_domain t.graph

  let to_conll label_domain t = G_graph.to_conll label_domain t.graph

  let save_dot_png label_domain ?filter ?main_feat base t =
    ignore (Dot.to_png_file (G_graph.to_dot label_domain ?main_feat t.graph) (base^".png"))

  IFDEF DEP2PICT THEN
  let save_dep_png label_domain ?filter ?main_feat base t =
    let dep = G_graph.to_dep label_domain ?filter ?main_feat t.graph in
    let d2p = Dep2pict.from_dep ~dep in
    let _ = Dep2pict.save_png ~filename: (base^".png") d2p in
    Dep2pict.highlight_shift ()

  let save_dep_svg label_domain ?filter ?main_feat base t =
    let dep = G_graph.to_dep label_domain ?filter ?main_feat t.graph in
    let d2p = Dep2pict.from_dep ~dep in
    let _ = Dep2pict.save_svg ~filename: (base^".png") d2p in
    Dep2pict.highlight_shift ()
 
  ELSE
  let save_dep_png ?filter ?main_feat base t = None
  let save_dep_svg ?filter ?main_feat base t = None
  ENDIF
end (* module Instance *)

(* ================================================================================ *)
module Instance_set = Set.Make (Instance)

(* ================================================================================ *)
module Rule = struct
  (* the rewriting depth is bounded to stop rewriting when the system is not terminating *)
  let max_depth = ref 500

  type const =
    | Cst_out of Pid.t * Label_cst.t
    | Cst_in of Pid.t * Label_cst.t
    | Feature_eq of Pid.t * string * Pid.t * string
    | Feature_diseq of Pid.t * string * Pid.t * string

    | Feature_ineq of Ast.ineq * Pid.t * string * Pid.t * string
    | Feature_ineq_cst of Ast.ineq * Pid.t * string * float

    | Filter of Pid.t * P_fs.t (* used when a without impose a fs on a node defined by the match basic *)

  let build_pos_constraint domain label_domain ?locals pos_table const =
    let pid_of_name loc node_name = Pid.Pos (Id.build ~loc node_name pos_table) in
    match const with
      | (Ast.Cst_out (id,label_cst), loc) ->
        Cst_out (pid_of_name loc id, Label_cst.build ~loc label_domain ?locals label_cst)
      | (Ast.Cst_in (id,label_cst), loc) ->
        Cst_in (pid_of_name loc id, Label_cst.build ~loc label_domain ?locals label_cst)

      | (Ast.Feature_eq ((node_name1, feat_name1), (node_name2, feat_name2)), loc) ->
        Domain.check_feature_name domain ~loc feat_name1;
        Domain.check_feature_name domain ~loc feat_name2;
        Feature_eq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

      | (Ast.Feature_diseq ((node_name1, feat_name1), (node_name2, feat_name2)), loc) ->
        Domain.check_feature_name domain ~loc feat_name1;
        Domain.check_feature_name domain ~loc feat_name2;
        Feature_diseq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

      | (Ast.Feature_ineq (ineq, (node_name1, feat_name1), (node_name2, feat_name2)), loc) ->
        Domain.check_feature_name domain ~loc feat_name1;
        Domain.check_feature_name domain ~loc feat_name2;
        Feature_ineq (ineq, pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

      | (Ast.Feature_ineq_cst (ineq, (node_name1, feat_name1), constant), loc) ->
        Domain.check_feature_name domain ~loc feat_name1;
        Feature_ineq_cst (ineq, pid_of_name loc node_name1, feat_name1, constant)


  type basic = {
    graph: P_graph.t;
    constraints: const list;
  }

  let build_pos_basic domain label_domain ?pat_vars ?(locals=[||]) basic_ast =
    let (graph, pos_table) =
      P_graph.build domain label_domain ?pat_vars ~locals basic_ast.Ast.pat_nodes basic_ast.Ast.pat_edges in
    (
      {
        graph = graph;
        constraints = List.map (build_pos_constraint domain label_domain ~locals pos_table) basic_ast.Ast.pat_const
      },
      pos_table
    )

  (* the neg part *)
  let build_neg_constraint domain label_domain ?locals pos_table neg_table const =
    let pid_of_name loc node_name =
      match Id.build_opt node_name pos_table with
        | Some i -> Pid.Pos i
        | None -> Pid.Neg (Id.build ~loc node_name neg_table) in
    match const with
      | (Ast.Cst_out (id,label_cst), loc) ->
        Cst_out (pid_of_name loc id, Label_cst.build ~loc label_domain ?locals label_cst)
      | (Ast.Cst_in (id,label_cst), loc) ->
        Cst_in (pid_of_name loc id, Label_cst.build ~loc label_domain ?locals label_cst)

      | (Ast.Feature_eq (feat_id1, feat_id2), loc) ->
        let (node_name1, feat_name1) = feat_id1
        and (node_name2, feat_name2) = feat_id2 in
        Domain.check_feature_name domain ~loc feat_name1;
        Domain.check_feature_name domain ~loc feat_name2;
        Feature_eq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

      | (Ast.Feature_diseq (feat_id1, feat_id2), loc) ->
        let (node_name1, feat_name1) = feat_id1
        and (node_name2, feat_name2) = feat_id2 in
        Domain.check_feature_name domain ~loc feat_name1;
        Domain.check_feature_name domain ~loc feat_name2;
        Feature_diseq (pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

      | (Ast.Feature_ineq (ineq, feat_id1, feat_id2), loc) ->
        let (node_name1, feat_name1) = feat_id1
        and (node_name2, feat_name2) = feat_id2 in
        Domain.check_feature_name domain ~loc feat_name1;
        Domain.check_feature_name domain ~loc feat_name2;
        Feature_ineq (ineq, pid_of_name loc node_name1, feat_name1, pid_of_name loc node_name2, feat_name2)

      | (Ast.Feature_ineq_cst (ineq, feat_id1, constant), loc) ->
        let (node_name1, feat_name1) = feat_id1 in
        Domain.check_feature_name domain ~loc feat_name1;
        Feature_ineq_cst (ineq, pid_of_name loc node_name1, feat_name1, constant)

  (* It may raise [P_fs.Fail_unif] in case of contradiction on constraints *)
  let build_neg_basic domain label_domain ?pat_vars ?(locals=[||]) pos_table basic_ast =
    let (extension, neg_table) =
      P_graph.build_extension domain label_domain ?pat_vars ~locals pos_table basic_ast.Ast.pat_nodes basic_ast.Ast.pat_edges in

    let filters = Pid_map.fold (fun id node acc -> Filter (id, P_node.get_fs node) :: acc) extension.P_graph.old_map [] in
    {
      graph = extension.P_graph.ext_map;
      constraints = filters @ List.map (build_neg_constraint domain label_domain ~locals pos_table neg_table) basic_ast.Ast.pat_const ;
    }

  let get_edge_ids basic =
    Pid_map.fold
      (fun _ node acc ->
        Massoc_pid.fold
          (fun acc2 _ edge -> match P_edge.get_id edge with None -> acc2 | Some id -> id::acc2)
          acc (P_node.get_next node)
      ) basic.graph []

  (* a [pattern] is described by the positive basic and a list of negative basics. *)
  type pattern = basic * basic list

  type t = {
      name: string;
      pattern: pattern;
      commands: Command.t list;
      param: Lex_par.t option;
      param_names: (string list * string list);
      loc: Loc.t;
    }

  let get_name t = t.name

  let get_loc t = t.loc

  let is_filter t = t.commands = []

  (* ====================================================================== *)
  let to_dep label_domain t =
    let pos_basic = fst t.pattern in
    let buff = Buffer.create 32 in
    bprintf buff "[GRAPH] { scale = 200; }\n";

    let nodes =
      Pid_map.fold
        (fun id node acc ->
          (node, sprintf "  N_%s { word=\"%s\"; subword=\"%s\"}"
            (Pid.to_id id) (P_node.get_name node) (P_fs.to_dep t.param_names (P_node.get_fs node))
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
              (P_edge.to_string label_domain edge)
          )
          (P_node.get_next node)
      ) pos_basic.graph;

    List.iteri
      (fun i cst ->
        match cst with
          | Cst_out (pid, label_cst) ->
            bprintf buff "  N_%s -> C_%d {label = \"%s\"; style=dot; bottom; color=green;}\n"
              (Pid.to_id pid) i (Label_cst.to_string label_domain label_cst)
          | Cst_in (pid, label_cst) ->
            bprintf buff "  C_%d -> N_%s {label = \"%s\"; style=dot; bottom; color=green;}\n"
              i (Pid.to_id pid) (Label_cst.to_string label_domain label_cst)
          | _ -> ()
      ) pos_basic.constraints;
    bprintf buff "}\n";
    Buffer.contents buff

  (* ====================================================================== *)
  let build_commands domain label_domain ?param ?(locals=[||]) suffixes pos pos_table ast_commands =
    let known_act_ids = List.map (fun x -> (Ast.No_sharp x)) (Array.to_list pos_table) in
    let known_edge_ids = get_edge_ids pos in

    let rec loop (kai,kei) = function
      | [] -> []
      | ast_command :: tail ->
          let (command, (new_kai, new_kei)) =
            Command.build
              domain
              label_domain
              ?param
              (kai,kei)
              pos_table
              locals
              suffixes
              ast_command in
          command :: (loop (new_kai,new_kei) tail) in
    loop (known_act_ids, known_edge_ids) ast_commands

  (* ====================================================================== *)
  let parse_vars loc vars =
    let rec parse_cmd_vars = function
      | [] -> []
      | x::t when x.[0] = '@' -> x :: parse_cmd_vars t
      | x::t -> Error.bug ~loc "Illegal feature definition '%s' in the lexical rule" x in
    let rec parse_pat_vars = function
      | [] -> ([],[])
      | x::t when x.[0] = '@' -> ([],parse_cmd_vars (x::t))
      | x::t when x.[0] = '$' -> let (pv,cv) = parse_pat_vars t in (x::pv, cv)
      | x::t -> Error.bug ~loc "Illegal feature definition '%s' in the lexical rule" x in
    parse_pat_vars vars

  (* ====================================================================== *)
  let build domain label_domain ?(locals=[||]) suffixes dir rule_ast =

    let (param, pat_vars, cmd_vars) =
      match rule_ast.Ast.param with
      | None -> (None,[],[])
      | Some (files,vars) ->
          let (pat_vars, cmd_vars) = parse_vars rule_ast.Ast.rule_loc vars in
          let nb_pv = List.length pat_vars in
          let nb_cv = List.length cmd_vars in

          let local_param = match rule_ast.Ast.lex_par with
          | None -> None
          | Some lines -> Some (Lex_par.from_lines ~loc:rule_ast.Ast.rule_loc nb_pv nb_cv lines) in

          let full_param = List.fold_left
            (fun acc file ->
              match acc with
              | None -> Some (Lex_par.load ~loc:rule_ast.Ast.rule_loc dir nb_pv nb_cv file)
              | Some lp -> Some (Lex_par.append (Lex_par.load ~loc:rule_ast.Ast.rule_loc dir nb_pv nb_cv file) lp)
            ) local_param files in

          (full_param, pat_vars, cmd_vars) in

    let (pos, pos_table) = build_pos_basic domain label_domain ~pat_vars ~locals rule_ast.Ast.pos_basic in
    let (negs,_) =
      List.fold_left
      (fun (acc,pos) basic_ast ->
        try ((build_neg_basic domain label_domain ~pat_vars ~locals pos_table basic_ast) :: acc, pos+1)
        with P_fs.Fail_unif ->
          Log.fwarning "In rule \"%s\" [%s], the wihtout number %d cannot be satisfied, it is skipped"
            rule_ast.Ast.rule_id (Loc.to_string rule_ast.Ast.rule_loc) pos;
          (acc, pos+1)
      ) ([],1) rule_ast.Ast.neg_basics in
    {
      name = rule_ast.Ast.rule_id;
      pattern = (pos, negs);
      commands = build_commands domain label_domain ~param:(pat_vars,cmd_vars) ~locals suffixes pos pos_table rule_ast.Ast.commands;
      loc = rule_ast.Ast.rule_loc;
      param = param;
      param_names = (pat_vars,cmd_vars)
    }

  let build_pattern domain label_domain pattern_ast =
    let (pos, pos_table) = build_pos_basic domain label_domain pattern_ast.Ast.pat_pos in
    let negs = List_.try_map P_fs.Fail_unif (fun basic_ast -> build_neg_basic domain label_domain pos_table basic_ast) pattern_ast.Ast.pat_negs in
    (pos, negs)

  (* ====================================================================== *)
  type matching = {
      n_match: Gid.t Pid_map.t;                     (* partial fct: pattern nodes |--> graph nodes *)
      e_match: (string*(Gid.t*Label.t*Gid.t)) list; (* edge matching: edge ident  |--> (src,label,tar) *)
      a_match: (Gid.t*Label.t*Gid.t) list;          (* anonymous edge matched *)
      m_param: Lex_par.t option;
    }

  let empty_matching param = { n_match = Pid_map.empty; e_match = []; a_match = []; m_param = param;}

  let e_comp (e1,_) (e2,_) = compare e1 e2

  let e_match_add ?pos edge_id matching =
    match List_.usort_insert ~compare:e_comp edge_id matching.e_match with
    | Some new_e_match -> { matching with e_match = new_e_match }
    | None -> Error.bug "The edge identifier '%s' is binded twice in the same pattern" (fst edge_id)

  let a_match_add edge matching = {matching with a_match = edge::matching.a_match }

  let match_deco pattern matching =
    { G_deco.nodes =
        Pid_map.fold
          (fun pid gid acc ->
            let pnode = P_graph.find pid (fst pattern).graph in
            (gid, (P_node.get_name pnode, P_fs.feat_list (P_node.get_fs pnode))) ::acc
          ) matching.n_match [];
      G_deco.edges = List.fold_left (fun acc (_,edge) -> edge::acc) matching.a_match matching.e_match;
    }

  let find cnode ?loc (matching, (created_nodes,activated_nodes)) =
    match cnode with
    | Command.Pat pid ->
        (try Pid_map.find pid matching.n_match
        with Not_found -> Error.bug ?loc "Inconsistent matching pid '%s' not found" (Pid.to_string pid))
    | Command.New name ->
        (try List.assoc name created_nodes
        with Not_found -> Error.run ?loc "Identifier '%s' not found" name)
    | Command.Act (pid, new_name) ->
        (try List.assoc (pid, new_name) activated_nodes
        with Not_found -> Error.run ?loc "Activated identifier with suffix '%s' not found" new_name)



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
           - all partial matching have the same domain
           - the domain of the pattern P is the disjoint union of domain([sub]) and [unmatched_nodes]
         *)
  (*  ---------------------------------------------------------------------- *)
  let init param basic =
    let roots = P_graph.roots basic.graph in

    let node_list = Pid_map.fold (fun pid _ acc -> pid::acc) basic.graph [] in

    (* put all roots in the front of the list to speed up the algo *)
    let sorted_node_list =
      List.sort
        (fun n1 n2 -> match (List.mem n1 roots, List.mem n2 roots) with
        | true, false -> -1
        | false, true -> 1
        | _ -> 0) node_list in

    { sub = empty_matching param;
      unmatched_nodes = sorted_node_list;
      unmatched_edges = [];
      already_matched_gids = [];
      check = basic.constraints;
    }

  (*  ---------------------------------------------------------------------- *)
  let apply_cst label_domain graph matching cst =
    let get_node pid = G_graph.find (Pid_map.find pid matching.n_match) graph in
    let get_string_feat pid = function
      | "position" -> Some (sprintf "%g" (G_node.get_position (get_node pid)))
      | feat_name -> G_fs.get_string_atom feat_name (G_node.get_fs (get_node pid)) in
    let get_float_feat pid = function
      | "position" -> Some (G_node.get_position (get_node pid))
      | feat_name -> G_fs.get_float_feat feat_name (G_node.get_fs (get_node pid)) in

    match cst with
      | Cst_out (pid,label_cst) ->
        let gid = Pid_map.find pid matching.n_match in
        if G_graph.edge_out label_domain graph gid label_cst
        then matching
        else raise Fail
      | Cst_in (pid,label_cst) ->
        let gid = Pid_map.find pid matching.n_match in
        if G_graph.node_exists
          (fun node ->
            List.exists (fun e -> Label_cst.match_ label_domain e label_cst) (Massoc_gid.assoc gid (G_node.get_next node))
          ) graph
        then matching
        else raise Fail
      | Filter (pid, fs) ->
        begin
          try
            let gid = Pid_map.find pid matching.n_match in
            let gnode = G_graph.find gid graph in
            let new_param = P_fs.match_ ?param:matching.m_param fs (G_node.get_fs gnode) in
            {matching with m_param = new_param }
          with P_fs.Fail -> raise Fail
        end
      | Feature_eq (pid1, feat_name1, pid2, feat_name2) ->
        begin
          match (get_string_feat pid1 feat_name1, get_string_feat pid2 feat_name2) with
            | Some fv1, Some fv2 when fv1 = fv2 -> matching
            | _ -> raise Fail
        end
      | Feature_diseq (pid1, feat_name1, pid2, feat_name2) ->
        begin
          match (get_string_feat pid1 feat_name1, get_string_feat pid2 feat_name2) with
            | Some fv1, Some fv2 when fv1 <> fv2 -> matching
            | _ -> raise Fail
        end
      | Feature_ineq (ineq, pid1, feat_name1, pid2, feat_name2) ->
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

  (*  ---------------------------------------------------------------------- *)
  (* returns all extension of the partial input matching *)
  let rec extend_matching label_domain (positive,neg) (graph:G_graph.t) (partial:partial) =
    match (partial.unmatched_edges, partial.unmatched_nodes) with
    | [], [] ->
      begin
        try
          let new_matching =
            List.fold_left
              (fun acc const ->
                apply_cst label_domain graph acc const
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

              match P_edge.match_list label_domain p_edge g_edges with
              | P_edge.Fail -> (* no good edge in graph for this pattern edge -> stop here *)
                  []
              | P_edge.Ok label -> (* at least an edge in the graph fits the p_edge "constraint" -> go on *)
                  [ {partial with unmatched_edges = tail_ue; sub = a_match_add (src_gid,label,tar_gid) partial.sub} ]
              | P_edge.Binds (id,labels) -> (* n edges in the graph match the identified p_edge -> make copies of the [k] matchings (and returns n*k matchings) *)
                  List.map
                    (fun label ->
                      {partial with sub = e_match_add (id,(src_gid,label,tar_gid)) partial.sub; unmatched_edges = tail_ue }
                    ) labels
            in List_.flat_map (extend_matching label_domain (positive,neg) graph) new_partials
          with Not_found -> (* p_edge goes to an unmatched node *)
            let candidates = (* candidates (of type (gid, matching)) for m(tar_pid) = gid) with new partial matching m *)
              let (src_gid : Gid.t) = Pid_map.find src_pid partial.sub.n_match in
              let src_gnode = G_graph.find src_gid graph in
              Massoc_gid.fold
                (fun acc gid_next g_edge ->
                  match P_edge.match_ label_domain p_edge g_edge with
                  | P_edge.Fail -> (* g_edge does not fit, no new candidate *)
                      acc
                  | P_edge.Ok label -> (* g_edge fits with the same matching *)
                      (gid_next, a_match_add (src_gid, label, gid_next) partial.sub) :: acc
                  | P_edge.Binds (id,[label]) -> (* g_edge fits with an extended matching *)
                      (gid_next, e_match_add (id, (src_gid, label, gid_next)) partial.sub) :: acc
                  | _ -> Error.bug "P_edge.match_ must return exactly one label"
                ) [] (G_node.get_next src_gnode) in
            List_.flat_map
              (fun (gid_next, matching) ->
                extend_matching_from label_domain (positive,neg) graph tar_pid gid_next
                  {partial with sub=matching; unmatched_edges = tail_ue}
              ) candidates
        end
    | [], pid :: _ ->
        G_graph.fold_gid
          (fun gid acc ->
            (extend_matching_from label_domain (positive,neg) graph pid gid partial) @ acc
          ) graph []

  (*  ---------------------------------------------------------------------- *)
  and extend_matching_from label_domain (positive,neg) (graph:G_graph.t) pid (gid : Gid.t) partial =
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
      let g_node = try G_graph.find gid graph with Not_found -> failwith "INS" in

      try
        let new_param = P_node.match_ ?param: partial.sub.m_param p_node g_node in
        (* add all out-edges from pid in pattern *)
        let new_unmatched_edges =
          Massoc_pid.fold
            (fun acc pid_next p_edge -> (pid, p_edge, pid_next) :: acc
            ) partial.unmatched_edges (P_node.get_next p_node) in

        let new_partial =
          { partial with
            unmatched_nodes = (try List_.rm pid partial.unmatched_nodes with Not_found -> failwith "List_.rm");
            unmatched_edges = new_unmatched_edges;
            already_matched_gids = gid :: partial.already_matched_gids;
            sub = {partial.sub with n_match = Pid_map.add pid gid partial.sub.n_match; m_param = new_param};
          } in
        extend_matching label_domain (positive,neg) graph new_partial
      with P_fs.Fail -> []

  (*  ---------------------------------------------------------------------- *)
  (* the exception below is added to handle unification failure in merge!! *)
  exception Command_execution_fail

  (*  ---------------------------------------------------------------------- *)
  (** [apply_command instance matching created_nodes command] returns [(new_instance, new_created_nodes)] *)
  let apply_command domain label_domain (command,loc) instance matching (created_nodes, (activated_nodes:((Pid.t * string) * Gid.t) list)) =
    let node_find cnode = find ~loc cnode (matching, (created_nodes, activated_nodes)) in

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
               (created_nodes, activated_nodes)
              )
          | None ->
              Error.run "ADD_EDGE: the edge '%s' already exists %s" (G_edge.to_string label_domain edge) (Loc.to_string loc)
        end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (
         {instance with
          Instance.graph = G_graph.del_edge label_domain loc instance.Instance.graph src_gid edge tar_gid;
           history = List_.sort_insert (Command.H_DEL_EDGE_EXPL (src_gid,tar_gid,edge)) instance.Instance.history
        },
         (created_nodes, activated_nodes)
        )

    | Command.DEL_EDGE_NAME edge_ident ->
        let (src_gid,edge,tar_gid) =
          try List.assoc edge_ident matching.e_match
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in
        (
         {instance with
          Instance.graph = G_graph.del_edge label_domain ~edge_ident loc instance.Instance.graph src_gid edge tar_gid;
          history = List_.sort_insert (Command.H_DEL_EDGE_EXPL (src_gid,tar_gid,edge)) instance.Instance.history
        },
         (created_nodes, activated_nodes)
        )

    | Command.DEL_NODE node_cn ->
        let node_gid = node_find node_cn in
        (
         {instance with
          Instance.graph = G_graph.del_node instance.Instance.graph node_gid;
          history = List_.sort_insert (Command.H_DEL_NODE node_gid) instance.Instance.history
        },
         (created_nodes, activated_nodes)
        )

    | Command.MERGE_NODE (src_cn, tar_cn) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (match G_graph.merge_node loc label_domain instance.Instance.graph src_gid tar_gid with
        | Some new_graph ->
            (
             {instance with
              Instance.graph = new_graph;
              history = List_.sort_insert (Command.H_MERGE_NODE (src_gid,tar_gid)) instance.Instance.history
            },
             (created_nodes, activated_nodes)
            )
        | None -> raise Command_execution_fail
        )

    | Command.UPDATE_FEAT (tar_cn,tar_feat_name, item_list) ->
        let tar_gid = node_find tar_cn in
        let rule_items = List.map
            (function
              | Command.Feat (cnode, feat_name) -> Concat_item.Feat (node_find cnode, feat_name)
              | Command.String s -> Concat_item.String s
              | Command.Param_out index ->
                  (match matching.m_param with
                  | None -> Error.bug "Cannot apply a UPDATE_FEAT command without parameter"
                  | Some param -> Concat_item.String (Lex_par.get_command_value index param))
              | Command.Param_in index ->
                  (match matching.m_param with
                  | None -> Error.bug "Cannot apply a UPDATE_FEAT command without parameter"
                  | Some param -> Concat_item.String (Lex_par.get_param_value index param))
            ) item_list in

        let (new_graph, new_feature_value) =
          G_graph.update_feat ~loc domain instance.Instance.graph tar_gid tar_feat_name rule_items in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_UPDATE_FEAT (tar_gid,tar_feat_name,new_feature_value)) instance.Instance.history
        },
         (created_nodes, activated_nodes)
        )

    | Command.DEL_FEAT (tar_cn,feat_name) ->
        let tar_gid = node_find tar_cn in
        (
         {instance with
          Instance.graph = G_graph.del_feat instance.Instance.graph tar_gid feat_name;
          history = List_.sort_insert (Command.H_DEL_FEAT (tar_gid,feat_name)) instance.Instance.history
        },
         (created_nodes, activated_nodes)
        )

    | Command.NEW_NEIGHBOUR (created_name,edge,base_pid) ->
        let base_gid = Pid_map.find base_pid matching.n_match in
        let (new_gid,new_graph) = G_graph.add_neighbour loc label_domain instance.Instance.graph base_gid edge in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_NEW_NEIGHBOUR (created_name,edge,new_gid)) instance.Instance.history;
          activated_node = new_gid :: instance.Instance.activated_node;
        },
         ((created_name,new_gid) :: created_nodes, activated_nodes)
        )

    | Command.ACT_NODE (Command.Act (pid, new_name)) ->
        let node_gid = node_find (Command.Pat(pid)) in
        let (new_gid, new_graph) = G_graph.activate loc node_gid new_name instance.Instance.graph in
        (
         {instance with
          Instance.graph = new_graph;
          history = List_.sort_insert (Command.H_ACT_NODE (node_gid,new_name)) instance.Instance.history
        },
         (created_nodes, ((pid, new_name), new_gid) :: activated_nodes)
        )
    | Command.ACT_NODE _ -> Error.bug "Try to activate a node without suffix" (Loc.to_string loc)

    | Command.SHIFT_IN (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (
         {instance with
          Instance.graph = G_graph.shift_in loc label_domain src_gid tar_gid label_cst instance.Instance.graph;
          history = List_.sort_insert (Command.H_SHIFT_IN (src_gid,tar_gid)) instance.Instance.history
        },
         (created_nodes, activated_nodes)
        )

    | Command.SHIFT_OUT (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (
         {instance with
          Instance.graph = G_graph.shift_out loc label_domain src_gid tar_gid label_cst instance.Instance.graph;
          history = List_.sort_insert (Command.H_SHIFT_OUT (src_gid,tar_gid)) instance.Instance.history
        },
         (created_nodes, activated_nodes)
        )

    | Command.SHIFT_EDGE (src_cn,tar_cn,label_cst) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (
          {instance with
            Instance.graph = G_graph.shift_edges loc label_domain src_gid tar_gid label_cst instance.Instance.graph;
            history = List_.sort_insert (Command.H_SHIFT_EDGE (src_gid,tar_gid)) instance.Instance.history
          },
          (created_nodes, activated_nodes)
        )

  (*  ---------------------------------------------------------------------- *)
  (** [apply_rule instance matching rule] returns a new instance after the application of the rule
      [Command_execution_fail] is raised if some merge unification fails *)
  let apply_rule domain label_domain instance matching rule =

    (* Timeout check *)
    (try Timeout.check () with Timeout.Stop -> Error.run "Time out");

    (* limit the rewriting depth to avoid looping rewriting *)
    begin
      if List.length instance.Instance.rules >= !max_depth
      then Error.run "Bound reached (when applying rule %s)" rule.name
    end;

    let (new_instance, created_nodes) =
      List.fold_left
        (fun (instance, created_nodes) command ->
          apply_command domain label_domain command instance matching created_nodes
        )
        (instance, ([],[]))
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
  let fulfill label_domain (pos_graph,neg_graph) graph new_partial_matching =
    match extend_matching label_domain (pos_graph, neg_graph) graph new_partial_matching with
    | [] -> true (* the without pattern in not found -> OK *)
    | _ -> false

  (*  ---------------------------------------------------------------------- *)
  let match_in_graph label_domain ?param (pos, negs) graph =
    let pos_graph = pos.graph in

    (* get the list of partial matching for positive part of the pattern *)
    let matching_list =
      extend_matching
        label_domain
        (pos_graph,P_graph.empty)
        graph
        (init param pos) in

    let filtered_matching_list =
      List.filter
        (fun (sub, already_matched_gids) ->
          List.for_all
            (fun without ->
              let neg_graph = without.graph in
              let new_partial_matching = update_partial pos_graph without (sub, already_matched_gids) in
              fulfill label_domain (pos_graph,neg_graph) graph new_partial_matching
            ) negs
        ) matching_list in

    List.map fst filtered_matching_list

  (*  ---------------------------------------------------------------------- *)
  (** [one_step instance rules] computes the list of one-step reduct with rules *)
  let one_step domain label_domain instance rules =
    List.fold_left
      (fun acc rule ->
        let matching_list = match_in_graph label_domain ?param:rule.param rule.pattern instance.Instance.graph in

        List.fold_left
          (fun acc1 matching ->
            try (apply_rule domain label_domain instance matching rule) :: acc1
            with Command_execution_fail -> acc1
          ) acc matching_list
      ) [] rules

  (*  ---------------------------------------------------------------------- *)
  (** [conf_one_step instance rules] computes one Some (one-step reduct) with rules, None if no rule apply *)
  let rec conf_one_step domain label_domain (instance : Instance.t) = function
    | [] -> None
    | rule::rule_tail ->
        let (pos,negs) = rule.pattern in

        (* get the list of partial matching for positive part of the pattern *)
        let matching_list =
          extend_matching
            label_domain
            (pos.graph,P_graph.empty)
            instance.Instance.graph
            (init rule.param pos) in

        try
          let (first_matching_where_all_witout_are_fulfilled,_) =
            List.find
              (fun (sub, already_matched_gids) ->
                List.for_all
                  (fun neg ->
                    let new_partial_matching = update_partial pos.graph neg (sub, already_matched_gids) in
                    fulfill label_domain (pos.graph,neg.graph) instance.Instance.graph new_partial_matching
                  ) negs
              ) matching_list in
          Some (apply_rule domain label_domain instance first_matching_where_all_witout_are_fulfilled rule)
        with Not_found -> (* try another rule *) conf_one_step domain label_domain instance rule_tail


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
  let normalize_instance domain label_domain modul_name instance rules =
    let rec loop to_do_set nf_set =
      if to_do_set = Instance_set.empty
      then nf_set
      else
        let (new_to_do_set,new_nf_set) =
          Instance_set.fold
            (fun v (to_do_set_acc,nf_set_acc) ->
              match one_step domain label_domain v rules with
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
  (* [filter_instance instance filters] return a boolean:
     - true iff the instance does NOT match any pattern in [filters] *)
  let filter_instance label_domain filters instance =
    let rec loop = function
      | [] -> true (* no more filter to check *)
      | filter::filter_tail ->
          let (pos,negs) = filter.pattern in

          (* get the list of partial matching for positive part of the pattern *)
          let matching_list =
            extend_matching
              label_domain
              (pos.graph,P_graph.empty)
              instance.Instance.graph
              (init filter.param pos) in

          if List.exists
              (fun (sub, already_matched_gids) ->
                List.for_all
                  (fun neg ->
                    let new_partial_matching = update_partial pos.graph neg (sub, already_matched_gids) in
                    fulfill label_domain (pos.graph,neg.graph) instance.Instance.graph new_partial_matching
                  ) negs
              ) matching_list
          then (* one of the matching can be extended *) false
          else loop filter_tail in
    loop filters

  (* ---------------------------------------------------------------------- *)
  let rec conf_normalize domain label_domain instance rules =
    match conf_one_step domain label_domain instance rules with
    | Some new_instance -> conf_normalize domain label_domain new_instance rules
    | None -> Instance.rev_steps instance

  (* ---------------------------------------------------------------------- *)
  let normalize domain label_domain modul_name ?(confluent=false) rules filters instance =
    if confluent
    then
      let output = conf_normalize domain label_domain instance rules in
      if filter_instance label_domain filters output
      then (Instance_set.singleton output, Instance_set.empty)
      else (Instance_set.empty, Instance_set.singleton output)
    else
      let output_set = normalize_instance domain label_domain modul_name instance rules in
      let (good_set, bad_set) = Instance_set.partition (filter_instance label_domain filters) output_set in
      (good_set, bad_set)
end (* module Rule *)
