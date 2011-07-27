open Log
open Printf 

open Utils
open Ast
open Grew_fs
open Grew_edge
open Command
open Grew_node
open Graph

IFDEF DEP2PICT THEN
open Dep2pict
ENDIF


module Instance = struct
  type t = {
      graph: Graph.t;
      commands: Command.h list;
      rules: string list;
      big_step: Grew_types.big_step option; 
    }

  let empty = {graph = Graph.empty; rules=[]; commands=[]; big_step=None;}

  let from_graph g = {empty with graph = g} 

  let build gr_ast = 
    { empty with graph = fst (Graph.build gr_ast.Ast.nodes gr_ast.Ast.edges) }

  let rev_steps t = 
    { t with big_step = match t.big_step with
    | None -> None
    | Some bs -> Some {bs with Grew_types.small_step = List.rev bs.Grew_types.small_step }
    }

  let clear t = {empty with graph = t.graph } (* FIXME: normalization of node ids ??? *)
  let get_graph t = t.graph

  (* comparition is done on the list of commands *)
  (* only graph rewrited from the same init graph can be "compared" *)
  let compare t1 t2 = Pervasives.compare t1.commands t2.commands

IFDEF DEP2PICT THEN
  let save_dep_png ?main_feat base t = 
	    ignore (Dep2pict.fromDepStringToPng (Graph.to_dep ?main_feat t.graph) (base^".png"))
ENDIF

end

module Instance_set = Set.Make (Instance)





module Rule = struct 
  type pid = int
  type gid = int

  let max_depth = ref 50
  exception Bound_reached

  type const = 
    | No_out of int * Edge.t 
    | No_in of int * Edge.t
    | Filter of int * Feature_structure.t (* used when a without impose a fs on a node defined by the match pattern *)

  let build_constraint ?locals table = function 
    | (Ast.Start (node_name, labels), loc) -> No_out (Id.build ~loc node_name table, Edge.make ?locals labels)
    | (Ast.End (node_name, labels),loc) -> No_in (Id.build ~loc node_name table, Edge.make ?locals labels)

  type pattern = 
      { graph: Graph.t;
        constraints: const list;
      }

  let build_pos_pattern ?domain ?(locals=[||]) pattern_ast =
    let (graph,table) = Graph.build ?domain ~locals pattern_ast.Ast.pat_nodes pattern_ast.Ast.pat_edges in
    (
     {
      graph = graph;
      constraints = List.map (build_constraint ~locals table) pattern_ast.Ast.pat_const ;
    }, 
     table
    )

      (* the neg part *)
  let build_neg_constraint ?locals pos_table neg_table const =
    let id_build loc string_id = 
      match Id.build_opt string_id pos_table with Some i -> i | None -> -1-(Id.build ~loc string_id neg_table) in
    match const with
    | (Ast.Start (node_name, labels),loc) -> No_out (id_build loc node_name, Edge.make ?locals labels)
    | (Ast.End (node_name, labels),loc) -> No_in (id_build loc node_name, Edge.make ?locals labels)

  let build_neg_pattern ?domain ?(locals=[||]) pos_table pattern_ast =
    let (extension, neg_table) =
      Graph.build_extention ?domain ~locals pos_table pattern_ast.Ast.pat_nodes pattern_ast.Ast.pat_edges in

    let filters = IntMap.fold (fun id node acc -> Filter (id, node.Node.fs) :: acc) extension.Graph.old_map [] in

    (* (\* DEBUG *\) Printf.printf "-----> |filters| = %d\n%!" (List.length filters); *)
    {
     graph = {Graph.map = extension.Graph.ext_map; lub=0 };
     constraints = filters @ List.map (build_neg_constraint ~locals pos_table neg_table) pattern_ast.Ast.pat_const ;
   }


  type t = {
      name: string;
      pos: pattern;
      neg: pattern list;
      commands: Command.t list;
    }

  let build ?domain ?(locals=[||]) rule_ast = 
    (* (\* DEBUG *\) Printf.printf "==<Rule.build |neg|=%d>==\n%!" (List.length rule_ast.Ast.neg_patterns); *)

    let (pos,pos_table) = build_pos_pattern ?domain ~locals rule_ast.Ast.pos_pattern in

    {
     name = rule_ast.Ast.rule_id ;
     pos = pos;
     neg = List.map (fun p -> build_neg_pattern ?domain ~locals pos_table p) rule_ast.Ast.neg_patterns;
     commands = List.map (Command.build ?domain pos_table locals) rule_ast.Ast.commands;
   }

  type matching = {
      n_match: gid IntMap.t;                    (* partial fct: pattern nodes |--> graph nodes    *)
      e_match: (string*(gid*Label.t*gid)) list; (* edge matching: edge ident  |--> (src,label,tar) *)
      a_match: (gid*Label.t*gid) list;          (* anonymous edge mached *)
    }

  let empty_matching = { n_match = IntMap.empty; e_match = []; a_match = [];}

  let singleton_matching i j =  { empty_matching with n_match = IntMap.add i j IntMap.empty }

  let e_comp (e1,_) (e2,_) = compare e1 e2
      
  let union match1 match2 = {
    n_match = IntMap.union_if match1.n_match match2.n_match; 
    e_match = List_.sort_disjoint_union ~compare:e_comp match1.e_match match2.e_match;
    a_match = match1.a_match @ match2.a_match;
  }

  let e_match_add ?pos edge_id matching =
    match List_.usort_insert ~compare:e_comp edge_id matching.e_match with
    | Some new_e_match -> { matching with e_match = new_e_match }
    | None -> Error.bug "The edge identifier '%s' is binded twice in the same pattern" (fst edge_id)
          
  let a_match_add edge matching = {matching with a_match = edge::matching.a_match }

  let up_deco matching = 
    { Deco.nodes = IntMap.fold (fun _ gid acc -> gid::acc) matching.n_match [];
      Deco.edges = List.fold_left (fun acc (_,edge) -> edge::acc) matching.a_match matching.e_match;
    }

  let find cnode ?loc (matching, created_nodes) =
    match cnode with
    | Command.Pid pid -> 
        (try IntMap.find pid matching.n_match
        with Not_found -> Error.bug ?loc "Inconsistent matching pid '%d' not found" pid)
    | Command.New name -> 
        try List.assoc name created_nodes 
        with Not_found -> Error.run ?loc "Identifier '%s' not found" name
            

  let down_deco (matching,created_nodes) commands =
    {
     Deco.nodes = List.fold_left
       (fun acc -> function
         | (Command.COPY_FEAT (tar_cn,_,_,_),loc)
         | (Command.ADD_FEAT (tar_cn,_,_),loc)
         | (Command.DEL_FEAT (tar_cn,_),loc)
         | (Command.SHIFT_EDGE (_,tar_cn),loc) -> 
             (find tar_cn (matching, created_nodes)) :: acc
         | _ -> acc
       ) [] commands;
     Deco.edges = List.fold_left
       (fun acc -> function 
         | (Command.ADD_EDGE (src_cn,tar_cn,edge),loc) ->  
             (find src_cn (matching, created_nodes), Edge.as_label edge, find tar_cn (matching, created_nodes)) :: acc
         | _ -> acc
       ) [] commands
   }

  exception Fail
(* ================================================================================ *)
  type partial = {
      sub: matching; 
      unmatched_nodes: pid list;
      unmatched_edges: (pid * Edge.t * pid) list;
      already_matched_gids: gid list; (* to ensure injectivity *)
      check: const list (* constraints to verify at the end of the matching *)
    }    
        
        (* PREREQUISITES: 
           - all partial matching have the same domain 
           - the domain of the pattern P is the disjoint union of domain([sub]) and [unmatched_nodes]
         *)

  let init pattern =
    let roots = Graph.roots pattern.graph in

    let node_list = IntMap.fold (fun pid _ acc -> pid::acc) pattern.graph.Graph.map [] in

    (* put all roots in the front of the list to speed up the algo *)
    let sorted_node_list = 
      List.sort
        (fun n1 n2 -> match (List.mem n1 roots, List.mem n2 roots) with
        | true, false -> -1
        | false, true -> 1
        | _ -> 0) node_list in
    
    { sub = empty_matching;
      unmatched_nodes = sorted_node_list;
      unmatched_edges = [];
      already_matched_gids = [];
      check = pattern.constraints;
    }

  let fullfill graph matching = function 
    | No_out (pid,edge) -> 
        let gid = IntMap.find pid matching.n_match in
        Graph.edge_out graph gid edge
    | No_in (pid,edge) -> 
        let gid = IntMap.find pid matching.n_match in
        IntMap.exists
          (fun _ node ->
            List.exists (fun e -> Edge.compatible edge e) (Massoc.assoc gid node.Node.next)
          ) graph.Graph.map
    | Filter (pid, fs) ->
        (* (\* DEBUG *\) Printf.printf "==<Filter>==%!"; *)
        let gid = IntMap.find pid matching.n_match in
        let gnode = IntMap.find gid graph.Graph.map in
(* (\* DEBUG *\) let res =  *)
        Feature_structure.filter fs gnode.Node.fs
(* (\* DEBUG *\) in  *)
(* (\* DEBUG *\)  *)
(* (\* DEBUG *\) Printf.printf " %b\n%!" res;  *)
(* (\* DEBUG *\) Printf.printf " fs = %s\n" (Feature_structure.to_string fs); *)
(* (\* DEBUG *\) Printf.printf " gnode.Node.fs = %s\n" (Feature_structure.to_string gnode.Node.fs); *)
(* (\* DEBUG *\) res *)
(* (\* DEBUG *\) *)


          (* returns all extension of the partial input matching *)
  let rec extend_matching (positive,neg) (graph:Graph.t) (partial:partial) =
    match (partial.unmatched_edges, partial.unmatched_nodes) with
    | [], [] -> 
        (* (\* DEBUG *\) Printf.printf "==<1>==\n%!"; *)
        if List.for_all (fun const -> fullfill graph partial.sub const) partial.check 
        then [partial.sub, partial.already_matched_gids]
        else []
    | (src_pid, p_edge, tar_pid)::tail_ue, _ ->
        begin
          try (* is the tar already found in the matching ? *)
            let new_partials = 
              let src_gid = IntMap.find src_pid partial.sub.n_match in
              let tar_gid = IntMap.find tar_pid partial.sub.n_match in
              let src_gnode = Graph.find src_gid graph in
              let g_edges = Massoc.assoc tar_gid src_gnode.Node.next in
              
              match Edge.match_list p_edge g_edges with
              | Edge.Fail -> (* no good edge in graph for this pattern edge -> stop here *)
                  []
              | Edge.Ok label -> (* at least an edge in the graph fits the p_edge "constraint" -> go on *)
                  [ {partial with unmatched_edges = tail_ue; sub = a_match_add (src_gid,label,tar_gid) partial.sub} ] 
              | Edge.Binds (id,labels) -> (* n edges in the graph match the identified p_edge -> make copies of the [k] matchings (and returns n*k matchings) *)
                  List.map 
                    (fun label ->
                      {partial with sub = e_match_add (id,(src_gid,label,tar_gid)) partial.sub; unmatched_edges = tail_ue }
                    ) labels 
            in List_.flat_map (extend_matching (positive,neg) graph) new_partials
          with Not_found -> (* p_edge goes to an unmatched node *)
            let candidates = (* candidates (of type (gid, matching)) for m(tar_pid) = gid) with new partial matching m *)
              let src_gid = IntMap.find src_pid partial.sub.n_match in
              let src_gnode = Graph.find src_gid graph in
              Massoc.fold_left 
                (fun acc gid_next g_edge ->
                  match Edge.match_ p_edge g_edge with
                  | Edge.Fail -> (* g_edge does not fit, no new candidate *)
                      acc
                  | Edge.Ok label -> (* g_edge fits with the same matching *)
                      (gid_next, a_match_add (src_gid, label, gid_next) partial.sub) :: acc 
                  | Edge.Binds (id,[label]) -> (* g_edge fits with an extended matching *)
                      (gid_next, e_match_add (id, (src_gid, label, gid_next)) partial.sub) :: acc
                  | _ -> Error.bug "Edge.match_ must return exactly one label"
                ) [] src_gnode.Node.next in
            List_.flat_map
              (fun (gid_next, matching) -> 
                extend_matching_from (positive,neg) graph tar_pid gid_next
                  {partial with sub=matching; unmatched_edges = tail_ue}
              ) candidates
        end
    | [], pid :: _ -> 
        IntMap.fold          
          (fun gid _ acc ->
            (extend_matching_from (positive,neg) graph pid gid partial) @ acc
          ) graph.Graph.map []
          
  and extend_matching_from (positive,neg) (graph:Graph.t) pid gid partial =
    if List.mem gid partial.already_matched_gids
    then [] (* the required association pid -> gid is not injective *)
    else
      let p_node = 
        if pid >= 0 
        then try Graph.find pid positive with Not_found -> failwith "POS"
        else try Graph.find pid neg with Not_found -> failwith "NEG" in
      let g_node = try Graph.find gid graph with Not_found -> failwith "INS" in
      if not (Node.is_a p_node g_node) 
      then [] (* the nodes don't match *) 
      else 
        (* add all out-edges from pid in pattern *)
        let new_unmatched_edges = 
          Massoc.fold_left
            (fun acc pid_next p_edge -> (pid, p_edge, pid_next) :: acc
            ) partial.unmatched_edges p_node.Node.next in

        let new_partial = { partial with
                            unmatched_nodes = (try List_.rm pid partial.unmatched_nodes with Not_found -> failwith "List_.rm"); 
                            unmatched_edges = new_unmatched_edges;
                            already_matched_gids = gid :: partial.already_matched_gids;
                            sub = {partial.sub with n_match = IntMap.add pid gid partial.sub.n_match};
                          } in
        extend_matching (positive,neg) graph new_partial



(* the exception below is added to handle unification failure in merge!! *)
  exception Command_execution_fail

(** [apply_command instance matching created_nodes command] returns [(new_instance, new_created_nodes)] *)
  let apply_command (command,loc) instance matching created_nodes =
    let node_find cnode = find ~loc cnode (matching, created_nodes) in

    match command with
    | Command.ADD_EDGE (src_cn,tar_cn,edge) -> 
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        begin
          match Graph.add_edge instance.Instance.graph src_gid edge tar_gid with
          | Some new_graph -> 
              (
               {instance with 
                Instance.graph = new_graph; 
                commands = List_.sort_insert (Command.H_ADD_EDGE (src_gid,tar_gid,edge)) instance.Instance.commands
              },
               created_nodes
              )
          | None -> 
              Error.run "ADD_EDGE: the edge '%s' already exists %s" (Edge.to_string edge) (Loc.to_string loc)
        end

    | Command.DEL_EDGE_EXPL (src_cn,tar_cn,edge) -> 
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (
         {instance with 
          Instance.graph = Graph.del_edge loc instance.Instance.graph src_gid edge tar_gid; 
          commands = List_.sort_insert (Command.H_DEL_EDGE_EXPL (src_gid,tar_gid,edge)) instance.Instance.commands
        },
         created_nodes
        )

    | Command.DEL_EDGE_NAME edge_ident ->
        let (src_gid,label,tar_gid) = 
          try List.assoc edge_ident matching.e_match 
          with Not_found -> Error.bug "The edge identifier '%s' is undefined %s" edge_ident (Loc.to_string loc) in
        let edge = Edge.of_label label in
        (
         {instance with 
          Instance.graph = Graph.del_edge loc instance.Instance.graph src_gid edge tar_gid; 
          commands = List_.sort_insert (Command.H_DEL_EDGE_EXPL (src_gid,tar_gid,edge)) instance.Instance.commands
        },
         created_nodes
        )    

    | Command.DEL_NODE node_cn -> 
        let node_gid = node_find node_cn in
        (
         {instance with 
          Instance.graph = Graph.del_node instance.Instance.graph node_gid;
          commands = List_.sort_insert (Command.H_DEL_NODE node_gid) instance.Instance.commands
        },
         created_nodes
        )

    | Command.MERGE_NODE (src_cn, tar_cn) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (match Graph.merge_node loc instance.Instance.graph src_gid tar_gid with
        | Some new_graph -> 
            (
             {instance with 
              Instance.graph = new_graph;
              commands = List_.sort_insert (Command.H_MERGE_NODE (src_gid,tar_gid)) instance.Instance.commands
            },
             created_nodes
            )
        | None -> raise Command_execution_fail
        )
          
    | Command.COPY_FEAT (tar_cn,src_cn,tar_feat_name, src_feat_name) ->
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (
         {instance with 
          Instance.graph = Graph.cpy_feat instance.Instance.graph src_gid tar_gid src_feat_name tar_feat_name;
          commands = List_.sort_insert (Command.H_COPY_FEAT (tar_gid,src_gid,tar_feat_name,src_feat_name)) instance.Instance.commands
        },
         created_nodes
        )     

    | Command.ADD_FEAT (tar_cn,feat_name, feat_value) ->
        let tar_gid = node_find tar_cn in
        (
         {instance with 
          Instance.graph = Graph.add_feat instance.Instance.graph tar_gid feat_name feat_value;
          commands = List_.sort_insert (Command.H_ADD_FEAT (tar_gid,feat_name,feat_value)) instance.Instance.commands
        },
         created_nodes
        )     

    | Command.DEL_FEAT (tar_cn,feat_name) ->
        let tar_gid = node_find tar_cn in
        (
         {instance with 
          Instance.graph = Graph.del_feat instance.Instance.graph tar_gid feat_name;
          commands = List_.sort_insert (Command.H_DEL_FEAT (tar_gid,feat_name)) instance.Instance.commands
        },
         created_nodes
        )     

    | Command.NEW_NEIGHBOUR (created_name,edge,base_pid) ->
        let base_gid = IntMap.find base_pid matching.n_match in
        let (new_gid,new_graph) = Graph.add_neighbour loc instance.Instance.graph base_gid edge in
        (
         {instance with 
          Instance.graph = new_graph;
          commands = List_.sort_insert (Command.H_NEW_NEIGHBOUR (created_name,edge,new_gid)) instance.Instance.commands
        },
         (created_name,new_gid) :: created_nodes
        )

    | Command.SHIFT_EDGE (src_cn,tar_cn) -> 
        let src_gid = node_find src_cn in
        let tar_gid = node_find tar_cn in
        (
         {instance with 
          Instance.graph = Graph.shift_edges loc instance.Instance.graph src_gid tar_gid; 
          commands = List_.sort_insert (Command.H_SHIFT_EDGE (src_gid,tar_gid)) instance.Instance.commands
        },
         created_nodes
        )


(* (\* DEBUG *\)let cpt = ref 0 *)

(** [apply_rule instance matching rule] returns a new instance after the application of the rule 
    [Command_execution_fail] is raised if some merge unification fails
 *)
  let apply_rule instance matching rule = 

    (* limit the rewriting depth to avoid looping rewriting *)
    begin
      if List.length instance.Instance.rules >= !max_depth 
      then Error.run "Bound reached (when applying rule %s)" rule.name
    end;
    
    let (new_instance, created_nodes) =
      List.fold_left 
        (fun (instance, created_nodes) command ->
          apply_command command instance matching created_nodes
        ) 
        (instance, []) 
        rule.commands in

    let rule_app = {Grew_types.rule_name = rule.name; up = up_deco matching; down = down_deco (matching,created_nodes) rule.commands } in

    (* (\* DEBUG *\) let up_dot = Graph.to_dot ~deco:rule_app.Grew_types.up instance.Instance.graph in *)
    (* (\* DEBUG *\) let _ = File.write up_dot (Printf.sprintf "dot_%d_a.dot" !cpt) in *)
    (* (\* DEBUG *\) let _ = Sys.command (Printf.sprintf "dot -Tpng -o dot_%d_a.png dot_%d_a.dot" !cpt !cpt) in *)

    (* (\* DEBUG *\) let up_dep = Graph.to_dep ~deco:rule_app.Grew_types.up instance.Instance.graph in *)
    (* (\* DEBUG *\) let _ = File.write up_dep (Printf.sprintf "dep_%d_a.dep" !cpt) in *)
    (* (\* DEBUG *\) let _ = Sys.command (Printf.sprintf "dep2pict -png -o dep_%d_a.png -dep dep_%d_a.dep" !cpt !cpt) in *)

    (* (\* DEBUG *\) let down_dot = Graph.to_dot ~deco:rule_app.Grew_types.down new_instance.Instance.graph in *)
    (* (\* DEBUG *\) let _ = File.write down_dot (Printf.sprintf "dot_%d_b.dot" !cpt) in *)
    (* (\* DEBUG *\) let _ = Sys.command (Printf.sprintf "dot -Tpng -o dot_%d_b.png dot_%d_b.dot" !cpt !cpt) in *)

    (* (\* DEBUG *\) let down_dep = Graph.to_dep ~deco:rule_app.Grew_types.down new_instance.Instance.graph in *)
    (* (\* DEBUG *\) let _ = File.write down_dep (Printf.sprintf "dep_%d_b.dep" !cpt) in *)
    (* (\* DEBUG *\) let _ = Sys.command (Printf.sprintf "dep2pict -png -o dep_%d_b.png -dep dep_%d_b.dep" !cpt !cpt) in *)

    (* (\* DEBUG *\) incr cpt; *)

    {new_instance with 
     Instance.rules = rule.name :: new_instance.Instance.rules;
     big_step = match new_instance.Instance.big_step with
     | None -> Some { Grew_types.first = rule_app; small_step = [] }
     | Some bs -> Some { bs with Grew_types.small_step = (instance.Instance.graph, rule_app) :: bs.Grew_types.small_step }
   } 

(*-----------------------------*)

  let update_partial pos_graph without (sub, already_matched_gids) = 
    let neg_graph = without.graph in
    let unmatched_nodes = IntMap.fold (fun pid _ acc -> if pid < 0 then pid::acc else acc) neg_graph.Graph.map [] in
    let unmatched_edges = 
      IntMap.fold 
        (fun pid node acc -> 
          if pid < 0 
          then acc
          else 
            Massoc.fold_left 
              (fun acc2 pid_next p_edge -> (pid, p_edge, pid_next) :: acc2)
              acc node.Node.next
        ) neg_graph.Graph.map [] in

(* Printf.printf "XXX -> unmatched_nodes: %d\n" (List.length unmatched_nodes); *)
(* Printf.printf "XXX -> unmatched_edges: %d\n" (List.length unmatched_edges); *)

    {
     sub = sub;
     unmatched_nodes = unmatched_nodes;
     unmatched_edges = unmatched_edges;
     already_matched_gids = already_matched_gids;
     check = without.constraints;
   }
      

  let fulfill (pos_graph,neg_graph) graph new_partial_matching  =
    match extend_matching (pos_graph, neg_graph) graph new_partial_matching with
    | [] -> true (* the without pattern in not found -> OK *)
    | x -> false 


(** [one_step instance rules] computes the list of one-step reduct with rules *)
  let one_step instance rules =
    List.fold_left 
      (fun acc rule -> 
        let pos_graph = rule.pos.graph in

        (* get the list of partial matching for positive part of the pattern *)
        let matching_list = 
          extend_matching 
            (pos_graph,Graph.empty) 
            instance.Instance.graph 
            (init rule.pos) in
        
        (* Printf.printf "XXX -> | matching_list | = %d\n" (List.length matching_list); *)

        let filtered_matching_list =
          List.filter
            (fun (sub, already_matched_gids) ->
              List.for_all 
                (fun without -> 
                  let neg_graph = without.graph in
                  let new_partial_matching = update_partial pos_graph without (sub, already_matched_gids) in
                  fulfill (pos_graph,neg_graph) instance.Instance.graph new_partial_matching
                ) rule.neg
            ) matching_list in
        
        (* Printf.printf "XXX -> | filtered_matching_list | = %d\n" (List.length filtered_matching_list); *)
        List.fold_left 
          (fun acc1 (matching,_) -> 
            try (apply_rule instance matching rule) :: acc1 
            with Command_execution_fail -> Printf.printf "******\n%!"; acc1
          ) acc filtered_matching_list
      ) [] rules 

(** [conf_one_step instance rules] computes one Some (one-step reduct) with rules, None if no rule apply *)
  let rec conf_one_step (instance : Instance.t) = function
    | [] -> None
    | rule::rule_tail ->
        let pos_graph = rule.pos.graph in

        (* get the list of partial matching for positive part of the pattern *)
        let matching_list = 
          extend_matching 
            (pos_graph,Graph.empty) 
            instance.Instance.graph 
            (init rule.pos) in
        
        try 
          let (first_matching_where_all_witout_are_fulfilled,_) =
            List.find
              (fun (sub, already_matched_gids) ->
                List.for_all 
                  (fun without -> 
                    let neg_graph = without.graph in
                    let new_partial_matching = update_partial pos_graph without (sub, already_matched_gids) in
                    fulfill (pos_graph,neg_graph) instance.Instance.graph new_partial_matching
                  ) rule.neg
              ) matching_list in
          Some (apply_rule instance first_matching_where_all_witout_are_fulfilled rule)
        with Not_found -> (* try another rule *) conf_one_step instance rule_tail


(** filter nfs being equal *)
  let rec filter_equal_nfs nfs =
    Instance_set.fold (fun nf acc ->
      if Instance_set.exists (fun e -> Graph.equals e.Instance.graph nf.Instance.graph) acc
      then (printf "two normal equal normal forms"; acc)
      else Instance_set.add nf acc)
      nfs Instance_set.empty
      
(** normalize [t] according to the [rules]
 * [t] is a raw graph
 * info about the commands applied on [t] are kept
 *)

      (* type: Instance.t -> t list -> Instance_set.t *)
  let normalize_instance instance rules =
    let rec loop to_do nf = 
      if to_do = Instance_set.empty
      then nf
      else
        let (new_to_do,new_nf) =  
          Instance_set.fold 
            (fun v (to_do1,nf1) ->
              let step_of_v = one_step v rules in
              if step_of_v = [] (* nothing came out of v*)
              then (to_do1,Instance_set.add (Instance.rev_steps v) nf1)
              else (List.fold_left (fun acc v1 -> Instance_set.add v1 acc) to_do1 step_of_v,nf1)
            )
            to_do (Instance_set.empty,nf) in
        loop new_to_do new_nf in
    let nfs = loop (Instance_set.singleton instance) Instance_set.empty in 
    filter_equal_nfs nfs

  let rec conf_normalize instance rules =
    match conf_one_step instance rules with
    | Some new_instance -> conf_normalize new_instance rules
    | None -> Instance.rev_steps instance

          (* type: t list -> (Instance_set.elt -> bool) -> Instance.t -> Instance_set.t * Instance_set.t *)

  let normalize ?(confluent=false) rules filter instance =
    if confluent
    then
      let output = conf_normalize instance rules in
      if filter output 
      then (Instance_set.singleton output, Instance_set.empty)
      else (Instance_set.empty, Instance_set.singleton output)
    else
      let output_set = normalize_instance instance rules in 
      let (good_set, bad_set) = Instance_set.partition filter output_set in
      (good_set, bad_set)
end
