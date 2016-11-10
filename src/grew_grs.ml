(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Log

open Grew_fs
open Grew_base
open Grew_types
open Grew_ast
open Grew_domain
open Grew_edge
open Grew_command
open Grew_graph
open Grew_rule
open Grew_loader

(* ================================================================================ *)
module Rewrite_history = struct
  type t = {
    instance: Instance.t;
    module_name: string;
    good_nf: t list;
    bad_nf: Instance.t list;
  }

  let rec get_graphs = function
    | { good_nf = []; bad_nf = []; instance } -> [instance.Instance.graph]
    | { good_nf = [] } -> []
    | { good_nf = l} -> List_.flat_map get_graphs l

  let rec is_empty t =
    (t.instance.Instance.rules = []) && List.for_all is_empty t.good_nf

  let rec num_sol = function
    | { good_nf = []; bad_nf = [] } -> 1
    | { good_nf = [] } -> 0 (* dead branch *)
    | { good_nf = l} -> List.fold_left (fun acc t -> acc + (num_sol t)) 0 l

  let save_nfs ?domain ?filter ?main_feat ~dot base_name t =
    let rec loop file_name rules t =
      match (t.good_nf, t.bad_nf) with
        | [],[] when dot -> Instance.save_dot_png ?domain ?filter ?main_feat file_name t.instance; [rules, file_name]
        | [],[] -> ignore (Instance.save_dep_png ?domain ?filter ?main_feat file_name t.instance); [rules, file_name]
        | [],_ -> []
        | l, _ ->
          List_.foldi_left
            (fun i acc son ->
              (* Instance.save_dep_png ?main_feat (sprintf "%s_%d" file_name i) son.instance; *)
              let nfs = loop
                (sprintf "%s_%d" file_name i)
                (rules @ [t.module_name, son.instance.Instance.rules])
                son in
              nfs @ acc
            )
            [] l in
    loop base_name [] t

  let save_gr ?domain base t =
    let rec loop file_name t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_gr ?domain t.instance) (file_name^".gr")
        | l, _ -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_conll ?domain base t =
    let rec loop file_name t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_conll_string ?domain t.instance) (file_name^".conll")
        | l, _ -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_full_conll ?domain base t =
    let cpt = ref 0 in
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> 
          File.write (Instance.to_conll_string ?domain t.instance) (sprintf "%s__%d.conll" base !cpt);
          incr cpt
        | l, _ -> List.iter loop l
    in loop t; !cpt

  (* suppose that all modules are confluent and produced exacly one normal form *)
  let save_det_gr ?domain base t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_gr ?domain t.instance) (base^".gr")
        | [one], [] -> loop one
        | _ -> Error.run "[save_det_gr] Not a single rewriting"
    in loop t

  let save_annot ?domain out_dir base_name t =
    List.mapi
      (fun i alts ->
        match alts.good_nf with
      | [alt_1; alt_2] ->
        let a = sprintf "%s_%d_A" base_name i in
        let b = sprintf "%s_%d_B" base_name i in
        let hpa = Instance.save_dep_svg ?domain (Filename.concat out_dir a) alt_1.instance in
        let hpb = Instance.save_dep_svg ?domain (Filename.concat out_dir b) alt_2.instance in
        let (afn,apos) = G_graph.get_annot_info alt_1.instance.Instance.graph
        and (bfn,bpos) = G_graph.get_annot_info alt_2.instance.Instance.graph in
        (base_name,i,(afn,apos),(bfn,bpos),(hpa,hpb))
      | _ -> Error.run "Not two alternatives in an annotation rewriting in %s" base_name
      ) t.good_nf

  let save_det_conll ?domain ?header base t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | ([],[]) ->
          let output =
            match header with
              | Some h -> sprintf "%% %s\n%s" h (Instance.to_conll_string ?domain t.instance)
              | None -> Instance.to_conll_string ?domain t.instance in
          File.write output (base^".conll")
        | ([one], []) -> loop one
        | _ -> Error.run "[save_det_conll] Not a single rewriting"
    in loop t

  let det_dep_string ?domain t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] ->
          let graph = t.instance.Instance.graph in
          Some (G_graph.to_dep ?domain graph)
        | [one], [] -> loop one
        | _ -> None
    in loop t

  let conll_dep_string ?domain ?(keep_empty_rh=false) t =
    if (not keep_empty_rh) && is_empty t
    then None
    else
      let rec loop t =
        match (t.good_nf, t.bad_nf) with
          | [],[] ->
            let graph = t.instance.Instance.graph in
            Some (G_graph.to_conll_string ?domain graph)
          | [one], [] -> loop one
          | _ -> None
      in loop t
end (* module Rewrite_history *)

(* ================================================================================ *)
module Modul = struct
  type t = {
    name: string;
    local_labels: (string * string list) array;
    rules: Rule.t list;
    filters: Rule.t list;
    confluent: bool;
    loc: Loc.t;
  }

  let check t =
    (* check for duplicate rules *)
    let rec loop already_defined = function
      | [] -> ()
      | r::_ when List.mem (Rule.get_name r) already_defined ->
        Error.build ~loc:(Rule.get_loc r) "Rule '%s' is defined twice in the same module" (Rule.get_name r)
      | r::tail -> loop ((Rule.get_name r) :: already_defined) tail in
    loop [] t.rules

  let build ?domain ast_module =
    let locals = Array.of_list ast_module.Ast.local_labels in
    Array.sort compare locals;
    let rules_or_filters = List.map (Rule.build ?domain ~locals ast_module.Ast.mod_dir) ast_module.Ast.rules in
    let (filters, rules) = List.partition Rule.is_filter rules_or_filters in
    let modul =
      {
        name = ast_module.Ast.module_id;
        local_labels = locals;
        rules;
        filters;
        confluent = ast_module.Ast.confluent;
        loc = ast_module.Ast.mod_loc;
      } in
    check modul; modul
end (* module Modul *)

(* ================================================================================ *)
module Grs = struct

  type t = {
    domain: Domain.t option;
    modules: Modul.t list;       (* the ordered list of modules used from rewriting *)
    strategies: Ast.strategy list;
    filename: string;
    ast: Ast.grs;
  }

  let get_modules t = t.modules
  let get_ast t = t.ast
  let get_filename t = t.filename

  let get_domain t = t.domain

  let sequence_names t = List.map (fun s -> s.Ast.strat_name) t.strategies

  let empty = {domain=None; modules=[]; strategies=[]; ast=Ast.empty_grs; filename=""; }

  let check t =
    (* check for duplicate modules *)
    let rec loop already_defined = function
      | [] -> ()
      | m::_ when List.mem m.Modul.name already_defined ->
        Error.build ~loc:m.Modul.loc "Module '%s' is defined twice" m.Modul.name
      | m::tail -> loop (m.Modul.name :: already_defined) tail in
    loop [] t.modules;

    (* check for duplicate strategies *)
    let rec loop already_defined = function
      | [] -> ()
      | s::_ when List.mem s.Ast.strat_name already_defined ->
        Error.build ~loc:s.Ast.strat_loc "Sequence '%s' is defined twice" s.Ast.strat_name
      | s::tail -> loop (s.Ast.strat_name :: already_defined) tail in
    loop [] t.strategies

  let domain_build ast_domain =
    Domain.build
      (Label_domain.build ast_domain.Ast.label_domain)
      (Feature_domain.build ast_domain.Ast.feature_domain)

  let build filename =
    let ast = Loader.grs filename in
    let domain = match ast.Ast.domain with None -> None | Some ast_dom -> Some (domain_build ast_dom) in
    let modules = List.map (Modul.build ?domain) ast.Ast.modules in
    let grs = {domain; strategies = ast.Ast.strategies; modules; ast; filename} in
    check grs;
    grs

  (* ---------------------------------------------------------------------------------------------------- *)
  let rewrite grs strategy_name graph =
    let strategy = List.find (fun s -> s.Ast.strat_name = strategy_name) grs.strategies in

    let rec old_loop instance module_list =
      match module_list with
      | [] -> {Rewrite_history.instance = instance; module_name = ""; good_nf = []; bad_nf = []; }
      | module_name :: tail ->
         let next =
           try List.find (fun m -> m.Modul.name=module_name) grs.modules
           with Not_found -> Log.fcritical "No module named '%s'" module_name in
        let (good_set, bad_set) =
          Rule.normalize
            ?domain: grs.domain
            next.Modul.name
            ~confluent: next.Modul.confluent
            next.Modul.rules
            next.Modul.filters
            (Instance.refresh instance) in
        let good_list = Instance_set.elements good_set
        and bad_list = Instance_set.elements bad_set in
        {
          Rewrite_history.instance = instance;
          module_name = next.Modul.name;
          good_nf = List.map (fun i -> old_loop i tail) good_list;
          bad_nf = bad_list;
        } in

    let loop instance def =
      match def with

      | Ast.Sequence module_list -> old_loop instance module_list
      | _ -> failwith "Not yet implemented" in

    loop (Instance.from_graph graph) (strategy.Ast.strat_def)

  (* [new_style grs module_list] return an equivalent strategy expressed with Seq, Pick and Star *)
  let new_style grs module_list =
    Ast.Seq
      (List.map
        (fun module_name ->
           let modul =
           try List.find (fun m -> m.Modul.name=module_name) grs.modules
           with Not_found -> Error.build "No module named '%s'" module_name in
           if modul.Modul.confluent
           then Ast.Pick (Ast.Star (Ast.Ref module_name))
           else Ast.Star (Ast.Ref module_name)
        ) module_list
      )

  (* [one_rewrite grs strat inst] tries to rewrite deterministically [inst] with [strat] defined in [grs] *)
  let one_rewrite grs strat inst =
    let rec loop inst = function
    (* name can refer to another strategy def or to a module *)
    | Ast.Ref name ->
      begin
        try
          let sub_strat = List.find (fun s -> s.Ast.strat_name = name) grs.strategies in
          loop inst sub_strat.Ast.strat_def
        with Not_found ->
          let modul =
            try List.find (fun m -> m.Modul.name=name) grs.modules
            with Not_found -> Error.build "No module or strategy named '%s'" name in
          Rule.conf_one_step ?domain: grs.domain name inst modul.Modul.rules
      end
    (* Union of strategies *)
    | Ast.Plus [] -> None (* the list can be empty in a recursive call! *)
    | Ast.Plus (head::tail) ->
      begin
        match loop inst head with
        | Some new_inst -> Some new_inst
        | None -> loop inst (Ast.Plus tail)
      end
    (* Sequence of strategies *)
    | Ast.Seq [] -> Log.fcritical "Empty sequence in strategy definition"
    | Ast.Seq [one] -> loop inst one
    | Ast.Seq (head::tail) ->
      begin
        match loop inst head with
        | Some new_inst -> loop new_inst (Ast.Seq tail)
        | None -> None
      end
    (* Interation of a strategy *)
    | Ast.Star sub_strat ->
      begin
        match loop inst sub_strat with
        | None -> Some inst
        | Some new_inst -> loop new_inst (Ast.Star sub_strat)
      end
    (* Pick *)
    | Ast.Pick sub_strat -> loop inst sub_strat
    (* Bang *)
    | Ast.Bang sub_strat -> loop inst (Ast.Star sub_strat)
    (* Try *)
    | Ast.Try sub_strat ->
      begin
        match loop inst sub_strat with
        | None -> Some inst
        | Some new_inst -> Some new_inst
      end
    (* Old style seq definition *)
    | Ast.Sequence module_list -> loop inst (new_style grs module_list) in
    loop inst strat


  let simple_rewrite grs strat_desc graph =
    let rec loop inst = function
    (* name can refer to another strategy def or to a module *)
    | Ast.Ref name ->
      begin
        try
          let sub_strat = List.find (fun s -> s.Ast.strat_name = name) grs.strategies in
          loop inst sub_strat.Ast.strat_def
        with Not_found ->
          let modul =
            try List.find (fun m -> m.Modul.name=name) grs.modules
            with Not_found -> Error.build "No module or strategy named '%s'" name in
          Rule.one_step ?domain: grs.domain name inst modul.Modul.rules
      end
    (* Union of strategies *)
    | Ast.Plus strat_list ->
      List.fold_left (fun acc strat -> Instance_set.union acc (loop inst strat)) Instance_set.empty strat_list
    (* Sequence of strategies *)
    | Ast.Seq [] -> Log.fcritical "Empty sequence in strategy definition"
    | Ast.Seq [one] -> loop inst one
    | Ast.Seq (head::tail) ->
      let after_first_mod = loop inst head in
      Instance_set.fold (fun new_inst acc -> Instance_set.union acc (loop new_inst (Ast.Seq tail))) after_first_mod Instance_set.empty
    (* Interation of a strategy *)
    | Ast.Star sub_strat ->
      let one_iter = loop inst sub_strat in
      if Instance_set.is_empty one_iter
      then Instance_set.singleton inst
      else Instance_set.fold (fun new_inst acc -> Instance_set.union acc (loop new_inst (Ast.Star sub_strat))) one_iter Instance_set.empty
    (* Pick *)
    | Ast.Pick sub_strat ->
      begin
        match one_rewrite grs sub_strat inst with
        | Some new_inst -> Instance_set.singleton new_inst
        | None -> Instance_set.empty
      end
    (* Try *)
    | Ast.Try sub_strat ->
      begin
        match one_rewrite grs sub_strat inst with
        | Some new_inst -> Instance_set.singleton new_inst
        | None -> Instance_set.singleton inst
      end
    (* Bang *)
    | Ast.Bang sub_strat -> loop inst (Ast.Pick (Ast.Star sub_strat))
    (* Old style seq definition *)
    | Ast.Sequence module_list -> loop inst (new_style grs module_list) in
    List.map
      (fun inst -> inst.Instance.graph)
      (Instance_set.elements (loop (Instance.from_graph graph) (Parser.strat_def strat_desc)))

  (* ---------------------------------------------------------------------------------------------------- *)
  (* construction of the rew_display *)
  let rec pick = function
    | Libgrew_types.Node (_, _, []) -> Log.bug "Empty node"; exit 12
    | Libgrew_types.Node (graph, name, (bs,rd)::_) -> Libgrew_types.Node (graph, "pick(" ^ name^")", [(bs, pick rd)])
    | x -> x

  let rec try_ = function
    | Libgrew_types.Node (_, _, []) -> Log.bug "Empty node"; exit 12
    | Libgrew_types.Node (graph, name, (bs,rd)::_) -> Libgrew_types.Node (graph, "try(" ^ name^")", [(bs, pick rd)])
    | x -> x

  (* ---------------------------------------------------------------------------------------------------- *)
  let rec clean = function
    | Libgrew_types.Empty -> Libgrew_types.Empty
    | Libgrew_types.Leaf graph -> Libgrew_types.Leaf graph
    | Libgrew_types.Local_normal_form (graph, name, Libgrew_types.Empty) -> Libgrew_types.Empty
    | Libgrew_types.Local_normal_form (graph, name, rd) -> Libgrew_types.Local_normal_form (graph, name, clean rd)
    | Libgrew_types.Node (graph, name, bs_rd_list) ->
        match
          List.fold_left (fun acc (bs,rd) ->
            match clean rd with
              | Libgrew_types.Empty -> acc
              | crd -> (bs, crd) :: acc
          ) [] bs_rd_list
        with
        | [] -> Libgrew_types.Empty
        | new_bs_rd_list -> Libgrew_types.Node (graph, name, new_bs_rd_list)


  (* ---------------------------------------------------------------------------------------------------- *)
  let build_rew_display grs strategy_name graph =
    let strategy = List.find (fun s -> s.Ast.strat_name = strategy_name) grs.strategies in

    let instance = Instance.from_graph graph in
    let rec old_loop instance module_list =
      match module_list with
      | [] -> Libgrew_types.Leaf instance.Instance.graph
      | next_name :: tail ->
         let next =
           try List.find (fun m -> m.Modul.name=next_name) grs.modules
           with Not_found -> Log.fcritical "No module named '%s'" next_name in
        let (good_set, bad_set) =
          Rule.normalize
            ?domain: grs.domain
            next.Modul.name
            ~confluent: next.Modul.confluent
            next.Modul.rules
            next.Modul.filters
            (Instance.refresh instance) in
        let inst_list = Instance_set.elements good_set
              (* and bad_list = Instance_set.elements bad_set *) in

        match inst_list with
          | [{Instance.big_step = None}] ->
            Libgrew_types.Local_normal_form (instance.Instance.graph, next.Modul.name, old_loop instance tail)
          | _ -> Libgrew_types.Node
            (
              instance.Instance.graph,
              next.Modul.name,
              List.map
                (fun inst ->
                  match inst.Instance.big_step with
                    | None -> Error.bug "Cannot have no big_steps and more than one reducts at the same time"
                    | Some bs -> (bs, old_loop inst tail)
                ) inst_list
            ) in 

    let indent = ref 10 in

    let rec apply_leaf strat_def = function
      | Libgrew_types.Empty -> Libgrew_types.Empty
      | Libgrew_types.Leaf graph -> loop (Instance.from_graph graph) strat_def
      | Libgrew_types.Local_normal_form (graph, name, rd) -> Libgrew_types.Local_normal_form (graph, name, apply_leaf strat_def rd)
      | Libgrew_types.Node (graph, name, bs_rd_list) -> Libgrew_types.Node (graph, name, List.map (fun (bs,rd) -> (bs, apply_leaf strat_def rd)) bs_rd_list)

    and loop instance strat_def =
      printf "%s===> loop  strat_def=%s\n%!"
        (String.make (2 * (max 0 !indent)) ' ')
        (Ast.strat_def_to_string strat_def);
      incr indent;

      match strat_def with

      | Ast.Sequence module_list -> old_loop instance module_list

      (* ========> reference to a module or to another strategy <========= *)
      | Ast.Ref name ->
        begin
          try
            let strategy = List.find (fun s -> s.Ast.strat_name = name) grs.strategies in
            loop instance strategy.Ast.strat_def
          with Not_found ->
            let modul =
              try List.find (fun m -> m.Modul.name=name) grs.modules
              with Not_found -> Log.fcritical "No [strategy or] module named '%s'" name in
            begin
              printf "%s one_step (module=%s)...%!" (String.make (2 * (max 0 !indent)) ' ') modul.Modul.name;
              let domain = get_domain grs in
              match Instance_set.elements (Rule.one_step ?domain modul.Modul.name instance modul.Modul.rules) with
              | [] -> printf "0\n%!"; let res = Libgrew_types.Empty in decr indent; res
              | instance_list -> printf "%d\n%!" (List.length instance_list);
                Libgrew_types.Node
                (instance.Instance.graph,
                  name,
                  List.map
                    (fun inst -> match inst.Instance.big_step with
                    | None -> Error.bug "Cannot have no big_steps and more than one reducts at the same time"
                    | Some bs -> let res = (bs, Libgrew_types.Leaf inst.Instance.graph) in decr indent; res
                    ) instance_list
                )
            end
        end

      (* ========> Strat defined as a sequence of sub-strategies <========= *)
      | Ast.Seq [] -> Log.bug "[Grs.build_rew_display] Empty sequence!"; exit 2
      | Ast.Seq [one] -> let res = loop instance one in decr indent; res
      | Ast.Seq (head_strat :: tail_strat) ->
        let one_step = loop instance head_strat in decr indent;
        apply_leaf (Ast.Seq tail_strat) one_step

      | Ast.Pick strat -> pick (loop instance strat)
      | Ast.Try strat -> try_ (loop instance strat)
      | Ast.Bang strat -> loop instance (Ast.Pick (Ast.Star strat))

      (* ========> Strat defined as a sequence of sub-strategies <========= *)
      | Ast.Plus [] -> Log.bug "[Grs.build_rew_display] Empty union!"; exit 2
      | Ast.Plus strat_list ->
        let rd_list = List.map (fun strat -> loop instance strat) strat_list in
        let (opt_lnf, opt_node_info) =
          List.fold_left (fun (acc_lnf, acc_node) rd ->
            match (rd, acc_lnf, acc_node) with
            | (Libgrew_types.Empty, acc_lnf, acc_node) -> (acc_lnf, acc_node)

            | (Libgrew_types.Leaf graph, None ,_) -> (Some (graph,"0"), acc_node)
            | (Libgrew_types.Leaf _,Some (graph,names) ,_) -> (Some (graph,"0+"^names), acc_node)

            | (Libgrew_types.Local_normal_form (graph,name,_), None, _) -> (Some (graph,name), acc_node)
            | (Libgrew_types.Local_normal_form (_,name,_), Some (graph,names), _) -> (Some (graph,name^"+"^names), acc_node)

            | (Libgrew_types.Node (graph,name,bs_rd_list), _, None) -> (acc_lnf, Some (graph,name,bs_rd_list))
            | (Libgrew_types.Node (_,name,bs_rd_list), _, Some (graph,acc_names,acc_bs_rd_list)) ->
                (acc_lnf, Some (graph, name^"+"^acc_names,bs_rd_list @ acc_bs_rd_list))
          ) (None,None) rd_list in
        begin
          match (opt_lnf, opt_node_info) with
            | (None, None) -> Libgrew_types.Empty
            | (Some (graph,lnf_name), None) -> Libgrew_types.Local_normal_form (graph, lnf_name, Libgrew_types.Leaf graph)
            | (None, Some (a,b,c)) -> Libgrew_types.Node (a,b,c)
            | (Some (_,lnf_name), Some (graph,acc_name,acc_bs_rd_list)) ->
                let bs = {Libgrew_types.first={Libgrew_types.rule_name="dummy";up=G_deco.empty;down=G_deco.empty}; small_step=[]} in
                Libgrew_types.Node (graph,acc_name,(bs, Libgrew_types.Leaf graph) :: acc_bs_rd_list)
        end

      | Ast.Star strat ->
        begin
          match clean (loop instance strat) with
          | Libgrew_types.Empty -> Libgrew_types.Leaf instance.Instance.graph
          | Libgrew_types.Local_normal_form _ -> Log.bug "dont know if 'Local_normal_form' in star should happen or not ???"; exit 1
          | rd -> apply_leaf (Ast.Star strat) rd
        end
      in

    loop instance (strategy.Ast.strat_def)

  (* ---------------------------------------------------------------------------------------------------- *)
  let rule_iter fct grs =
    List.iter
      (fun modul ->
        List.iter (fun rule -> fct modul.Modul.name rule) modul.Modul.rules
      ) grs.modules

  (* ---------------------------------------------------------------------------------------------------- *)
  let filter_iter fct grs =
    List.iter
      (fun modul ->
        List.iter (fun filter -> fct modul.Modul.name filter) modul.Modul.filters
      ) grs.modules
end (* module Grs *)
