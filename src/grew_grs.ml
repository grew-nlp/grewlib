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
    strategies: Strategy.t list;
    filename: string;
    ast: Ast.grs;
  }

  let get_modules t = t.modules
  let get_ast t = t.ast
  let get_filename t = t.filename

  let get_domain t = t.domain

  let sequence_names t = List.map (fun s -> s.Strategy.name) t.strategies

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
      | s::_ when List.mem s.Strategy.name already_defined ->
        Error.build ~loc:s.Strategy.loc "Sequence '%s' is defined twice" s.Strategy.name
      | s::tail -> loop (s.Strategy.name :: already_defined) tail in
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

  (* compute the list of modules to apply for a requested sentence *)

  let rewrite grs sequence graph =
    let instance = Instance.from_graph graph in
    Timeout.start ();
    let modules_to_apply = [] (* modules_of_sequence grs sequence *) in

    let rec loop instance module_list =
      match module_list with
      | [] -> (* no more modules to apply *)
        {Rewrite_history.instance = instance; module_name = ""; good_nf = []; bad_nf = []; }
      | next::tail ->
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
          good_nf = List.map (fun i -> loop i tail) good_list;
          bad_nf = bad_list;
        } in
    loop instance modules_to_apply

  (* construction of the rew_display *)
  let rec diamond = function
    | Libgrew_types.Node (_, _, []) -> Log.bug "Empty node"; exit 12
    | Libgrew_types.Node (graph, name, (bs,rd)::_) -> Libgrew_types.Node (graph, "◇" ^ name, [(bs, diamond rd)])
    | x -> x

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

  let build_rew_display grs strategy graph =
    let instance = Instance.from_graph graph in

(* =============
    let rec loop instance module_list =
      match module_list with
      | [] -> Libgrew_types.Leaf instance.Instance.graph
      | next :: tail ->
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
            Libgrew_types.Local_normal_form (instance.Instance.graph, next.Modul.name, loop instance tail)
          | _ -> Libgrew_types.Node
            (
              instance.Instance.graph,
              next.Modul.name,
              List.map
                (fun inst ->
                  match inst.Instance.big_step with
=======  *)
    let indent = ref 10 in

    let strat = List.find (fun s -> s.Strategy.name = strategy) grs.strategies in

    let rec apply_leaf strat = function
      | Libgrew_types.Empty -> Libgrew_types.Empty
      | Libgrew_types.Leaf graph -> loop (Instance.from_graph graph) strat
      | Libgrew_types.Local_normal_form (graph, name, rd) -> Libgrew_types.Local_normal_form (graph, name, apply_leaf strat rd)
      | Libgrew_types.Node (graph, name, bs_rd_list) -> Libgrew_types.Node (graph, name, List.map (fun (bs,rd) -> (bs, apply_leaf strat rd)) bs_rd_list)

    and loop (instance : Instance.t) def =
      printf "%s===> loop  def=%s\n%!"
        (String.make (2 * (max 0 !indent)) ' ')
        (Strategy.to_string def);
      incr indent;

      match def with

      (* ========> reference to a module or to another strategy <========= *)
      | Strategy.Ref name ->
        begin
          try
            let strat = List.find (fun s -> s.Strategy.name = name) grs.strategies in
            loop instance strat.Strategy.def
          with Not_found ->
            let modul =
              try List.find (fun m -> m.Modul.name=name) grs.modules
              with Not_found -> Log.fcritical "No [strategy or] module named '%s'" name in
            begin
              printf "%s one_step (module=%s)...%!" (String.make (2 * (max 0 !indent)) ' ') modul.Modul.name;
              let domain = get_domain grs in
              match Rule.one_step ?domain modul.Modul.name instance modul.Modul.rules with
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
      | Strategy.Seq [] -> Log.bug "[Grs.build_rew_display] Empty sequence!"; exit 2
      | Strategy.Seq [one] -> let res = loop instance one in decr indent; res
      | Strategy.Seq (head_strat :: tail_strat) ->
        let one_step = loop instance head_strat in decr indent;
        apply_leaf (Strategy.Seq tail_strat) one_step

      | Strategy.Diamond strat -> diamond (loop instance strat)

      (* ========> Strat defined as a sequence of sub-strategies <========= *)
      | Strategy.Plus [] -> Log.bug "[Grs.build_rew_display] Empty union!"; exit 2
      | Strategy.Plus strat_list ->
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

      | Strategy.Star strat ->
        begin
          match clean (loop instance strat) with
          | Libgrew_types.Empty -> Libgrew_types.Leaf instance.Instance.graph
          | Libgrew_types.Local_normal_form _ -> Log.bug "dont know if 'Local_normal_form' in star should happen or not ???"; exit 1
          | rd -> apply_leaf (Strategy.Star strat) rd
        end
      in

    loop instance (strat.Strategy.def)

  let rule_iter fct grs =
    List.iter
      (fun modul ->
        List.iter (fun rule -> fct modul.Modul.name rule) modul.Modul.rules
      ) grs.modules

  let filter_iter fct grs =
    List.iter
      (fun modul ->
        List.iter (fun filter -> fct modul.Modul.name filter) modul.Modul.filters
      ) grs.modules
end (* module Grs *)
