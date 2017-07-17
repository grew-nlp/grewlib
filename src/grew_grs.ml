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
  }

  let rec get_graphs = function
    | { good_nf = []; instance } -> [instance.Instance.graph]
    | { good_nf = l} -> List_.flat_map get_graphs l

  let rec is_empty t =
    (t.instance.Instance.rules = []) && List.for_all is_empty t.good_nf

  let rec num_sol = function
    | { good_nf = [] } -> 1
    | { good_nf = l} -> List.fold_left (fun acc t -> acc + (num_sol t)) 0 l

  let save_nfs ?domain ?filter ?main_feat ~dot base_name t =
    let rec loop file_name rules t =
      match t.good_nf with
        | [] when dot -> Instance.save_dot_png ?domain ?filter ?main_feat file_name t.instance; [rules, file_name]
        | [] -> ignore (Instance.save_dep_png ?domain ?filter ?main_feat file_name t.instance); [rules, file_name]
        | l ->
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
      match t.good_nf with
        | [] -> File.write (Instance.to_gr ?domain t.instance) (file_name^".gr")
        | l -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_conll ?domain base t =
    let rec loop file_name t =
      match t.good_nf with
        | [] -> File.write (Instance.to_conll_string ?domain t.instance) (file_name^".conll")
        | l -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_full_conll ?domain base t =
    let cpt = ref 0 in
    let rec loop t =
      match t.good_nf with
        | [] ->
          File.write (Instance.to_conll_string ?domain t.instance) (sprintf "%s__%d.conll" base !cpt);
          incr cpt
        | l -> List.iter loop l
    in loop t; !cpt

  (* suppose that all modules are deterministic and produced exacly one normal form *)
  let save_det_gr ?domain base t =
    let rec loop t =
      match t.good_nf with
        | [] -> File.write (Instance.to_gr ?domain t.instance) (base^".gr")
        | [one] -> loop one
        | _ -> Error.run "[save_det_gr] Not a single rewriting"
    in loop t

  let save_det_conll ?domain ?header base t =
    let rec loop t =
      match t.good_nf with
        | [] ->
          let output =
            match header with
              | Some h -> sprintf "%% %s\n%s" h (Instance.to_conll_string ?domain t.instance)
              | None -> Instance.to_conll_string ?domain t.instance in
          File.write output (base^".conll")
        | [one] -> loop one
        | _ -> Error.run "[save_det_conll] Not a single rewriting"
    in loop t

  let det_dep_string ?domain t =
    let rec loop t =
      match t.good_nf with
        | [] ->
          let graph = t.instance.Instance.graph in
          Some (G_graph.to_dep ?domain graph)
        | [one] -> loop one
        | _ -> None
    in loop t

  let conll_dep_string ?domain ?(keep_empty_rh=false) t =
    if (not keep_empty_rh) && is_empty t
    then None
    else
      let rec loop t =
        match t.good_nf with
          | [] ->
            let graph = t.instance.Instance.graph in
            Some (G_graph.to_conll_string ?domain graph)
          | [one] -> loop one
          | _ -> None
      in loop t
end (* module Rewrite_history *)

(* ================================================================================ *)
module Modul = struct
  type t = {
    name: string;
    rules: Rule.t list;
    deterministic: bool;
    loc: Loc.t;
  }

  let to_json ?domain t =
    `Assoc [
      ("module_name", `String t.name);
      ("deterministic", `Bool t.deterministic);
      ("rules", `List (List.map (Rule.to_json ?domain) t.rules));
    ]

  let check t =
    (* check for duplicate rules *)
    let rec loop already_defined = function
      | [] -> ()
      | r::_ when List.mem (Rule.get_name r) already_defined ->
        Error.build ~loc:(Rule.get_loc r) "Rule '%s' is defined twice in the same module" (Rule.get_name r)
      | r::tail -> loop ((Rule.get_name r) :: already_defined) tail in
    loop [] t.rules

  let build ?domain ast_module =
    let rules = List.map (Rule.build ?domain ast_module.Ast.mod_dir) ast_module.Ast.rules in
    let modul =
      {
        name = ast_module.Ast.module_id;
        rules;
        deterministic = ast_module.Ast.deterministic;
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

  let to_json t = `List (List.map (Modul.to_json ?domain:t.domain) t.modules)

  let get_modules t = t.modules
  let get_ast t = t.ast
  let get_filename t = t.filename

  let get_domain t = t.domain

  let sequence_names t = List.map (fun s -> s.Ast.strat_name) t.strategies

  let empty = {domain=None; modules=[]; strategies=[]; ast=Ast.empty_grs; filename=""; }

  let check_strategy strat t =
    let rec loop = function
    | Ast.Ref name ->
      (if not (List.exists (fun m -> name = m.Modul.name) t.modules)
      then Error.build ~loc:strat.Ast.strat_loc "In sequence '%s' definition, module '%s' undefined" strat.Ast.strat_name name)
    | Ast.Seq sd_list -> List.iter loop sd_list
    | Ast.Plus sd_list -> List.iter loop sd_list
    | Ast.Star sd -> loop sd
    | Ast.Bang sd -> loop sd
    | Ast.Pick sd -> loop sd
    | Ast.Try sd -> loop sd
    | Ast.Sequence name_list ->
      List.iter (fun name ->
        if not (List.exists (fun m -> name = m.Modul.name) t.modules)
        then Error.build ~loc:strat.Ast.strat_loc "In sequence '%s' definition, module '%s' undefined" strat.Ast.strat_name name
      ) name_list
    in loop strat.Ast.strat_def

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
    loop [] t.strategies;

    (* check for undefined module or strategy *)
    List.iter (fun strat ->
      check_strategy strat t
    ) t.strategies



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
    let strategy =
      try List.find (fun s -> s.Ast.strat_name = strategy_name) grs.strategies
      with Not_found ->
        Error.run "[rewrite] Undefined stategy \"%s\"\nAvailable stategies: %s"
        strategy_name
        (String.concat "; " (List.map (fun s -> s.Ast.strat_name) grs.strategies)) in

    let rec old_loop instance module_list =
      match module_list with
      | [] -> {Rewrite_history.instance = instance; module_name = ""; good_nf = []; }
      | module_name :: tail ->
         let next =
           try List.find (fun m -> m.Modul.name=module_name) grs.modules
           with Not_found -> Log.fcritical "No module named '%s'" module_name in
        let good_set =
          Rule.normalize
            ?domain: grs.domain
            next.Modul.name
            ~deterministic: next.Modul.deterministic
            next.Modul.rules
            (Instance.refresh instance) in
        let good_list = Instance_set.elements good_set in
        {
          Rewrite_history.instance = instance;
          module_name = next.Modul.name;
          good_nf = List.map (fun i -> old_loop i tail) good_list;
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
           if modul.Modul.deterministic
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
          Rule.conf_one_step ?domain: grs.domain inst modul.Modul.rules
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
          Rule.one_step ?domain: grs.domain inst modul.Modul.rules
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
        let good_set =
          Rule.normalize
            ?domain: grs.domain
            next.Modul.name
            ~deterministic: next.Modul.deterministic
            next.Modul.rules
            (Instance.refresh instance) in
        let inst_list = Instance_set.elements good_set in

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
              match Instance_set.elements (Rule.one_step ?domain instance modul.Modul.rules) with
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

end (* module Grs *)























module New_grs = struct

  type decl =
  | Rule of Rule.t
  | Strategy of string * New_ast.strat
  | Package of string * decl list

  type t = {
    filename: string;
    domain: Domain.t option;
    decls: decl list;
    ast: New_ast.grs;
  }

  let get_strat_list grs = Grew_ast.New_ast.strat_list grs.ast

  let rec dump_decl indent = function
    | Rule r -> printf "%srule %s\n" (String.make indent ' ') (Rule.get_name r)
    | Strategy (name, def) -> printf "%sstrat %s\n" (String.make indent ' ') name
    | Package (name, decl_list) ->
      printf "%spackage %s:\n" (String.make indent ' ') name;
      List.iter (dump_decl (indent + 2)) decl_list

  let dump t =
    printf "================ New_grs ================\n";
    Domain.dump t.domain;
    printf "-----------------------\n";
    List.iter (dump_decl 0) t.decls;
    printf "================ New_grs ================\n%!";
    ()


  let rec build_decl ?domain = function
  | New_ast.Package (loc, name, decl_list) -> Package (name, List.map (build_decl ?domain) decl_list)
  | New_ast.Rule ast_rule -> Rule (Rule.build ?domain "TODO: remove this arg (old grs)" ast_rule)
  | New_ast.Strategy (loc, name, ast_strat) -> Strategy (name, ast_strat)
  | _ -> Error.bug "[build_decl] Inconsistent ast for new_grs"

  let domain t = t.domain

  let load filename =
    let ast = Loader.new_grs filename in

    let feature_domains = List_.opt_map
      (fun x -> match x with
        | New_ast.Features desc -> Some desc
        | _ -> None
      ) ast in
    let feature_domain = match feature_domains with
    | [] -> None
    | h::t -> Some (Feature_domain.build (List.fold_left Feature_domain.merge h t)) in

    let label_domains = List_.opt_map
      (fun x -> match x with
        | New_ast.Labels desc -> Some desc
        | _ -> None
      ) ast in
    let label_domain = match label_domains with
    | [] -> None
    | h::t -> Some (Label_domain.build (List.fold_left Label_domain.merge h t)) in

    let domain = match (label_domain, feature_domain) with
    | (None, None) -> None
    | (Some ld, None) -> Some (Domain.build_labels_only ld)
    | (None, Some fd) -> Some (Domain.build_features_only fd)
    | (Some ld, Some fd) -> Some (Domain.build ld fd) in

    let decls = List_.opt_map
      (fun x -> match x with
        | New_ast.Features _ -> None
        | New_ast.Labels _ -> None
        | New_ast.Import _ -> Error.bug "[load] Import: inconsistent ast for new_grs"
        | New_ast.Include _ -> Error.bug "[load] Inlcude: inconsistent ast for new_grs"
        | x -> Some (build_decl ?domain x)
      ) ast in

    { filename;
      ast;
      domain;
      decls;
    }

  (* The type [pointed] is a zipper style data structure for resolving names x.y.z *)
  type pointed =
  | Top of decl list
  | Pack of (decl list * pointed)  (* (content, mother package) *)





  let top grs = Top grs.decls

  let decl_list = function
  | Top dl -> dl
  | Pack (dl, _) -> dl

  let down pointed name =
    let rec loop = function
    | [] -> None
    | Package (n,dl) :: _ when n=name -> Some (Pack (dl, pointed))
    | _::t -> loop t in
    loop (decl_list pointed)


  (* search for a decl named [name] defined in the working directory [wd] in [grs] *)
  let rec search_at pointed path = match path with
    | [] -> None
    | [one] ->
      (
        try
          let item = List.find (* ?? rule and strategy with the same name ?? *)
            (function
              | Strategy (s,_) when s=one -> true
              | Rule r when Rule.get_name r = one -> true
              | Package (p,_) when p=one -> true
              | _ -> false
            ) (decl_list pointed) in
          Some (item, pointed)
        with Not_found -> None
      )
    | head::tail ->
      match down pointed head with
      | None -> None
      | Some new_p -> search_at new_p tail

  (* search for the path in current location and recursively on mother structure *)
  let rec search_from pointed path =
    match search_at pointed path with
      | Some r_or_s -> Some r_or_s
      | None ->
      (match pointed with
        | Top _ -> None
        | Pack (_,mother) -> search_from mother path
      )

  (* det apply a package to an instance = apply only top level rules in the package *)
  let det_pack_rewrite ?domain decl_list instance =
    let rec loop = function
      | [] -> None
      | Rule r :: tail_decl ->
        (match Rule.det_apply ?domain r instance with
        | Some x -> Some x
        | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
      loop decl_list

  (* apply a package to an instance = apply only top level rules in the package *)
  let pack_rewrite ?domain decl_list instance =
    List.fold_left
      (fun acc decl -> match decl with
        | Rule r -> Instance_set.union acc (Rule.apply ?domain r instance)
        | _ -> acc
      ) Instance_set.empty decl_list

  (* deterministic case *)
  let rec det_intern_simple_rewrite ?domain pointed strat_name instance =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) -> Rule.det_apply ?domain r instance
    | Some (Package (_, decl_list), _) -> det_pack_rewrite ?domain decl_list instance
    | Some (Strategy (_,ast_strat), new_pointed) ->
      det_strat_simple_rewrite new_pointed ast_strat instance

  and det_strat_simple_rewrite ?domain pointed strat instance =
    match strat with
    | New_ast.Ref subname -> det_intern_simple_rewrite ?domain pointed subname instance
    | New_ast.Pick strat -> det_strat_simple_rewrite ?domain pointed strat instance

    | New_ast.Alt [] -> None
    | New_ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match det_strat_simple_rewrite ?domain pointed head_strat instance with
          | None -> loop tail_strat
          | Some x -> Some x in
        loop strat_list

    | New_ast.Seq [] -> Some instance
    | New_ast.Seq (head_strat :: tail_strat) ->
      begin
        match det_strat_simple_rewrite ?domain pointed head_strat instance with
        | None -> None
        | Some inst -> det_strat_simple_rewrite ?domain pointed (New_ast.Seq tail_strat) inst
      end

    | New_ast.Iter strat ->
      begin
        match det_strat_simple_rewrite ?domain pointed strat instance with
        | None -> Some instance
        | Some inst -> det_strat_simple_rewrite ?domain pointed (New_ast.Iter strat) inst
        end

    | New_ast.Try strat ->
        begin
          match det_strat_simple_rewrite ?domain pointed strat instance with
          | None -> Some instance
          | Some i -> Some i
        end

    | New_ast.If (s, s1, s2) ->
      begin
        match det_strat_simple_rewrite ?domain pointed s instance with
        | None -> det_strat_simple_rewrite ?domain pointed s1 instance
        | Some _ -> det_strat_simple_rewrite ?domain pointed s2 instance
      end

  (* non deterministic case *)
  let rec intern_simple_rewrite ?domain pointed strat_name instance =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) -> Rule.apply r instance
    | Some (Package (_, decl_list), _) -> pack_rewrite decl_list instance
    | Some (Strategy (_,ast_strat), new_pointed) ->
      strat_simple_rewrite ?domain new_pointed ast_strat instance

  and strat_simple_rewrite ?domain pointed strat instance =
    match strat with
    | New_ast.Ref subname -> intern_simple_rewrite ?domain pointed subname instance
    | New_ast.Pick strat ->
      begin
        match det_strat_simple_rewrite ?domain pointed strat instance with
        | None -> Grew_rule.Instance_set.empty
        | Some x -> Instance_set.singleton x
      end

    | New_ast.Alt [] -> Grew_rule.Instance_set.empty
    | New_ast.Alt strat_list -> List.fold_left
      (fun acc strat -> Instance_set.union acc (strat_simple_rewrite ?domain pointed strat instance)
      ) Instance_set.empty strat_list

    | New_ast.Seq [] -> Instance_set.singleton instance
    | New_ast.Seq (head_strat :: tail_strat) ->
      let first_strat = strat_simple_rewrite ?domain pointed head_strat instance in
      Instance_set.fold
        (fun instance acc -> Instance_set.union acc (strat_simple_rewrite ?domain pointed (New_ast.Seq tail_strat) instance)
        ) first_strat Instance_set.empty

    | New_ast.Iter strat ->
      let one_step = strat_simple_rewrite ?domain pointed strat instance in
      if Instance_set.is_empty one_step
      then Instance_set.singleton instance
      else Instance_set.fold
        (fun instance acc -> Instance_set.union acc (strat_simple_rewrite ?domain pointed (New_ast.Iter strat) instance)
        ) one_step Instance_set.empty

    | New_ast.Try strat ->
      begin
        let one_step = strat_simple_rewrite ?domain pointed strat instance in
        if Instance_set.is_empty one_step
        then Instance_set.singleton instance
        else one_step
      end

    | New_ast.If (s, s1, s2) ->
      begin
        match det_strat_simple_rewrite ?domain pointed s instance with
        | None -> strat_simple_rewrite ?domain pointed s1 instance
        | Some _ -> strat_simple_rewrite ?domain pointed s2 instance
      end


  let simple_rewrite grs strat graph =
    let domain = domain grs in
    let instance = Instance.from_graph graph in
    let set = strat_simple_rewrite ?domain (top grs) (Parser.strategy strat) instance in
    List.map
      (fun inst -> inst.Instance.graph)
      (Instance_set.elements set)





  let det_pack_one ?domain decl_list instance =
    let rec loop = function
      | [] -> None
      | Rule r :: tail_decl ->
        (match Rule.det_apply ?domain r instance with
        | Some x -> Some (x, Rule.get_name r)
        | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
      loop decl_list



  let det_iter_pack ?domain decl_list instance = (* return a (big step, inst) *)
    match det_pack_one decl_list instance with
    | None -> None
    | Some (x, rule_name) ->
      let first = {Libgrew_types.rule_name; up=G_deco.empty; down=G_deco.empty} in
      let rec loop inst =
        match det_pack_one ?domain decl_list inst with
        | None -> ([], inst)
        | Some (next, rule_name) ->
            let (tail, final) = loop next in
            (
              (inst.Instance.graph, {Libgrew_types.rule_name; up=G_deco.empty; down=G_deco.empty} ) :: tail,
              final
            ) in
      let (small_step, final) = loop x in
      Some ({ Libgrew_types.first; small_step }, final)

  let rec det_rew_display_tmp ?domain pointed strat instance =
    match strat with
    | New_ast.Ref subname ->
      let path = Str.split (Str.regexp "\\.") subname in
      begin
        match search_from pointed path with
        | None -> Error.build "Simple rewrite, cannot find strat %s" subname
        | Some (Rule r,_) ->
          begin
            match Rule.det_apply ?domain r instance with
            | None -> None
            | Some inst -> Some [(
                Rule.get_name r,
                { Libgrew_types.first = {Libgrew_types.rule_name=Rule.get_name r; up=G_deco.empty; down=G_deco.empty}; small_step = [] },
                inst
              )]
          end
        | Some (Package (_, decl_list), _) ->
            begin
              match det_pack_one ?domain decl_list instance with
              | None -> None
              | Some (inst,rule_name) -> Some [(
                  rule_name,
                  { Libgrew_types.first = {Libgrew_types.rule_name=rule_name; up=G_deco.empty; down=G_deco.empty}; small_step = [] },
                  inst
                )]
            end
        | Some (Strategy (_,ast_strat), new_pointed) ->
            det_rew_display_tmp ?domain new_pointed ast_strat instance
      end

    | New_ast.Pick strat -> det_rew_display_tmp ?domain pointed strat instance

    | New_ast.Alt [] -> None
    | New_ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match det_rew_display_tmp ?domain pointed head_strat instance with
          | None -> loop tail_strat
          | Some x -> Some x in
        loop strat_list

    | New_ast.Seq [] -> Some []
    | New_ast.Seq (head_strat :: tail_strat) ->
      begin
        match det_rew_display_tmp ?domain pointed head_strat instance with
        | None -> None
        | Some [] -> det_rew_display_tmp ?domain pointed (New_ast.Seq tail_strat) instance
        | Some (((_,_,inst) :: _) as l) ->
          begin
            match det_rew_display_tmp ?domain pointed (New_ast.Seq tail_strat) inst with
            | None -> None
            | Some l2 -> Some (l2 @ l)
          end
      end

    | New_ast.Iter (New_ast.Ref subname) ->
      let path = Str.split (Str.regexp "\\.") subname in
        begin
          match search_from pointed path with
          | None -> Error.build "Simple rewrite, cannot find strat %s" subname
          | Some (Rule r,_) ->
            begin
              match det_iter_pack [Rule r] instance with
              | Some (big_step, final) -> Some [(Rule.get_name r, big_step, final)]
              | None -> Some []
            end
          | Some (Package (pack_name, decl_list), _) ->
            begin
              match det_iter_pack decl_list instance with
              | Some (big_step, final) -> Some [(pack_name, big_step, final)]
              | None -> Some []
            end
          | Some (Strategy (_,ast_strat), new_pointed) ->
              det_rew_display_tmp ?domain new_pointed ast_strat instance
        end

    | New_ast.Iter strat ->
      begin
        match det_rew_display_tmp ?domain pointed strat instance with
        | None -> Some []
        | Some [] -> Some []
        | Some (((_,_,inst) :: _) as l) ->
          begin
            match det_rew_display_tmp ?domain pointed (New_ast.Iter strat) inst with
            | None -> Some l
            | Some l2 -> Some (l2 @ l)
          end
      end

    | New_ast.Try strat ->
        begin
          match det_rew_display_tmp ?domain pointed strat instance with
          | None -> Some []
          | Some i -> Some i
        end

    | New_ast.If (s, s1, s2) ->
      begin
        match det_strat_simple_rewrite ?domain pointed s instance with
        | None -> det_rew_display_tmp ?domain pointed s1 instance
        | Some _ -> det_rew_display_tmp ?domain pointed s2 instance
      end

  let det_rew_display grs strat graph =
    let domain = domain grs in
    let instance = Instance.from_graph graph in
    let rec loop (s,b,rd) = function
    | [] -> Libgrew_types.Node (instance.Instance.graph, s, [b, rd])
    | (s2, b2, i2) :: tail -> loop (s2, b2, Libgrew_types.Node (i2.Instance.graph,s,[b,rd])) tail in

    match det_rew_display_tmp ?domain (top grs) (Parser.strategy strat) instance with
    | None -> Libgrew_types.Empty
    | Some [] -> Libgrew_types.Leaf instance.Instance.graph
    | Some ((s1,b1,i1) :: tail) -> loop (s1,b1,Libgrew_types.Leaf i1.Instance.graph) tail

end

