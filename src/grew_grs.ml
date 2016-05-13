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

  let rec is_empty t =
    (t.instance.Instance.rules = []) && List.for_all is_empty t.good_nf

  let rec num_sol = function
    | { good_nf = []; bad_nf = [] } -> 1
    | { good_nf = [] } -> 0 (* dead branch *)
    | { good_nf = l} -> List.fold_left (fun acc t -> acc + (num_sol t)) 0 l

  let save_nfs label_domain ?filter ?main_feat ~dot base_name t =
    let rec loop file_name rules t =
      match (t.good_nf, t.bad_nf) with
        | [],[] when dot -> Instance.save_dot_png label_domain ?filter ?main_feat file_name t.instance; [rules, file_name]
        | [],[] -> ignore (Instance.save_dep_png label_domain ?filter ?main_feat file_name t.instance); [rules, file_name]
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
            [] l
    in loop base_name [] t

  let save_gr label_domain base t =
    let rec loop file_name t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_gr label_domain t.instance) (file_name^".gr")
        | l, _ -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_conll label_domain base t =
    let rec loop file_name t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_conll_string label_domain t.instance) (file_name^".conll")
        | l, _ -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_full_conll label_domain base t =
    let cpt = ref 0 in
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> 
          File.write (Instance.to_conll_string label_domain t.instance) (sprintf "%s__%d.conll" base !cpt);
          incr cpt
        | l, _ -> List.iter loop l
    in loop t; !cpt

  (* suppose that all modules are confluent and produced exacly one normal form *)
  let save_det_gr label_domain base t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_gr label_domain t.instance) (base^".gr")
        | [one], [] -> loop one
        | _ -> Error.run "[save_det_gr] Not a single rewriting"
    in loop t

  let save_annot label_domain out_dir base_name t =
    List.mapi
      (fun i alts ->
        match alts.good_nf with
      | [alt_1; alt_2] ->
        let a = sprintf "%s_%d_A" base_name i in
        let b = sprintf "%s_%d_B" base_name i in
        let hpa = Instance.save_dep_svg label_domain (Filename.concat out_dir a) alt_1.instance in
        let hpb = Instance.save_dep_svg label_domain (Filename.concat out_dir b) alt_2.instance in
        let (afn,apos) = G_graph.get_annot_info alt_1.instance.Instance.graph
        and (bfn,bpos) = G_graph.get_annot_info alt_2.instance.Instance.graph in
        (base_name,i,(afn,apos),(bfn,bpos),(hpa,hpb))
      | _ -> Error.run "Not two alternatives in an annotation rewriting in %s" base_name
      ) t.good_nf

  let save_det_conll label_domain ?header base t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | ([],[]) ->
          let output =
            match header with
              | Some h -> sprintf "%% %s\n%s" h (Instance.to_conll_string label_domain t.instance)
              | None -> Instance.to_conll_string label_domain t.instance in
          File.write output (base^".conll")
        | ([one], []) -> loop one
        | _ -> Error.run "[save_det_conll] Not a single rewriting"
    in loop t

  let det_dep_string label_domain t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] ->
          let graph = t.instance.Instance.graph in
          Some (G_graph.to_dep label_domain graph)
        | [one], [] -> loop one
        | _ -> None
    in loop t

  let conll_dep_string label_domain ?(keep_empty_rh=false) t =
    if (not keep_empty_rh) && is_empty t
    then None
    else
      let rec loop t =
        match (t.good_nf, t.bad_nf) with
          | [],[] ->
            let graph = t.instance.Instance.graph in
            Some (G_graph.to_conll_string label_domain graph)
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

  let build domain ast_module =
    let locals = Array.of_list ast_module.Ast.local_labels in
    Array.sort compare locals;
    let rules_or_filters = List.map (Rule.build domain ~locals ast_module.Ast.mod_dir) ast_module.Ast.rules in
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
module Sequence = struct
  type t = {
    name: string;
    def: string list;
    loc: Loc.t;
  }

  let check module_list t =
    List.iter
      (fun module_name ->
        if not (List.exists (fun modul -> modul.Modul.name = module_name) module_list)
        then Error.build ~loc:t.loc "sequence \"%s\" refers to the unknown module \"%s\"."
          t.name module_name
      ) t.def

  let build module_list ast_sequence =
    match ast_sequence with
    | Ast.New ((n,_),s) ->
    printf "----%s----> %s\n%!" n (Ast.new_sequence_to_string s);
    printf "====%s====> %s\n%!" n (Ast.new_sequence_to_string (Ast.flatten s));
    {name=n; def=[]; loc=Loc.file "No_file_given"; }
    | Ast.Old old_seq ->
    let sequence =
      {
        name = old_seq.Ast.seq_name;
        def = old_seq.Ast.seq_mod;
        loc = old_seq.Ast.seq_loc;
      } in
    check module_list sequence; sequence
end (* module Sequence *)

(* ================================================================================ *)
module Grs = struct

  type t = {
    domain: Domain.t;
    modules: Modul.t list;       (* the ordered list of modules used from rewriting *)
    sequences: Sequence.t list;
    filename: string;
    ast: Ast.grs;
  }

  let get_modules t = t.modules
  let get_ast t = t.ast
  let get_filename t = t.filename
  let get_domain t = t.domain
  let sequence_names t = List.map (fun s -> s.Sequence.name) t.sequences

  let empty = {domain=Domain.empty; modules=[]; sequences=[]; ast=Ast.empty_grs; filename=""; }

  let check t =
    (* check for duplicate modules *)
    let rec loop already_defined = function
      | [] -> ()
      | m::_ when List.mem m.Modul.name already_defined ->
        Error.build ~loc:m.Modul.loc "Module '%s' is defined twice" m.Modul.name
      | m::tail -> loop (m.Modul.name :: already_defined) tail in
    loop [] t.modules;

    (* check for duplicate sequences *)
    let rec loop already_defined = function
      | [] -> ()
      | s::_ when List.mem s.Sequence.name already_defined ->
        Error.build ~loc:s.Sequence.loc "Sequence '%s' is defined twice" s.Sequence.name
      | s::tail -> loop (s.Sequence.name :: already_defined) tail in
    loop [] t.sequences

  let domain_build ast_domain =
    Domain.build
      (Label_domain.build ast_domain.Ast.label_domain)
      (Feature_domain.build ast_domain.Ast.feature_domain)

  let build filename =
    let ast = Loader.grs filename in
    let domain = domain_build ast.Ast.domain in
    let modules = List.map (Modul.build domain) ast.Ast.modules in
    let grs = {domain; sequences = List.map (Sequence.build modules) ast.Ast.sequences; modules; ast; filename} in
    check grs;
    grs

  (* compute the list of modules to apply for a requested sentence *)
  let modules_of_sequence grs sequence =
    try
      let seq = List.find (fun s -> s.Sequence.name = sequence) grs.sequences in
      List.map (fun name -> List.find (fun m -> m.Modul.name=name) grs.modules) seq.Sequence.def
    with Not_found ->
      try
        let modul = List.find (fun m -> m.Modul.name=sequence) grs.modules in
        Log.fwarning "\"%s\" is a module but not a senquence, only this module is used" sequence; [modul]
      with Not_found ->
        match grs.sequences with
        | head::_ ->
          Log.fwarning "No sequence and no module named \"%s\", the first sequence \"%s\" is used" sequence head.Sequence.name;
          List.map (fun name -> List.find (fun m -> m.Modul.name=name) grs.modules) head.Sequence.def
        | _ -> Error.run "No sequence defined and no module named \"%s\", cannot go on" sequence

  let rewrite grs sequence graph =
    let instance = Instance.from_graph graph in
    Timeout.start ();
    let modules_to_apply = modules_of_sequence grs sequence in

    let rec loop instance module_list =
      match module_list with
      | [] -> (* no more modules to apply *)
        {Rewrite_history.instance = instance; module_name = ""; good_nf = []; bad_nf = []; }
      | next::tail ->
        let (good_set, bad_set) =
          Rule.normalize
            grs.domain
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

  let build_rew_display grs sequence graph =
    let instance = Instance.from_graph graph in
    let modules_to_apply = modules_of_sequence grs sequence in

    let rec loop instance module_list =
      match module_list with
      | [] -> Libgrew_types.Leaf instance.Instance.graph
      | next :: tail ->
        let (good_set, bad_set) =
          Rule.normalize
            grs.domain
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
                    | None -> Error.bug "Cannot have no big_steps and more than one reducts at the same time"
                    | Some bs -> (bs, loop inst tail)
                ) inst_list
            )
    in loop instance modules_to_apply

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
