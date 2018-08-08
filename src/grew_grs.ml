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

  let save_gr base t =
    let rec loop file_name t =
      match t.good_nf with
        | [] -> File.write (Instance.to_gr t.instance) (file_name^".gr")
        | l -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_conll base t =
    let rec loop file_name t =
      match t.good_nf with
        | [] -> File.write (Instance.to_conll_string t.instance) (file_name^".conll")
        | l -> List.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_full_conll base t =
    let cpt = ref 0 in
    let rec loop t =
      match t.good_nf with
        | [] ->
          File.write (Instance.to_conll_string t.instance) (sprintf "%s__%d.conll" base !cpt);
          incr cpt
        | l -> List.iter loop l
    in loop t; !cpt

  (* suppose that all modules are deterministic and produced exacly one normal form *)
  let save_det_gr base t =
    let rec loop t =
      match t.good_nf with
        | [] -> File.write (Instance.to_gr t.instance) (base^".gr")
        | [one] -> loop one
        | _ -> Error.run "[save_det_gr] Not a single rewriting"
    in loop t

  let save_det_conll ?header base t =
    let rec loop t =
      match t.good_nf with
        | [] ->
          let output =
            match header with
              | Some h -> sprintf "%% %s\n%s" h (Instance.to_conll_string t.instance)
              | None -> Instance.to_conll_string t.instance in
          File.write output (base^".conll")
        | [one] -> loop one
        | _ -> Error.run "[save_det_conll] Not a single rewriting"
    in loop t

  let det_dep_string t =
    let rec loop t =
      match t.good_nf with
        | [] ->
          let graph = t.instance.Instance.graph in
          Some (G_graph.to_dep graph)
        | [one] -> loop one
        | _ -> None
    in loop t

  let conll_dep_string ?(keep_empty_rh=false) t =
    if (not keep_empty_rh) && is_empty t
    then None
    else
      let rec loop t =
        match t.good_nf with
          | [] ->
            let graph = t.instance.Instance.graph in
            Some (G_graph.to_conll_string graph)
          | [one] -> loop one
          | _ -> None
      in loop t
end (* module Rewrite_history *)

(* ================================================================================ *)
module Grs = struct

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

  let rec decl_to_json ?domain = function
    | Rule r -> Rule.to_json ?domain r
    | Strategy (name, strat) -> `Assoc [("strat_name", `String name); ("strat_def", New_ast.strat_to_json strat)]
    | Package (name, decl_list) -> `Assoc [("package_name", `String name); "decls", `List (List.map (decl_to_json ?domain) decl_list)]

  let to_json t =
    match t.domain with
    | None -> `Assoc [
      "filename", `String t.filename;
      "decls", `List (List.map decl_to_json t.decls)
    ]
    | Some dom -> `Assoc [
      "domain", Domain.to_json dom;
      "filename", `String t.filename;
      "decls", `List (List.map (decl_to_json ~domain:dom) t.decls)
    ]


  let get_strat_list grs = Grew_ast.New_ast.strat_list grs.ast

  let rec dump_decl indent = function
    | Rule r -> printf "%srule %s\n" (String.make indent ' ') (Rule.get_name r)
    | Strategy (name, def) -> printf "%sstrat %s\n" (String.make indent ' ') name
    | Package (name, decl_list) ->
      printf "%spackage %s:\n" (String.make indent ' ') name;
      List.iter (dump_decl (indent + 2)) decl_list

  let dump t =
    printf "================ Grs ================\n";
    Domain.dump t.domain;
    printf "-----------------------\n";
    List.iter (dump_decl 0) t.decls;
    printf "================ Grs ================\n%!";
    ()


  let rec build_decl ?domain = function
  | New_ast.Package (loc, name, decl_list) -> Package (name, List.map (build_decl ?domain) decl_list)
  | New_ast.Rule ast_rule -> Rule (Rule.build ?domain "TODO: remove this arg (old grs)" ast_rule)
  | New_ast.Strategy (loc, name, ast_strat) -> Strategy (name, ast_strat)
  | _ -> Error.bug "[build_decl] Inconsistent ast for new_grs"

  let domain t = t.domain

  let domain_build ast_domain =
    Domain.build
      (Label_domain.build ast_domain.Ast.label_domain)
      (Feature_domain.build ast_domain.Ast.feature_domain)

  let from_ast filename ast =
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
        | New_ast.Conll_fields _ -> None
        | New_ast.Import _ -> Error.bug "[load] Import: inconsistent ast for new_grs"
        | New_ast.Include _ -> Error.bug "[load] Include: inconsistent ast for new_grs"
        | x -> Some (build_decl ?domain x)
      ) ast in

    { filename;
      ast;
      domain;
      decls;
    }

  let load filename = from_ast filename (Loader.new_grs filename)

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
    | New_ast.Onf s -> det_strat_simple_rewrite ?domain pointed (New_ast.Pick (New_ast.Iter s)) instance
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
    | New_ast.Onf s -> strat_simple_rewrite ?domain pointed (New_ast.Pick (New_ast.Iter s)) instance
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
        | Some x -> Some x
        | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
      loop decl_list

  let det_iter_pack ?domain decl_list instance =
    match det_pack_one ?domain decl_list instance with
    | None -> None
    | Some x ->
      let rec loop inst =
        match det_pack_one ?domain decl_list inst with
        | None -> Some (Instance.swap inst)
        | Some next -> loop next
      in loop x

  let rec det_rew_display_tmp ?domain pointed strat instance =
    match strat with
    | New_ast.Onf s -> det_rew_display_tmp ?domain pointed (New_ast.Pick (New_ast.Iter s)) instance
    | New_ast.Ref subname ->
      let path = Str.split (Str.regexp "\\.") subname in
      begin
        match search_from pointed path with
        | None -> Error.build "Simple rewrite, cannot find strat %s" subname
        | Some (Rule r,_) ->
          begin
            match Rule.det_apply ?domain r (Instance.refresh instance) with
            | None -> None
            | Some inst -> Some [(Rule.get_name r, inst)]
          end
        | Some (Package (pack_name, decl_list), _) ->
            begin
              match det_pack_one ?domain decl_list (Instance.refresh instance) with
              | None -> None
              | Some inst -> Some [( pack_name, inst )]
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
        | Some (((_,inst) :: _) as l) ->
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
              match det_iter_pack ?domain [Rule r] (Instance.refresh instance) with
              | Some final -> Some [(Rule.get_name r, final)]
              | None -> Some []
            end
          | Some (Package (pack_name, decl_list), _) ->
            begin
              match det_iter_pack ?domain decl_list (Instance.refresh instance) with
              | Some final -> Some [(pack_name, final)]
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
        | Some (((_,inst) :: _) as l) ->
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

    let rec loop inst = function
    | [] -> Libgrew_types.Leaf inst.Instance.graph
    | (s2, i2) :: tail ->
      begin
        match i2.Instance.big_step with
        | Some bs2 -> Libgrew_types.Node (inst.Instance.graph, s2, [bs2, loop i2 tail])
        | _ -> failwith "missing BS"
      end in

    match CCOpt.map List.rev (det_rew_display_tmp ?domain (top grs) (Parser.strategy strat) instance) with
    | None -> Libgrew_types.Empty
    | Some [] -> Libgrew_types.Leaf instance.Instance.graph
    | Some ((s1,i1) :: tail) ->
      match i1.Instance.big_step with
      | Some bs -> Libgrew_types.Node (instance.Instance.graph, s1, [bs, loop i1 tail])
      (* Libgrew_types.Node (i2.Instance.graph,s2,[bs,rd])) tail *)
    | _ -> failwith "missing BS2"



(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)
(* ============================================================================================= *)


  (* return true if strat always return at least one graph *)
  let at_least_one grs strat =
    let rec loop pointed strat =
      match strat with
      | New_ast.Ref strat_name ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Rule _,_)
          | Some (Package _, _) -> false
          | Some (Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | New_ast.Pick s -> loop pointed s
      | New_ast.Onf s -> true
      | New_ast.Alt l -> List.exists (fun s -> loop pointed s) l
      | New_ast.Seq l -> List.for_all (fun s -> loop pointed s) l
      | New_ast.Iter _ -> true
      | New_ast.If (_,s1, s2) -> (loop pointed s1) && (loop pointed s2)
      | New_ast.Try (s) -> loop pointed s in
    loop (top grs) (Parser.strategy strat)

  (* return true if strat always return at most one graph *)
  let at_most_one grs strat =
    let rec loop pointed strat =
      match strat with
      | New_ast.Ref strat_name ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Rule _,_)
          | Some (Package _, _) -> false
          | Some (Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | New_ast.Pick s -> true
      | New_ast.Onf s -> true
      | New_ast.Alt [one] -> loop pointed one
      | New_ast.Alt _ -> false
      | New_ast.Seq l -> List.for_all (fun s -> loop pointed s) l
      | New_ast.Iter s -> loop pointed s
      | New_ast.If (_,s1, s2) -> (loop pointed s1) || (loop pointed s2)
      | New_ast.Try (s) -> loop pointed s in
    loop (top grs) (Parser.strategy strat)



  (* ============================================================================================= *)
  (* Rewriting in the deterministic case with graph type *)
  (* ============================================================================================= *)

  (* apply a package to an instance = apply only top level rules in the package *)
  let onf_pack_rewrite ?domain decl_list graph =
    let rec loop = function
      | [] -> None
      | Rule r :: tail_decl ->
        (match Rule.onf_apply ?domain r graph with
        | Some x -> Some x
        | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
      loop decl_list

  let rec onf_intern_simple_rewrite ?domain pointed strat_name graph =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) -> Rule.onf_apply ?domain r graph
    | Some (Package (_, decl_list), _) -> onf_pack_rewrite ?domain decl_list graph
    | Some (Strategy (_,ast_strat), new_pointed) ->
      onf_strat_simple_rewrite ?domain new_pointed ast_strat graph

  and onf_strat_simple_rewrite ?domain pointed strat graph =
    match strat with
    | New_ast.Ref subname -> onf_intern_simple_rewrite ?domain pointed subname graph
    | New_ast.Pick strat -> onf_strat_simple_rewrite ?domain pointed strat graph

    | New_ast.Alt [] -> None
    | New_ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match onf_strat_simple_rewrite ?domain pointed head_strat graph with
          | None -> loop tail_strat
          | Some x -> Some x in
        loop strat_list

    | New_ast.Seq [] -> Some graph
    | New_ast.Seq (head_strat :: tail_strat) ->
      begin
        match onf_strat_simple_rewrite ?domain pointed head_strat graph with
        | None -> None
        | Some inst -> onf_strat_simple_rewrite ?domain pointed (New_ast.Seq tail_strat) inst
      end

    | New_ast.Iter sub_strat ->
      begin
        match onf_strat_simple_rewrite ?domain pointed sub_strat graph with
        | None -> Some graph
        | Some inst -> onf_strat_simple_rewrite ?domain pointed strat inst
        end

    | New_ast.Try sub_strat ->
        begin
          match onf_strat_simple_rewrite ?domain pointed sub_strat graph with
          | None -> Some graph
          | Some i -> Some i
        end

    | New_ast.If (s, s1, s2) ->
      begin
        match onf_strat_simple_rewrite ?domain pointed s graph with
        | None   -> onf_strat_simple_rewrite ?domain pointed s1 graph
        | Some _ -> onf_strat_simple_rewrite ?domain pointed s2 graph
      end

    | New_ast.Onf (s) -> onf_strat_simple_rewrite ?domain pointed s graph (* TODO check Onf (P) == 1 rule app ? *)

  (* ============================================================================================= *)
  (* Rewriting in the non-deterministic case with Graph_with_history.t type *)
  (* ============================================================================================= *)

  (* apply a package to an instance = apply only top level rules in the package *)
  let gwh_pack_rewrite ?domain decl_list gwh =
    List.fold_left
      (fun acc decl -> match decl with
        | Rule r -> Graph_with_history_set.union acc (Rule.gwh_apply ?domain r gwh)
        | _ -> acc
      ) Graph_with_history_set.empty decl_list

  let rec gwh_intern_simple_rewrite ?domain pointed strat_name gwh =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) -> Rule.gwh_apply r gwh
    | Some (Package (_, decl_list), _) -> gwh_pack_rewrite decl_list gwh
    | Some (Strategy (_,ast_strat), new_pointed) ->
      gwh_strat_simple_rewrite ?domain new_pointed ast_strat gwh

  and gwh_strat_simple_rewrite ?domain pointed strat gwh =
    match strat with
    | New_ast.Ref subname -> gwh_intern_simple_rewrite ?domain pointed subname gwh
    | New_ast.Pick strat ->
      begin
        match Graph_with_history_set.choose_opt
          (gwh_strat_simple_rewrite ?domain pointed strat gwh) with
        | None -> Graph_with_history_set.empty
        | Some x -> Graph_with_history_set.singleton x
      end

    | New_ast.Alt [] -> Graph_with_history_set.empty
    | New_ast.Alt strat_list -> List.fold_left
      (fun acc strat -> Graph_with_history_set.union acc (gwh_strat_simple_rewrite ?domain pointed strat gwh)
      ) Graph_with_history_set.empty strat_list

    | New_ast.Seq [] -> Graph_with_history_set.singleton gwh
    | New_ast.Seq (head_strat :: tail_strat) ->
      let first_strat = gwh_strat_simple_rewrite ?domain pointed head_strat gwh in
      Graph_with_history_set.fold
        (fun gwh acc -> Graph_with_history_set.union acc (gwh_strat_simple_rewrite ?domain pointed (New_ast.Seq tail_strat) gwh)
        ) first_strat Graph_with_history_set.empty

    | New_ast.Iter strat -> iter_gwh ?domain pointed strat gwh

    | New_ast.Try strat ->
      begin
        let one_step = gwh_strat_simple_rewrite ?domain pointed strat gwh in
        if Graph_with_history_set.is_empty one_step
        then Graph_with_history_set.singleton gwh
        else one_step
      end

    | New_ast.If (s, s1, s2) ->
      begin
        match (* TODO: is it correct to put onf_ ?*)
        onf_strat_simple_rewrite ?domain pointed s gwh.Graph_with_history.graph with
        | Some _ -> gwh_strat_simple_rewrite ?domain pointed s1 gwh
        | None   -> gwh_strat_simple_rewrite ?domain pointed s2 gwh
      end

    | New_ast.Onf s ->
      begin
        match onf_strat_simple_rewrite ?domain pointed (New_ast.Iter s) gwh.Graph_with_history.graph with
        | None -> Graph_with_history_set.singleton gwh
        | Some new_g -> Graph_with_history_set.singleton (Graph_with_history.from_graph new_g)
      end

  and iter_gwh ?domain pointed strat gwh =
    let rec loop  (todo, not_nf, nf) =
      match Graph_with_history_set.choose_opt todo with
      | None -> nf
      | Some one ->
        let new_todo = Graph_with_history_set.remove one todo in
        let one_step = gwh_strat_simple_rewrite ?domain pointed strat one in
        if Graph_with_history_set.subset one_step (Graph_with_history_set.singleton one)
        then
        loop (
          new_todo,
          not_nf,
          Graph_with_history_set.add one nf
        )
        else
          let new_graphs =
            (Graph_with_history_set.diff
              (Graph_with_history_set.diff
                (Graph_with_history_set.diff one_step todo)
              not_nf)
            nf) in
        loop (
          Graph_with_history_set.union new_todo new_graphs,
          Graph_with_history_set.add one not_nf,
          nf
        ) in
      loop (Graph_with_history_set.singleton gwh, Graph_with_history_set.empty, Graph_with_history_set.empty)


  let gwh_simple_rewrite grs strat_string graph =
    let domain = domain grs in
    let casted_graph = G_graph.cast ?domain graph in
    let strat = Parser.strategy strat_string in
    let gwh = Graph_with_history.from_graph casted_graph in
    let set = gwh_strat_simple_rewrite ?domain (top grs) strat gwh in
    List.map
      (fun gwh -> gwh.Graph_with_history.graph)
      (Graph_with_history_set.elements set)


  (* ============================================================================================= *)
  (* production of rew_display of linear rewriting history for GUI *)
  (* ============================================================================================= *)
  type linear_rd = {
    graph: G_graph.t;
    steps: (string * G_graph.t * Libgrew_types.big_step) list;
  }

  let wrd_pack_rewrite ?domain decl_list graph_with_big_step =
    let rec loop = function
      | [] -> None
      | Rule r :: tail_decl ->
        (match Rule.wrd_apply ?domain r graph_with_big_step with
        | Some x -> Some x
        | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
      loop decl_list

  let rec wrd_pack_iter_rewrite ?domain decl_list graph_with_big_step =
    match (graph_with_big_step, wrd_pack_rewrite ?domain decl_list graph_with_big_step) with
      | (_, Some (new_gr, new_bs)) -> wrd_pack_iter_rewrite ?domain decl_list (new_gr, Some new_bs)
      | ((gr, Some bs), None) -> Some (gr, bs)
      | ((gr, None), None) -> None

  (* functions [wrd_intern_simple_rewrite] and [wrd_strat_simple_rewrite] computes
     one normal form and output the data needed for rew_display production.
     output = list of ... tranformed later into rew_display by [build_rew_display_from_linear_rd]
     [iter_flag] is set to true when rules application should be put together (in the old modules style).
  *)
  let rec wrd_intern_simple_rewrite ?domain iter_flag pointed strat_name linear_rd =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) ->
      begin
        match Rule.wrd_apply ?domain r (linear_rd.graph, None) with
          | None -> None
          | Some (new_graph, big_step) -> Some {steps = (Rule.get_name r, linear_rd.graph, big_step) :: linear_rd.steps; graph = new_graph}
      end
    | Some (Package (name, decl_list), _) when iter_flag ->
      begin
        match wrd_pack_iter_rewrite ?domain decl_list (linear_rd.graph, None) with
          | None -> None
          | Some (new_graph, big_step) -> Some {steps = (name, linear_rd.graph, big_step) :: linear_rd.steps; graph = new_graph}
      end
    | Some (Package (name, decl_list), _) ->
      begin
        match wrd_pack_rewrite ?domain decl_list (linear_rd.graph, None) with
          | None -> None
          | Some (new_graph, big_step) -> Some {steps = (name, linear_rd.graph, big_step) :: linear_rd.steps; graph = new_graph}
      end
    | Some (Strategy (_,ast_strat), new_pointed) ->
      wrd_strat_simple_rewrite ?domain iter_flag new_pointed ast_strat linear_rd

  and wrd_strat_simple_rewrite ?domain iter_flag pointed strat linear_rd  =
    match strat with
    | New_ast.Ref subname -> wrd_intern_simple_rewrite iter_flag ?domain pointed subname linear_rd
    | New_ast.Pick strat -> wrd_strat_simple_rewrite iter_flag ?domain pointed strat linear_rd

    | New_ast.Alt [] -> None
    | New_ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match wrd_strat_simple_rewrite ?domain false pointed head_strat linear_rd  with
          | None -> loop tail_strat
          | Some x -> Some x in
        loop strat_list

    | New_ast.Seq [] -> Some linear_rd
    | New_ast.Seq (head_strat :: tail_strat) ->
      begin
        match wrd_strat_simple_rewrite ?domain false pointed head_strat linear_rd  with
        | None -> None
        | Some gwrd -> wrd_strat_simple_rewrite iter_flag ?domain pointed (New_ast.Seq tail_strat) gwrd
      end

    | New_ast.Iter sub_strat
    | New_ast.Onf sub_strat ->
      begin
        match wrd_strat_simple_rewrite ?domain true pointed sub_strat linear_rd  with
        | None -> Some linear_rd
        | Some gwrd -> wrd_strat_simple_rewrite ?domain iter_flag pointed strat gwrd
      end

    | New_ast.Try sub_strat ->
        begin
          match wrd_strat_simple_rewrite ?domain false pointed sub_strat linear_rd  with
          | None -> Some linear_rd
          | Some i -> Some i
        end

    | New_ast.If (s, s1, s2) ->
      begin
        match onf_strat_simple_rewrite ?domain pointed s linear_rd.graph with
        | Some _ -> wrd_strat_simple_rewrite iter_flag ?domain pointed s1 linear_rd
        | None   -> wrd_strat_simple_rewrite iter_flag ?domain pointed s2 linear_rd
      end

  let build_rew_display_from_linear_rd linear_rd =
    List.fold_left
      (fun acc (n,g,bs) -> Libgrew_types.Node (g, n, [Libgrew_types.swap bs, acc])) (Libgrew_types.Leaf linear_rd.graph) linear_rd.steps

  let wrd_rewrite grs strat graph =
    let domain = domain grs in
    let casted_graph = G_graph.cast ?domain graph in
    match wrd_strat_simple_rewrite ?domain false (top grs) (Parser.strategy strat) {graph=casted_graph; steps=[]} with
    | None -> Libgrew_types.Leaf graph
    | Some linear_rd -> build_rew_display_from_linear_rd linear_rd

end (* module Grs *)








