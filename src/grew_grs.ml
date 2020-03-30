(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
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
module Grs = struct

  type decl =
    | Rule of Rule.t
    | Strategy of string * Ast.strat
    | Package of string * decl list

  type t = {
    filename: string;
    domain: Domain.t option;
    decls: decl list;
    ast: Ast.grs;
  }

  let empty = {
    filename = "";
    domain = None;
    decls = [Strategy ("main", Ast.Seq [])];
    ast = [];
  }

  let rec decl_to_json ?domain = function
    | Rule r -> Rule.to_json ?domain r
    | Strategy (name, strat) -> `Assoc [("strat_name", `String name); ("strat_def", Ast.strat_to_json strat)]
    | Package (name, decl_list) -> `Assoc [("package_name", `String name); "decls", `List (List.map (decl_to_json ?domain) decl_list)]

  let decl_to_string ?domain = function
    | Rule r -> sprintf "RULE: %s" (Rule.get_name r)
    | Strategy (name, strat) -> sprintf "STRAT: %s" (name)
    | Package (name, decl_list) -> sprintf "PACK: %s" (name)

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


  let get_strat_list grs = Grew_ast.Ast.strat_list grs.ast

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
    | Ast.Package (loc, name, decl_list) -> Package (name, List.map (build_decl ?domain) decl_list)
    | Ast.Rule ast_rule -> Rule (Rule.build ?domain ast_rule)
    | Ast.Strategy (loc, name, ast_strat) -> Strategy (name, ast_strat)
    | _ -> Error.bug "[build_decl] Inconsistent ast for grs"

  let domain t = t.domain

  let domain_build ast_domain =
    Domain.build
      (Label_domain.build ast_domain.Ast.label_domain)
      (Feature_domain.build ast_domain.Ast.feature_domain)

  let from_ast filename ast =
    let feature_domains = List_.opt_map
        (fun x -> match x with
           | Ast.Features desc -> Some desc
           | _ -> None
        ) ast in

    let feature_domain = match feature_domains with
      | [] -> None
      | h::t -> Some (Feature_domain.build (List.fold_left Feature_domain.merge h t)) in

    let label_domains = List_.opt_map
        (fun x -> match x with
           | Ast.Labels desc -> Some desc
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
           | Ast.Features _ -> None
           | Ast.Labels _ -> None
           | Ast.Conll_fields _ -> None
           | Ast.Import _ -> Error.bug "[load] Import: inconsistent ast for grs"
           | Ast.Include _ -> Error.bug "[load] Include: inconsistent ast for grs"
           | x -> Some (build_decl ?domain x)
        ) ast in

    { filename;
      ast;
      domain;
      decls;
    }

  let load filename = from_ast filename (Loader.grs filename)

  (* The type [pointed] is a zipper style data structure for resolving names x.y.z *)
  type pointed =
    | Top of decl list
    | Pack of (decl list * pointed)  (* (content, mother package) *)

  let rec dump_pointed = function
    | Top l -> printf "TOP: %s\n" (String.concat "+" (List.map decl_to_string l))
    | Pack (l, pointed)  ->
      printf "PACK: %s\nMOTHER --> " (String.concat "+" (List.map decl_to_string l));
      dump_pointed pointed



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
      begin
        match List.find_opt
                (function
                  | Strategy (s,_) when s=one -> true
                  | Rule r when Rule.get_name r = one -> true
                  | Package (p,_) when p=one -> true
                  | _ -> false
                ) (decl_list pointed) with
        | Some item -> Some (item, pointed)
        | None -> None
      end
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


  (* return true if strat always return at least one graph *)
  let at_least_one grs strat_string =
    let rec loop pointed strat =
      match strat with
      | Ast.Ref strat_name ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Rule _,_)
          | Some (Package _, _) -> false
          | Some (Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | Ast.Pick s -> loop pointed s
      | Ast.Onf s -> true
      | Ast.Alt l -> List.exists (fun s -> loop pointed s) l
      | Ast.Seq l -> List.for_all (fun s -> loop pointed s) l
      | Ast.Iter _ -> true
      | Ast.If (_,s1, s2) -> (loop pointed s1) && (loop pointed s2)
      | Ast.Try (s) -> loop pointed s in
    loop (top grs) (Parser.strategy strat_string)

  (* return true if strat always return at most one graph *)
  let at_most_one grs strat_string =
    let rec loop pointed strat =
      match strat with
      | Ast.Ref strat_name ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Rule _,_)
          | Some (Package _, _) -> false
          | Some (Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | Ast.Pick s -> true
      | Ast.Onf s -> true
      | Ast.Alt [one] -> loop pointed one
      | Ast.Alt _ -> false
      | Ast.Seq l -> List.for_all (fun s -> loop pointed s) l
      | Ast.Iter s -> loop pointed s
      | Ast.If (_,s1, s2) -> (loop pointed s1) || (loop pointed s2)
      | Ast.Try (s) -> loop pointed s in
    loop (top grs) (Parser.strategy strat_string)

  let is_without_history grs strat_string =
    let rec loop pointed strat =
      match strat with
      | Ast.Onf (Ast.Ref strat_name) ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Rule _,_)
          | Some (Package _, _) -> true
          | Some (Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | Ast.Ref strat_name ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Rule _,_)
          | Some (Package _, _) -> false
          | Some (Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | Ast.Pick s -> loop pointed s
      | Ast.Onf s -> loop pointed s
      | Ast.Alt [one] -> loop pointed one
      | Ast.Alt _ -> false
      | Ast.Seq l -> List.for_all (fun s -> loop pointed s) l
      | Ast.Iter s -> false
      | Ast.If (_,s1, s2) -> (loop pointed s1) || (loop pointed s2)
      | Ast.Try (s) -> loop pointed s in
    loop (top grs) (Parser.strategy strat_string)



  (* ============================================================================================= *)
  (* Rewriting in the deterministic case with graph type *)
  (* ============================================================================================= *)

  (* apply a package to a graph = apply only top level rules in the package *)
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

  (* one step or until normal_form *)
  (* 3 values: Zero | Id | New_graph g *)

  let rec onf_intern_simple_rewrite ?domain pointed strat_name graph =
    (* printf "===== onf_intern_simple_rewrite ==== %s\n%!" strat_name; *)
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) -> Rule.onf_apply ?domain r graph
    | Some (Package (_, decl_list), _) -> onf_pack_rewrite ?domain decl_list graph
    | Some (Strategy (_,ast_strat), new_pointed) ->
      onf_strat_simple_rewrite ?domain new_pointed ast_strat graph

  and onf_strat_simple_rewrite ?domain pointed strat graph =
    match strat with
    | Ast.Ref subname -> onf_intern_simple_rewrite ?domain pointed subname graph
    | Ast.Pick strat -> onf_strat_simple_rewrite ?domain pointed strat graph

    | Ast.Alt [] -> None
    | Ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match onf_strat_simple_rewrite ?domain pointed head_strat graph with
          | None -> loop tail_strat
          | Some x -> Some x in
      loop strat_list

    | Ast.Seq [] -> Some graph
    | Ast.Seq (head_strat :: tail_strat) ->
      begin
        match onf_strat_simple_rewrite ?domain pointed head_strat graph with
        | None -> None
        | Some inst -> onf_strat_simple_rewrite ?domain pointed (Ast.Seq tail_strat) inst
      end

    | Ast.Iter sub_strat ->
      begin
        match onf_strat_simple_rewrite ?domain pointed sub_strat graph with
        | None -> Some graph
        | Some inst -> onf_strat_simple_rewrite ?domain pointed strat inst
      end

    | Ast.Try sub_strat ->
      begin
        match onf_strat_simple_rewrite ?domain pointed sub_strat graph with
        | None -> Some graph
        | Some i -> Some i
      end

    | Ast.If (s, s1, s2) ->
      begin
        match onf_strat_simple_rewrite ?domain pointed s graph with
        | None   -> onf_strat_simple_rewrite ?domain pointed s1 graph
        | Some _ -> onf_strat_simple_rewrite ?domain pointed s2 graph
      end

    | Ast.Onf (s) -> onf_strat_simple_rewrite ?domain pointed s graph (* TODO check Onf (P) == 1 rule app ? *)

  (* TODO: unused function, should be used for some cases like Seq (Onf(p1), Onf(p2)) *)
  (* iter until normal form *)
  let onf_rewrite ?domain pointed strat graph =
    let rec loop graph2 =
      match onf_strat_simple_rewrite ?domain pointed strat graph2 with
      | None -> graph2
      | Some x -> loop x in
    loop graph

  (* ============================================================================================= *)
  (* Rewriting in the deterministic case with Graph_with_history.t type *)
  (* ============================================================================================= *)

  (* NB: the next 3 functions compute one step (with option output) for correct recusice call in case of Alt *)
  (* the function [owh_rewrite] handle the iteration until normal_form *)
  let owh_pack_rewrite ?domain decl_list gwh =
    let rec loop = function
      | [] -> None
      | Rule r :: tail_decl ->
        (match Rule.owh_apply ?domain r gwh with
         | Some x -> Some x
         | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
    loop decl_list

  let rec owh_intern_simple_rewrite ?domain pointed strat_name gwh =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) -> Rule.owh_apply ?domain r gwh
    | Some (Package (_, decl_list), _) -> owh_pack_rewrite ?domain decl_list gwh
    | Some (Strategy (_,ast_strat), new_pointed) ->
      owh_strat_simple_rewrite ?domain new_pointed ast_strat gwh

  and owh_strat_simple_rewrite ?domain pointed strat gwh =
    match strat with
    | Ast.Ref subname -> owh_intern_simple_rewrite ?domain pointed subname gwh
    | Ast.Pick strat -> owh_strat_simple_rewrite ?domain pointed strat gwh

    | Ast.Alt [] -> None
    | Ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match owh_strat_simple_rewrite ?domain pointed head_strat gwh with
          | None -> loop tail_strat
          | Some x -> Some x in
      loop strat_list

    | Ast.Seq [] -> Some gwh
    | Ast.Seq (head_strat :: tail_strat) ->
      begin
        match owh_strat_simple_rewrite ?domain pointed head_strat gwh with
        | None -> None
        | Some gwh2 -> owh_strat_simple_rewrite ?domain pointed (Ast.Seq tail_strat) gwh2
      end

    | Ast.Try sub_strat
    | Ast.Onf sub_strat
    | Ast.Iter sub_strat -> owh_strat_simple_rewrite ?domain pointed sub_strat gwh

    | Ast.If (s, s1, s2) ->
      begin
        (* NB: checking one real step is enough to decide… *)
        match onf_strat_simple_rewrite ?domain pointed s gwh.Graph_with_history.graph with
        | None   -> owh_strat_simple_rewrite ?domain pointed s1 gwh
        | Some _ -> owh_strat_simple_rewrite ?domain pointed s2 gwh
      end


  (* iter until normal form *)
  let owh_rewrite ?domain pointed strat gwh =
    let rec loop gwh2 =
      match owh_strat_simple_rewrite ?domain pointed strat gwh2 with
      | None -> gwh2
      | Some x -> loop x in
    loop gwh

  (* ============================================================================================= *)
  (* Rewriting in the non-deterministic case with Graph_with_history.t type *)
  (* ============================================================================================= *)

  (* apply a package to an graph_with_history = apply only top level rules in the package *)
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
    | Ast.Ref subname -> gwh_intern_simple_rewrite ?domain pointed subname gwh
    | Ast.Pick strat ->
      begin
        match Graph_with_history_set.choose_opt
                (gwh_strat_simple_rewrite ?domain pointed strat gwh) with
        | None -> Graph_with_history_set.empty
        | Some x -> Graph_with_history_set.singleton x
      end

    | Ast.Alt [] -> Graph_with_history_set.empty
    | Ast.Alt strat_list -> List.fold_left
                              (fun acc strat -> Graph_with_history_set.union acc (gwh_strat_simple_rewrite ?domain pointed strat gwh)
                              ) Graph_with_history_set.empty strat_list

    | Ast.Seq [] -> Graph_with_history_set.singleton gwh
    | Ast.Seq (head_strat :: tail_strat) ->
      let first_strat = gwh_strat_simple_rewrite ?domain pointed head_strat gwh in
      Graph_with_history_set.fold
        (fun gwh acc -> Graph_with_history_set.union acc (gwh_strat_simple_rewrite ?domain pointed (Ast.Seq tail_strat) gwh)
        ) first_strat Graph_with_history_set.empty

    | Ast.Iter s -> iter_gwh ?domain pointed s gwh
    | Ast.Onf s ->  Graph_with_history_set.singleton (owh_rewrite ?domain pointed strat gwh)

    | Ast.Try strat ->
      begin
        let one_step = gwh_strat_simple_rewrite ?domain pointed strat gwh in
        if Graph_with_history_set.is_empty one_step
        then Graph_with_history_set.singleton gwh
        else one_step
      end

    | Ast.If (s, s1, s2) ->
      begin
        match (* TODO: is it correct to put onf_ ?*)
          onf_strat_simple_rewrite ?domain pointed s gwh.Graph_with_history.graph with
        | Some _ -> gwh_strat_simple_rewrite ?domain pointed s1 gwh
        | None   -> gwh_strat_simple_rewrite ?domain pointed s2 gwh
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

  (* ============================================================================================= *)
  let gwh_simple_rewrite grs strat graph =
    Rule.reset_rules ();
    Timeout.start ();
    let domain = domain grs in
    let casted_graph = G_graph.cast ?domain graph in
    let gwh = Graph_with_history.from_graph casted_graph in
    let set = gwh_strat_simple_rewrite ?domain (top grs) strat gwh in
    List.map
      (fun gwh -> gwh.Graph_with_history.graph)
      (Graph_with_history_set.elements set)

  (* ============================================================================================= *)
  let simple_rewrite grs strat_string graph =
    let strat = Parser.strategy strat_string in
    if (* is_without_history grs strat_string *) false
    then [onf_rewrite (top grs) strat graph]
    else gwh_simple_rewrite grs strat graph

  (* ============================================================================================= *)
  (* production of rew_display of linear rewriting history for GUI *)
  (* ============================================================================================= *)
  type linear_rd = {
    graph: G_graph.t;
    steps: (string * G_graph.t * Libgrew_types.big_step) list;
    know_normal_form: bool;
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
     output = list of ... transformed later into rew_display by [build_rew_display_from_linear_rd]
     [iter_flag] is set to true when rules application should be put together (in the old modules style).
  *)
  let rec wrd_intern_simple_rewrite ?domain iter_flag pointed strat_name linear_rd =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) when iter_flag ->
      begin (* pack iterations on one rule as one "package" *)
        match wrd_pack_iter_rewrite ?domain [Rule r] (linear_rd.graph, None) with
        | None -> None
        | Some (new_graph, big_step) -> Some {
            steps = (sprintf "Onf(%s)" (Rule.get_name r), linear_rd.graph, big_step) :: linear_rd.steps;
            graph = new_graph;
            know_normal_form = true;
          }
      end
    | Some (Rule r,_) ->
      begin
        match Rule.wrd_apply ?domain r (linear_rd.graph, None) with
        | None -> None
        | Some (new_graph, big_step) -> Some {
            steps = (Rule.get_name r, linear_rd.graph, big_step) :: linear_rd.steps;
            graph = new_graph;
            know_normal_form=false
          }
      end
    | Some (Package (name, decl_list), _) when iter_flag ->
      begin
        match wrd_pack_iter_rewrite ?domain decl_list (linear_rd.graph, None) with
        | None -> None
        | Some (new_graph, big_step) -> Some {
            steps = (name, linear_rd.graph, big_step) :: linear_rd.steps;
            graph = new_graph;
            know_normal_form = true;
          }
      end
    | Some (Package (name, decl_list), _) ->
      begin
        match wrd_pack_rewrite ?domain decl_list (linear_rd.graph, None) with
        | None -> None
        | Some (new_graph, big_step) -> Some {
            steps = (name, linear_rd.graph, big_step) :: linear_rd.steps;
            graph = new_graph;
            know_normal_form = true;
          }
      end
    | Some (Strategy (_,ast_strat), new_pointed) ->
      wrd_strat_simple_rewrite ?domain iter_flag new_pointed ast_strat linear_rd

  and wrd_strat_simple_rewrite ?domain iter_flag pointed strat linear_rd  =
    match strat with
    | Ast.Ref subname -> wrd_intern_simple_rewrite iter_flag ?domain pointed subname linear_rd
    | Ast.Pick strat -> wrd_strat_simple_rewrite iter_flag ?domain pointed strat linear_rd

    | Ast.Alt [] -> None
    | Ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match wrd_strat_simple_rewrite ?domain false pointed head_strat linear_rd  with
          | None -> loop tail_strat
          | Some x -> Some x in
      loop strat_list

    | Ast.Seq [] -> Some linear_rd
    | Ast.Seq (head_strat :: tail_strat) ->
      begin
        match wrd_strat_simple_rewrite ?domain false pointed head_strat linear_rd  with
        | None -> None
        | Some gwrd -> wrd_strat_simple_rewrite iter_flag ?domain pointed (Ast.Seq tail_strat) gwrd
      end

    | Ast.Iter sub_strat
    | Ast.Onf sub_strat ->
      begin
        match wrd_strat_simple_rewrite ?domain true pointed sub_strat linear_rd  with
        | None -> Some {linear_rd with know_normal_form = true}
        | Some gwrd when gwrd.know_normal_form -> Some gwrd
        | Some gwrd -> wrd_strat_simple_rewrite ?domain iter_flag pointed strat gwrd
      end

    | Ast.Try sub_strat ->
      begin
        match wrd_strat_simple_rewrite ?domain false pointed sub_strat linear_rd  with
        | None -> Some linear_rd
        | Some i -> Some i
      end

    | Ast.If (s, s1, s2) ->
      begin
        match onf_strat_simple_rewrite ?domain pointed s linear_rd.graph with
        | Some _ -> wrd_strat_simple_rewrite iter_flag ?domain pointed s1 linear_rd
        | None   -> wrd_strat_simple_rewrite iter_flag ?domain pointed s2 linear_rd
      end

  let build_rew_display_from_linear_rd linear_rd =
    List.fold_left
      (fun acc (n,g,bs) -> Libgrew_types.Node (g, n, [Libgrew_types.swap bs, acc])) (Libgrew_types.Leaf linear_rd.graph) linear_rd.steps

  let wrd_rewrite grs strat graph =
    Rule.reset_rules ();
    Timeout.start ();
    let domain = domain grs in
    let casted_graph = G_graph.cast ?domain graph in
    match wrd_strat_simple_rewrite ?domain false (top grs) (Parser.strategy strat) {graph=casted_graph; steps=[]; know_normal_form=false} with
    | None -> Libgrew_types.Leaf graph
    | Some linear_rd -> build_rew_display_from_linear_rd linear_rd


  let eud2ud = load (Filename.concat DATADIR "eud2ud.grs")
  let apply_eud2ud graph =
    match simple_rewrite eud2ud "main" graph with
    | [one] -> one
    | _ -> Error.run "the conversion eud2ud is not deterministic"

  let apply grs_name graph =
    match grs_name with
    | "eud2ud" -> apply_eud2ud graph
    | x -> Error.run "[Grs.apply] Unknown GRS \"%s\"" x


end (* module Grs *)







