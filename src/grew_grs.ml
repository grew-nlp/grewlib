(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Conll

open Grew_utils
open Grew_ast
open Grew_graph
open Grew_rule
open Grew_loader

(* ================================================================================ *)
module Decl = struct

  type t =
    | Rule of Rule.t
    | Strategy of string * Ast.strat
    | Package of string * t list

  let rec to_json ~config = function
    | Rule r -> (Rule.get_name r, Rule.to_json ~config r)
    | Strategy (name, strat) -> (name, `String (Ast.strat_to_string strat))
    | Package (name, decl_list) -> (name, `Assoc ["decls", `Assoc (List.map (fun x -> to_json ~config x) decl_list)])

  let to_string = function
    | Rule r -> sprintf "RULE: %s" (Rule.get_name r)
    | Strategy (name, _) -> sprintf "STRAT: %s" (name)
    | Package (name, _) -> sprintf "PACK: %s" (name)

  let rec build ~config = function
    | Ast.Package (_, name, decl_list) -> Package (name, List.map (build ~config) decl_list)
    | Ast.Rule ast_rule -> Rule (Rule.of_ast ~config ast_rule)
    | Ast.Strategy (_, name, ast_strat) -> Strategy (name, ast_strat)
    | _ -> Error.bug "[Decl.build] Inconsistent ast for grs"

  let rec string_of_json (key, json) =
    let open Yojson.Basic.Util in
    try let strat = json |> to_string in sprintf "strat %s { %s }" key strat
    with Type_error _ ->
      try let assoc = json |> to_assoc in
        match (List.assoc_opt "request" assoc, List.assoc_opt "commands" assoc, List.assoc_opt "decls" assoc) with
        | (Some r, Some c, None) -> sprintf "rule %s {\n%s\n  }" key (Rule.string_of_json r c)
        | (None, None, Some p) -> sprintf "package %s { %s }" key (p |> to_assoc |> List.map string_of_json |> String.concat "\n")
        | _ -> Error.build "[Decl.string_of_json]"
      with Type_error _ ->
        Error.build "[Decl.string_of_json]"

  let rec _dump indent = function
    | Rule r -> printf "%srule %s\n" (String.make indent ' ') (Rule.get_name r)
    | Strategy (name, _) -> printf "%sstrat %s\n" (String.make indent ' ') name
    | Package (name, decl_list) ->
      printf "%spackage %s:\n" (String.make indent ' ') name;
      List.iter (_dump (indent + 2)) decl_list

end (* module Decl *)

(* ================================================================================ *)
module Grs = struct

  type t = {
    filename: string;
    decls: Decl.t list;
    ast: Ast.grs;
    timestamp: float option;
  }

  let empty = {
    filename = "";
    decls = [Strategy ("main", Ast.Seq [])];
    ast = [];
    timestamp = None;
  }

  let to_json ~config t =
    `Assoc [
      "filename", `String t.filename;
      "decls", `Assoc (List.map (fun x -> Decl.to_json ~config x) t.decls)
    ]

  let get_strat_list grs = Ast.strat_list grs.ast
  let get_strat_lists grs = Ast.strat_lists grs.ast
  let get_package_list grs = Ast.package_list grs.ast
  let get_rule_list grs = Ast.rule_list grs.ast

  let get_timestamp_opt grs = grs.timestamp

  let _dump t =
    printf "================ Grs ================\n";
    List.iter (Decl._dump 0) t.decls;
    printf "================ Grs ================\n%!";
    ()


  let build ~config ?timestamp filename ast =
    let decls = CCList.filter_map
        (fun x -> match x with
           | Ast.Features _ -> None
           | Ast.Labels _ -> None
           | Ast.Conll_fields _ -> None
           | Ast.Import _ -> Error.bug "[load] Import: inconsistent ast for grs"
           | Ast.Include _ -> Error.bug "[load] Include: inconsistent ast for grs"
           | x -> Some (Decl.build ~config x)
        ) ast in

    { filename;
      ast;
      decls;
      timestamp;
    }

  let load ~config filename =
    let _ = Global.reset_grs_timestamp () in
    let grs_ast = Loader.grs filename in
    build ~config ~timestamp:(Global.get_grs_timestamp ()) filename grs_ast

  let parse ~config string_grs = build ~config "" (Parser.grs string_grs)

  let string_of_json json =
    let open Yojson.Basic.Util in
    try
      let decls = json |> member "decls" |> to_assoc in
      String.concat "\n" (List.map Decl.string_of_json decls)
    with Type_error _ -> "[Grs.string_of_json]"

  let of_json ~config json =
    let s = string_of_json json in
    let ast = Parser.grs s in
    build ~config "" ast









  (* The type [pointed] is a zipper style data structure for resolving names x.y.z *)
  type pointed =
    | Top of Decl.t list
    | Pack of (Decl.t list * pointed)  (* (content, mother package) *)

  let rec _dump_pointed = function
    | Top l -> printf "TOP: %s\n" (String.concat "+" (List.map Decl.to_string l))
    | Pack (l, pointed)  ->
      printf "PACK: %s\nMOTHER --> " (String.concat "+" (List.map Decl.to_string l));
      _dump_pointed pointed



  let top grs = Top grs.decls

  let decl_list = function
    | Top dl -> dl
    | Pack (dl, _) -> dl

  let down pointed name =
    let rec loop = function
      | [] -> None
      | Decl.Package (n,dl) :: _ when n=name -> Some (Pack (dl, pointed))
      | _::t -> loop t in
    loop (decl_list pointed)

  (* search for a decl named [name] defined in the working directory [wd] in [grs] *)
  let rec search_at pointed = function
    | [] -> None
    | [one] ->
      begin
        match List.find_opt
                (function
                  | Decl.Strategy (s,_) when s=one -> true
                  | Decl.Rule r when Rule.get_name r = one -> true
                  | Decl.Package (p,_) when p=one -> true
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

  let _is_without_history grs strat_string =
    let rec loop pointed strat =
      match strat with
      | Ast.Onf (Ast.Ref strat_name) ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Decl.Rule _,_)
          | Some (Decl.Package _, _) -> true
          | Some (Decl.Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | Ast.Ref strat_name ->
        begin
          let path = Str.split (Str.regexp "\\.") strat_name in
          match search_from pointed path with
          | None -> Error.build "cannot find strat %s" strat_name
          | Some (Decl.Rule _,_)
          | Some (Decl.Package _, _) -> false
          | Some (Decl.Strategy (_,ast_strat), new_pointed) -> loop new_pointed ast_strat
        end
      | Ast.Pick s -> loop pointed s
      | Ast.Onf s -> loop pointed s
      | Ast.Alt [one] -> loop pointed one
      | Ast.Alt _ -> false
      | Ast.Seq l -> List.for_all (fun s -> loop pointed s) l
      | Ast.Iter _ -> false
      | Ast.If (_,s1, s2) -> (loop pointed s1) || (loop pointed s2)
      | Ast.Try (s) -> loop pointed s in
    loop (top grs) (Parser.strategy strat_string)



  (* ============================================================================================= *)
  (* Rewriting in the deterministic case with graph type *)
  (* ============================================================================================= *)

  (* apply a package to a graph = apply only top level rules in the package *)
  let onf_pack_rewrite ~config decl_list graph =
    let rec loop = function
      | [] -> None
      | Decl.Rule r :: tail_decl ->
        (match Rule.onf_apply_opt ~config r graph with
         | Some x -> Some x
         | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
    loop decl_list

  (* one step or until normal_form *)
  (* 3 values: Zero | Id | New_graph g *)

  let rec onf_intern_simple_rewrite pointed strat_name graph =
    (* printf "===== onf_intern_simple_rewrite ==== %s\n%!" strat_name; *)
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Decl.Rule r,_) -> Rule.onf_apply_opt r graph
    | Some (Decl.Package (_, decl_list), _) -> onf_pack_rewrite decl_list graph
    | Some (Decl.Strategy (_,ast_strat), new_pointed) ->
      onf_strat_simple_rewrite new_pointed ast_strat graph

  and onf_strat_simple_rewrite ~config pointed strat graph =
    match strat with
    | Ast.Ref subname -> onf_intern_simple_rewrite ~config pointed subname graph
    | Ast.Pick strat -> onf_strat_simple_rewrite ~config pointed strat graph

    | Ast.Alt [] -> None
    | Ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match onf_strat_simple_rewrite ~config pointed head_strat graph with
          | None -> loop tail_strat
          | Some x -> Some x in
      loop strat_list

    | Ast.Seq [] -> Some graph
    | Ast.Seq (head_strat :: tail_strat) ->
      begin
        match onf_strat_simple_rewrite ~config pointed head_strat graph with
        | None -> None
        | Some inst -> onf_strat_simple_rewrite ~config pointed (Ast.Seq tail_strat) inst
      end

    | Ast.Iter sub_strat ->
      begin
        match onf_strat_simple_rewrite ~config pointed sub_strat graph with
        | None -> Some graph
        | Some inst -> onf_strat_simple_rewrite ~config pointed strat inst
      end

    | Ast.Try sub_strat ->
      begin
        match onf_strat_simple_rewrite ~config pointed sub_strat graph with
        | None -> Some graph
        | Some i -> Some i
      end

    | Ast.If (s, s1, s2) ->
      begin
        match onf_strat_simple_rewrite ~config pointed s graph with
        | None   -> onf_strat_simple_rewrite ~config pointed s1 graph
        | Some _ -> onf_strat_simple_rewrite ~config pointed s2 graph
      end

    | Ast.Onf (s) -> onf_strat_simple_rewrite ~config pointed s graph (* TODO check Onf (P) == 1 rule app ? *)

  (* TODO: unused function, should be used for some cases like Seq (Onf(p1), Onf(p2)) *)
  (* iter until normal form *)
  let onf_rewrite ~config pointed strat graph =
    let rec loop graph2 =
      match onf_strat_simple_rewrite ~config pointed strat graph2 with
      | None -> graph2
      | Some x -> loop x in
    loop graph

  (* ============================================================================================= *)
  let onf_rewrite_opt ~config grs strat_string graph =
    Rule.reset_rules ();
    Global.track_rules := true; (* This is needed for [G_graph.is_initial] to work properly below *)
    let strat = Parser.strategy strat_string in
    let new_graph = onf_rewrite ~config (top grs) strat (G_graph.clear_rules graph) in
    if G_graph.is_initial new_graph
    then None
    else Some new_graph

  (* ============================================================================================= *)
  (* Rewriting in the deterministic case with Graph_with_history.t type *)
  (* ============================================================================================= *)

  (* NB: the next 3 functions compute one step (with option output) for correct recusice call in case of Alt *)
  (* the function [owh_rewrite] handle the iteration until normal_form *)
  let owh_pack_rewrite ~config decl_list gwh =
    let rec loop = function
      | [] -> None
      | Decl.Rule r :: tail_decl ->
        (match Rule.owh_apply_opt ~config r gwh with
         | Some x -> Some x
         | None -> loop tail_decl
        )
      | _ :: tail_decl -> loop tail_decl in
    loop decl_list

  let rec owh_intern_simple_rewrite ~config pointed strat_name gwh =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Decl.Rule r,_) -> Rule.owh_apply_opt ~config r gwh
    | Some (Decl.Package (_, decl_list), _) -> owh_pack_rewrite ~config decl_list gwh
    | Some (Decl.Strategy (_,ast_strat), new_pointed) ->
      owh_strat_simple_rewrite ~config new_pointed ast_strat gwh

  and owh_strat_simple_rewrite ~config pointed strat gwh =
    match strat with
    | Ast.Ref subname -> owh_intern_simple_rewrite ~config pointed subname gwh
    | Ast.Pick strat -> owh_strat_simple_rewrite ~config pointed strat gwh

    | Ast.Alt [] -> None
    | Ast.Alt strat_list ->
      let rec loop = function
        | [] -> None
        | head_strat :: tail_strat ->
          match owh_strat_simple_rewrite ~config pointed head_strat gwh with
          | None -> loop tail_strat
          | Some x -> Some x in
      loop strat_list

    | Ast.Seq [] -> Some gwh
    | Ast.Seq (head_strat :: tail_strat) ->
      begin
        match owh_strat_simple_rewrite ~config pointed head_strat gwh with
        | None -> None
        | Some gwh2 -> owh_strat_simple_rewrite ~config pointed (Ast.Seq tail_strat) gwh2
      end

    | Ast.Try sub_strat
    | Ast.Onf sub_strat
    | Ast.Iter sub_strat -> owh_strat_simple_rewrite ~config pointed sub_strat gwh

    | Ast.If (s, s1, s2) ->
      begin
        (* NB: checking one real step is enough to decide… *)
        match onf_strat_simple_rewrite ~config pointed s gwh.Graph_with_history.graph with
        | None   -> owh_strat_simple_rewrite ~config pointed s1 gwh
        | Some _ -> owh_strat_simple_rewrite ~config pointed s2 gwh
      end


  (* iter until normal form *)
  let owh_rewrite ~config pointed strat gwh =
    let rec loop gwh2 =
      match owh_strat_simple_rewrite ~config pointed strat gwh2 with
      | None -> gwh2
      | Some x -> loop x in
    loop gwh

  (* ============================================================================================= *)
  (* Rewriting in the non-deterministic case with Graph_with_history.t type *)
  (* ============================================================================================= *)

  (* apply a package to an graph_with_history = apply only top level rules in the package *)
  let gwh_pack_rewrite ~config decl_list gwh =
    List.fold_left
      (fun acc decl -> match decl with
         | Decl.Rule r -> Graph_with_history_set.union acc (Rule.gwh_apply ~config r gwh)
         | _ -> acc
      ) Graph_with_history_set.empty decl_list

  let rec gwh_intern_simple_rewrite ~config pointed strat_name gwh =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Decl.Rule r,_) -> Rule.gwh_apply ~config r gwh
    | Some (Decl.Package (_, decl_list), _) -> gwh_pack_rewrite ~config decl_list gwh
    | Some (Decl.Strategy (_,ast_strat), new_pointed) ->
      gwh_strat_simple_rewrite ~config new_pointed ast_strat gwh

  and gwh_strat_simple_rewrite ~config pointed strat gwh =
    match strat with
    | Ast.Ref subname -> gwh_intern_simple_rewrite ~config pointed subname gwh
    | Ast.Pick strat ->
      begin
        match Graph_with_history_set.choose_opt
                (gwh_strat_simple_rewrite ~config pointed strat gwh) with
        | None -> Graph_with_history_set.empty
        | Some x -> Graph_with_history_set.singleton x
      end

    | Ast.Alt [] -> Graph_with_history_set.empty
    | Ast.Alt strat_list -> List.fold_left
                              (fun acc strat -> Graph_with_history_set.union acc (gwh_strat_simple_rewrite ~config pointed strat gwh)
                              ) Graph_with_history_set.empty strat_list

    | Ast.Seq [] -> Graph_with_history_set.singleton gwh
    | Ast.Seq (head_strat :: tail_strat) ->
      let first_strat = gwh_strat_simple_rewrite ~config pointed head_strat gwh in
      Graph_with_history_set.fold
        (fun gwh acc -> Graph_with_history_set.union acc (gwh_strat_simple_rewrite ~config pointed (Ast.Seq tail_strat) gwh)
        ) first_strat Graph_with_history_set.empty

    | Ast.Iter s -> iter_gwh ~config pointed s gwh
    | Ast.Onf s ->  Graph_with_history_set.singleton (owh_rewrite ~config pointed s gwh)

    | Ast.Try strat ->
      begin
        let one_step = gwh_strat_simple_rewrite ~config pointed strat gwh in
        if Graph_with_history_set.is_empty one_step
        then Graph_with_history_set.singleton gwh
        else one_step
      end

    | Ast.If (s, s1, s2) ->
      begin
        match (* TODO: is it correct to put onf_ ?*)
          onf_strat_simple_rewrite ~config pointed s gwh.Graph_with_history.graph with
        | Some _ -> gwh_strat_simple_rewrite ~config pointed s1 gwh
        | None   -> gwh_strat_simple_rewrite ~config pointed s2 gwh
      end

  and iter_gwh ~config pointed strat gwh =
    let rec loop  (todo, not_nf, nf) =
      match Graph_with_history_set.choose_opt todo with
      | None -> nf
      | Some one ->
        let new_todo = Graph_with_history_set.remove one todo in
        let one_step = gwh_strat_simple_rewrite ~config pointed strat one in
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
  let gwh_simple_rewrite ~config grs strat graph =
    Rule.reset_rules ();
    Timeout.start ();
    let gwh = Graph_with_history.from_graph graph in
    let set = gwh_strat_simple_rewrite ~config (top grs) strat gwh in
    List.map
      (fun gwh -> gwh.Graph_with_history.graph)
      (Graph_with_history_set.elements set)
    |> (fun x -> Timeout.stop (); x)

  (* ============================================================================================= *)
  let simple_rewrite ~config grs strat_string graph =
    let strat = Parser.strategy strat_string in
    if (* is_without_history grs strat_string *) false (* TODO: review onf VS gwh *)
    then [onf_rewrite ~config (top grs) strat graph]
    else gwh_simple_rewrite ~config grs strat graph

  (* With the dune build system, the installation of external files breaks eliom-based code compilation.
     The grs is temporaly hard-coded here. *)
  let eud2ud_string = "
  package eud_to_ud {
    rule deep { % remove add enhanced relations
      pattern { e:N -[enhanced=yes]-> M }
      commands { del_edge e}
    }

    rule empty { % remove empty nodes
      pattern { N [wordform=__EMPTY__, textform=_] }
      commands { del_node N }
    }
  }

  strat main { Onf(eud_to_ud) }
"

  let eud2ud = parse ~config:(Conll_config.build "ud") eud2ud_string

  let apply_eud2ud ~config graph =
    match simple_rewrite ~config eud2ud "main" graph with
    | [one] -> one
    | _ -> Error.run "the conversion eud2ud is not deterministic"

  let apply ~config grs_name graph =
    match grs_name with
    | "eud2ud" -> apply_eud2ud ~config graph
    | x -> Error.run "[Grs.apply] Unknown GRS \"%s\"" x


end (* module Grs *)







