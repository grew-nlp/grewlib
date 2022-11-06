(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Conllx

open Grew_fs
open Grew_types
open Grew_utils
open Grew_ast
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
    decls: decl list;
    ast: Ast.grs;
  }

  let empty = {
    filename = "";
    decls = [Strategy ("main", Ast.Seq [])];
    ast = [];
  }

  let rec decl_to_json ~config = function
    | Rule r -> (Rule.get_name r, Rule.to_json ~config r)
    | Strategy (name, strat) -> (name, `String (Ast.strat_to_string strat))
    | Package (name, decl_list) -> (name, `Assoc ["decls", `Assoc (List.map (fun x -> decl_to_json ~config x) decl_list)])

  let decl_to_string = function
    | Rule r -> sprintf "RULE: %s" (Rule.get_name r)
    | Strategy (name, strat) -> sprintf "STRAT: %s" (name)
    | Package (name, decl_list) -> sprintf "PACK: %s" (name)

  let to_json ~config t =
    `Assoc [
      "filename", `String t.filename;
      "decls", `Assoc (List.map (fun x -> decl_to_json ~config x) t.decls)
    ]

  let get_strat_list grs = Ast.strat_list grs.ast
  let get_strat_lists grs = Ast.strat_lists grs.ast
  let get_package_list grs = Ast.package_list grs.ast
  let get_rule_list grs = Ast.rule_list grs.ast

  let rec dump_decl indent = function
    | Rule r -> printf "%srule %s\n" (String.make indent ' ') (Rule.get_name r)
    | Strategy (name, def) -> printf "%sstrat %s\n" (String.make indent ' ') name
    | Package (name, decl_list) ->
      printf "%spackage %s:\n" (String.make indent ' ') name;
      List.iter (dump_decl (indent + 2)) decl_list

  let dump t =
    printf "================ Grs ================\n";
    List.iter (dump_decl 0) t.decls;
    printf "================ Grs ================\n%!";
    ()

  let rec build_decl ~config = function
    | Ast.Package (loc, name, decl_list) -> Package (name, List.map (build_decl ~config) decl_list)
    | Ast.Rule ast_rule -> Rule (Rule.of_ast ~config ast_rule)
    | Ast.Strategy (loc, name, ast_strat) -> Strategy (name, ast_strat)
    | _ -> Error.bug "[build_decl] Inconsistent ast for grs"

  let from_ast ~config filename ast =
    let decls = CCList.filter_map
        (fun x -> match x with
           | Ast.Features _ -> None
           | Ast.Labels _ -> None
           | Ast.Conll_fields _ -> None
           | Ast.Import _ -> Error.bug "[load] Import: inconsistent ast for grs"
           | Ast.Include _ -> Error.bug "[load] Include: inconsistent ast for grs"
           | x -> Some (build_decl ~config x)
        ) ast in

    { filename;
      ast;
      decls;
    }

  let load ~config filename = from_ast ~config filename (Loader.grs filename)

  let parse ~config string_grs = from_ast ~config "" (Parser.grs string_grs)


  let request_string_to_json request = 
    let open Yojson.Basic.Util in
    try
      request 
      |> to_list 
      |> List.map 
        (fun item -> 
          item |> to_assoc |> 
          (function
          | ["pattern", l] -> sprintf "  pattern {%s}" (l |> to_list |> List.map to_string |> String.concat ";\n")
          | ["without", l] -> sprintf "  without {%s}" (l |> to_list |> List.map to_string |> String.concat ";\n")
          | ["global", l] -> sprintf "  global {%s}" (l |> to_list |> List.map to_string |> String.concat ";\n")
          | _ -> Error.build "[Grs.request_string_to_json]"
          )
        )  
      |> String.concat "\n" 
    with Type_error _ -> 
      printf "*********request*******\n%s\n****************\n%!" (Yojson.Basic.pretty_to_string request);
      Error.build "[Grs.request_string_to_json]"

  let request_of_json ~config request = 
    let ast = Parser.pattern (request_string_to_json request) in
    Request.of_ast ~config ast

  let rule_string_of_json request commands =
    let open Yojson.Basic.Util in
    let request_string =  request_string_to_json request in 
    let commands_string = try
      sprintf "\n  commands {%s}" (commands |> to_list |> List.map to_string |> String.concat ";\n") 
    with Type_error _ -> 
      printf "*********request*******\n%s\n****************\n%!" (Yojson.Basic.pretty_to_string request);
      Error.build "[Grs.rule_string_of_json]" in
    request_string ^ commands_string

  let rec decl_string_of_json (key, json) =
    let open Yojson.Basic.Util in
    try let strat = json |> to_string in sprintf "strat %s { %s }" key strat
    with Type_error _ ->
      try let assoc = json |> to_assoc in
        match (List.assoc_opt "request" assoc, List.assoc_opt "commands" assoc, List.assoc_opt "decls" assoc) with
        | (Some r, Some c, None) -> sprintf "rule %s {\n%s\n  }" key (rule_string_of_json r c)
        | (None, None, Some p) -> sprintf "package %s { %s }" key (p |> to_assoc |> List.map decl_string_of_json  |> String.concat "\n")
        | _ -> Error.build "[Grs.decl_string_of_json]"
      with Type_error _ -> 
        printf "****************\n%s\n****************\n%!" (Yojson.Basic.pretty_to_string json);
        Error.build "[Grs.decl_string_of_json]"

  let string_of_json json =
    let open Yojson.Basic.Util in
    try
      let decls = json |> member "decls" |> to_assoc in
      String.concat "\n" (List.map decl_string_of_json decls)
    with Type_error _ -> "[Grs.string_of_json]"

  let of_json ~config json =
    let s = string_of_json json in
    (* printf "*******!!!!!*********\n%s\n********!!!!!********\n%!" s; *)
    let ast = Parser.grs s in
    from_ast ~config "" ast









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
  let onf_pack_rewrite ~config decl_list graph =
    let rec loop = function
      | [] -> None
      | Rule r :: tail_decl ->
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
    | Some (Rule r,_) -> Rule.onf_apply_opt r graph
    | Some (Package (_, decl_list), _) -> onf_pack_rewrite decl_list graph
    | Some (Strategy (_,ast_strat), new_pointed) ->
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
      | Rule r :: tail_decl ->
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
    | Some (Rule r,_) -> Rule.owh_apply_opt ~config r gwh
    | Some (Package (_, decl_list), _) -> owh_pack_rewrite ~config decl_list gwh
    | Some (Strategy (_,ast_strat), new_pointed) ->
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
         | Rule r -> Graph_with_history_set.union acc (Rule.gwh_apply ~config r gwh)
         | _ -> acc
      ) Graph_with_history_set.empty decl_list

  let rec gwh_intern_simple_rewrite ~config pointed strat_name gwh =
    let path = Str.split (Str.regexp "\\.") strat_name in
    match search_from pointed path with
    | None -> Error.build "Simple rewrite, cannot find strat %s" strat_name
    | Some (Rule r,_) -> Rule.gwh_apply ~config r gwh
    | Some (Package (_, decl_list), _) -> gwh_pack_rewrite ~config decl_list gwh
    | Some (Strategy (_,ast_strat), new_pointed) ->
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
    | Ast.Onf s ->  Graph_with_history_set.singleton (owh_rewrite ~config pointed strat gwh)

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

  let eud2ud = load ~config:(Conllx_config.build "ud") (Filename.concat DATADIR "eud2ud.grs")
  let apply_eud2ud ~config graph =
    match simple_rewrite ~config eud2ud "main" graph with
    | [one] -> one
    | _ -> Error.run "the conversion eud2ud is not deterministic"

  let apply ~config grs_name graph =
    match grs_name with
    | "eud2ud" -> apply_eud2ud ~config graph
    | x -> Error.run "[Grs.apply] Unknown GRS \"%s\"" x


end (* module Grs *)







