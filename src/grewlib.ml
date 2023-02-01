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
open Conll

include Grew_types

(* ==================================================================================================== *)
(** {2 Exceptions} *)
(* ==================================================================================================== *)
module Grewlib = struct
  exception Error of string
  exception Bug of string

  let handle ?(name="") fct () =
    try fct () with
    (* Raise again already caught exceptions *)
    | Error msg -> raise (Error msg)
    | Bug msg -> raise (Bug msg)

    (* Catch new exceptions *)
    | Grew_utils.Error.Parse (msg, Some loc) -> raise (Error (sprintf "%s %s" (Grew_utils.Loc.to_string loc) msg))
    | Grew_utils.Error.Parse (msg, None) -> raise (Error (sprintf "%s" msg))
    | Grew_utils.Error.Build (msg, Some loc) -> raise (Error (sprintf "%s %s" (Grew_utils.Loc.to_string loc) msg))
    | Grew_utils.Error.Build (msg, None) -> raise (Error (sprintf "%s" msg))
    | Grew_utils.Error.Run (msg, Some loc) -> raise (Error (sprintf "%s %s" (Grew_utils.Loc.to_string loc) msg))
    | Grew_utils.Error.Run (msg, None) -> raise (Error (sprintf "%s" msg))
    | Conll_error msg -> raise (Error (sprintf "Conll error: %s" (Yojson.Basic.pretty_to_string msg)))

    | Grew_utils.Error.Bug (msg, Some loc) -> raise (Bug (sprintf "%s %s" (Grew_utils.Loc.to_string loc) msg))
    | Grew_utils.Error.Bug (msg, None) -> raise (Bug (sprintf "%s" msg))
    | Grew_utils.Timeout.Stop bound -> raise (Error (sprintf "Timeout (running time execeeds %g seconds)" bound))
    | exc -> raise (Bug (sprintf "[Grewlib.%s] UNCAUGHT EXCEPTION: %s" name (Printexc.to_string exc)))

  let get_version () =
    match Build_info.V1.version () with
    | None -> "dev"
    | Some v -> Build_info.V1.Version.to_string v
  
  let set_debug_mode flag = Grew_utils.Global.debug := flag

  let set_safe_commands flag = Grew_utils.Global.safe_commands := flag

  let set_track_rules flag = Grew_utils.Global.track_rules := flag
  let set_track_history flag = Grew_utils.Global.track_history:= flag
  let set_track_impact flag = Grew_utils.Global.track_impact:= flag
end

(* ==================================================================================================== *)
(** {2 Deco} *)
(* ==================================================================================================== *)
module Deco = struct
  type t = Grew_graph.G_deco.t
end

(* ==================================================================================================== *)
(** {2 Graph} *)
(* ==================================================================================================== *)
module Graph = struct
  type t = Grew_graph.G_graph.t

  let size t = Grew_graph.G_graph.size t

  let get_meta_opt key t = Grew_graph.G_graph.get_meta_opt key t

  let get_meta_list t = Grew_graph.G_graph.get_meta_list t

  let set_meta key value t = Grew_graph.G_graph.set_meta key value t

  let append_in_ag_lex feature_name_list t ag_lex = Grew_graph.G_graph.append_in_ag_lex feature_name_list t ag_lex

  let load_conll ~config file =
    Grewlib.handle ~name:"Graph.load_conll"
      (fun () ->
        Conll.load ~config file |> Conll.to_json |> Grew_graph.G_graph.of_json
      ) ()

  let load_pst file =
    if not (Sys.file_exists file)
    then raise (Grewlib.Error ("File_not_found: " ^ file))
    else
      Grewlib.handle ~name:"load_pst"
        (fun () ->
           let const_ast = Grew_loader.Loader.phrase_structure_tree file in
           Grew_graph.G_graph.of_pst const_ast
        ) ()

  let load ~config file =
    Grewlib.handle ~name:"Graph.load_graph"
      (fun () ->
         match Grew_utils.String_.get_suffix_opt file with
         | Some ".conll" | Some ".conllu" -> load_conll ~config file
         | Some ".cst" -> load_pst file
         | _ ->
           Grew_utils.Error.warning "Unknown file format for input graph '%s', try to guess..." file;
           let rec loop = function
             | [] -> Grew_utils.Error.bug "[Grewlib.load_graph] Cannot guess input file format of file '%s'." file
             | load_fct :: tail -> try load_fct file with _ -> loop tail in
           loop [load_conll ~config; load_pst]
      ) ()

  let of_pst pst_string =
    Grewlib.handle ~name:"of_pst"
      (fun () ->
         let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
         (Grew_graph.G_graph.of_pst pst_ast)
      ) ()

  let sentence_of_pst pst_string =
    Grewlib.handle ~name:"of_pst"
      (fun () ->
         let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
         let word_list = Grew_ast.Ast.word_list pst_ast in
         String.concat " " word_list
      ) ()

  let to_dot ?main_feat ~config ?(deco=Grew_graph.G_deco.empty) graph =
    Grewlib.handle ~name:"Graph.to_dot" (fun () -> Grew_graph.G_graph.to_dot ?main_feat ~config graph ~deco) ()

  let to_dep ?filter ?no_root ?main_feat ?(deco=Grew_graph.G_deco.empty) ~config graph =
    Grewlib.handle ~name:"Graph.to_dep" (fun () -> Grew_graph.G_graph.to_dep ?filter ?no_root ?main_feat ~deco ~config graph) ()

  let of_json graph =
    Grewlib.handle ~name:"Graph.of_json" (fun () -> Grew_graph.G_graph.of_json graph) ()

  let to_json graph =
    Grewlib.handle ~name:"Graph.to_json" (fun () -> Grew_graph.G_graph.to_json graph) ()

  let to_sentence ?pivot ?deco gr =
    Grewlib.handle ~name:"Graph.to_sentence"
      (fun () ->
         Grew_graph.G_graph.to_sentence ?pivot ?deco gr
      ) ()

  let to_sentence_audio ?deco gr =
    Grewlib.handle ~name:"Graph.to_sentence_audio"
      (fun () ->
         Grew_graph.G_graph.to_sentence_audio ?deco gr
      ) ()

  let get_feature_values feature_name t =
    Grew_graph.G_graph.get_feature_values feature_name t

  let get_relations ~config t =
    Grew_graph.G_graph.get_relations ~config t

  let get_features t =
    Grew_graph.G_graph.get_features t

  let get_history t =
    Grew_graph.G_graph.get_history t

  let trace_depth t =
    Grew_graph.G_graph.trace_depth t
end

(* ==================================================================================================== *)
(** {2 Requests} *)
(* ==================================================================================================== *)
module Request = struct
  type t = Grew_rule.Request.t

  type basic = Grew_rule.Request.basic

  let load ~config file =
    Grewlib.handle ~name:"Request.load" (fun () -> Grew_rule.Request.of_ast ~config (Grew_loader.Loader.request file)) ()

  let parse ~config desc =
    Grewlib.handle ~name:"Request.parse" (fun () -> Grew_rule.Request.of_ast ~config (Grew_loader.Parser.request desc)) ()

  let parse_basic ~config request desc =
    Grewlib.handle
      ~name:"Request.parse_basic"
      (fun () -> Grew_rule.Request.build_whether ~config request (Grew_loader.Parser.basic desc)) ()

  let pid_name_list request =
    Grewlib.handle ~name:"Request.pid_list"
      (fun () -> List.map (fun x -> x) (Grew_rule.Request.pid_name_list request)
      ) ()
  let of_json ~config grs =
    Grewlib.handle ~name:"Request.of_json"
      (fun () ->
         Grew_rule.Request.of_json ~config grs
      ) ()
  
end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching = struct
  type t = Grew_rule.Matching.t

  let to_json ?(all_edges=false) request graph t = Grew_rule.Matching.to_json ~all_edges request graph t

  let nodes request graph matching =
    Grewlib.handle ~name:"Matching.nodes" (fun () ->
        Grew_rule.Matching.node_matching request graph matching
      ) ()

  let get_value_opt ~config cluster_key request graph matching =
    Grewlib.handle ~name:"Matching.get_value_opt" (fun () ->
        Grew_rule.Matching.get_value_opt ~config cluster_key request graph matching
      ) ()

  let whether ~config extension request graph matching =
    Grewlib.handle ~name:"Matching.whether" (fun () ->
        Grew_rule.Matching.whether ~config extension request graph matching
      ) ()

  let subgraph graph matching depth =
    Grewlib.handle ~name:"Matching.subgraph" (fun () ->
        Grew_rule.Matching.subgraph graph matching depth
      ) ()

  let search_request_in_graph ~config request graph =
    Grewlib.handle ~name:"Matching.search_request_in_graph" (fun () ->
      Grew_rule.Matching.search_request_in_graph ~config request graph
    ) ()

  let build_deco request matching = Grew_rule.Matching.build_deco request matching
  let get_clust_value_opt =
    Grewlib.handle ~name:"Matching.get_clust_value_opt" 
    (fun () -> Grew_rule.Matching.get_clust_value_opt) ()
end


(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs = struct
  type t = Grew_grs.Grs.t

  let empty = Grew_grs.Grs.empty

  let load ~config file =
    Grewlib.handle ~name:"Grs.load"
      (fun () ->
         Grew_grs.Grs.load ~config file
      ) ()

  let parse ~config file =
    Grewlib.handle ~name:"Grs.parse"
      (fun () ->
         Grew_grs.Grs.parse ~config file
      ) ()

  let to_json ~config grs =
    Grewlib.handle ~name:"Grs.to_json"
      (fun () ->
         Grew_grs.Grs.to_json ~config grs
      ) ()

  let dump grs =
    Grewlib.handle ~name:"Grs.dump"
      (fun () ->
         Grew_grs.Grs.dump grs
      ) ()

  let get_strat_list grs =
    Grewlib.handle ~name:"Grs.get_strat_list"
      (fun () ->
         Grew_grs.Grs.get_strat_list grs
      ) ()

  let get_strat_lists grs =
    Grewlib.handle ~name:"Grs.get_strat_lists"
      (fun () ->
         Grew_grs.Grs.get_strat_lists grs
      ) ()

  let get_package_list grs =
    Grewlib.handle ~name:"Grs.get_package_list"
      (fun () ->
         Grew_grs.Grs.get_package_list grs
      ) ()

  let get_rule_list grs =
    Grewlib.handle ~name:"Grs.get_rule_list"
      (fun () ->
         Grew_grs.Grs.get_rule_list grs
      ) ()

  let of_json ~config grs =
    Grewlib.handle ~name:"Grs.of_json"
      (fun () ->
         Grew_grs.Grs.of_json ~config grs
      ) ()
end

(* ==================================================================================================== *)
(** {2 Rewrite} *)
(* ==================================================================================================== *)
module Rewrite = struct
  let set_max_rules bound = Grew_rule.Rule.set_max_rules bound

  let set_timeout t = Grew_utils.Timeout.timeout := t

  let simple_rewrite ~config gr grs strat =
    Grewlib.handle ~name:"Rewrite.simple_rewrite" (fun () -> Grew_grs.Grs.simple_rewrite ~config grs strat gr) ()

  let log_rewrite () =
    `Assoc [("rules", `Int (Grew_rule.Rule.get_nb_rules ())); ("time", `Float (Grew_utils.Timeout.get_duration()))]

  let onf_rewrite_opt ~config gr grs strat =
    Grewlib.handle ~name:"Rewrite.onf_rewrite_opt" (fun () -> Grew_grs.Grs.onf_rewrite_opt ~config grs strat gr) ()
end

(* ==================================================================================================== *)
(** {2 Corpus} *)
(* ==================================================================================================== *)
module Corpus = struct
  type t = Grew_corpus.Corpus.t

  let size = Grew_corpus.Corpus.size
  let permut_length = Grew_corpus.Corpus.permut_length

  let graph_of_sent_id sent_id t =
    Grewlib.handle ~name:"Corpus.graph_of_sent_id" (fun () -> Grew_corpus.Corpus.graph_of_sent_id sent_id t) ()

  let get_graph position t =
    Grewlib.handle ~name:"Corpus.get_graph" (fun () -> Grew_corpus.Corpus.get_graph position t) ()

  let get_sent_id position t =
    Grewlib.handle ~name:"Corpus.get_sent_id" (fun () -> Grew_corpus.Corpus.get_sent_id position t) ()

  let is_conll t =
    Grewlib.handle ~name:"Corpus.is_conll" (fun () -> Grew_corpus.Corpus.is_conll t) ()

  let get_text position t =
    Grewlib.handle ~name:"Corpus.get_text" (fun () -> Grew_corpus.Corpus.get_text position t) ()

  let update_graph position t =
    Grewlib.handle ~name:"Corpus.update_graph" (fun () -> Grew_corpus.Corpus.update_graph position t) ()

  let fold_left fct init t =
    Grewlib.handle ~name:"Corpus.fold_left" (fun () -> Grew_corpus.Corpus.fold_left fct init t) ()

  let fold_right fct t init =
    Grewlib.handle ~name:"Corpus.fold_left" (fun () -> Grew_corpus.Corpus.fold_right fct t init) ()

  (* NB: no handle here because it's sensible to raise out onw exception in [iteri] *)
  let iteri = Grew_corpus.Corpus.iteri

  let of_conllx_corpus conllx_corpus =
    Grewlib.handle ~name:"Corpus.of_conllx_corpus" (fun () -> Grew_corpus.Corpus.of_conllx_corpus conllx_corpus) ()

  let from_stdin ?ext ?log_file ?config () =
    Grewlib.handle ~name:"Corpus.from_stdin" (fun () -> Grew_corpus.Corpus.from_stdin ?ext ?log_file ?config ()) ()

  let from_string ?ext ?log_file ?config s =
    Grewlib.handle ~name:"Corpus.from_string" (fun () -> Grew_corpus.Corpus.from_string ?ext ?log_file ?config s) ()

  let from_file ?ext ?log_file ?config file =
    Grewlib.handle ~name:"Corpus.from_file" (fun () -> Grew_corpus.Corpus.from_file ?ext ?log_file ?config file) ()

  let from_dir ?config dir =
    Grewlib.handle ~name:"Corpus.from_dir" (fun () -> Grew_corpus.Corpus.from_dir ?config dir) ()

  let from_assoc_list list =
    Grewlib.handle ~name:"Corpus.from_assoc_list" (fun () -> Grew_corpus.Corpus.from_assoc_list list) ()

  let merge corpus_list =
    Grewlib.handle ~name:"Corpus.merge" (fun () -> Grew_corpus.Corpus.merge corpus_list) ()

  let get_columns_opt = Grew_corpus.Corpus.get_columns_opt

  let search ?(json_label=false) ~config null update request cluster_item_list corpus = 
    Grewlib.handle ~name:"Corpus.search" 
    (fun () -> Grew_corpus.Corpus.search ~json_label ~config null update request cluster_item_list corpus) ()

  let bounded_search ~config ?ordering bound timeout null update request cluster_item_list corpus =
    Grewlib.handle ~name:"Corpus.search" 
    (fun () -> Grew_corpus.Corpus.bounded_search ~config ?ordering bound timeout null update request cluster_item_list corpus) ()

end


(* ==================================================================================================== *)
(** {2 Corpus_desc} *)
(* ==================================================================================================== *)
module Corpus_desc = struct

  type t = Grew_corpus.Corpus_desc.t

  let get_directory = Grew_corpus.Corpus_desc.get_directory
  let get_id = Grew_corpus.Corpus_desc.get_id
  let get_lang_opt = Grew_corpus.Corpus_desc.get_lang_opt
  let get_config = Grew_corpus.Corpus_desc.get_config
  let is_rtl = Grew_corpus.Corpus_desc.is_rtl
  let get_display = Grew_corpus.Corpus_desc.get_display
  let is_audio = Grew_corpus.Corpus_desc.is_audio

  let build id directory =
    Grewlib.handle ~name:"Corpus.build" (fun () -> Grew_corpus.Corpus_desc.build id directory) ()

  let build_corpus t =
    Grewlib.handle ~name:"Corpus.build_corpus" (fun () -> Grew_corpus.Corpus_desc.build_corpus t) ()

  let load_corpus_opt t =
    Grewlib.handle ~name:"Corpus.load_corpus_opt" (fun () -> Grew_corpus.Corpus_desc.load_corpus_opt t) ()

  let load_json grs =
    Grewlib.handle ~name:"Corpus.load_json" (fun () -> Grew_corpus.Corpus_desc.load_json grs) ()

  let compile ?force ?grew_match t =
    Grewlib.handle ~name:"Corpus.compile" (fun () -> Grew_corpus.Corpus_desc.compile ?force ?grew_match t) ()

  let clean t =
    Grewlib.handle ~name:"Corpus.clean" (fun () -> Grew_corpus.Corpus_desc.clean t) ()
end


module Sbn = struct
  let to_json t =
    Grewlib.handle ~name:"Sbn.to_json" (fun () -> Grew_utils.Sbn.to_json t) ()
end