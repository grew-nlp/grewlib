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
open Conll
open Conllx




module String_set = Grew_base.String_set

(* ==================================================================================================== *)
(** {2 Location} *)
(* ==================================================================================================== *)
module Loc = struct
  type t = Grew_base.Loc.t
  let to_string = Grew_base.Loc.to_string
end

(* ==================================================================================================== *)
(** {2 Exceptions} *)
(* ==================================================================================================== *)
module Libgrew = struct
  exception Error of string
  exception Bug of string

  let handle ?(name="") ?(file="No file defined") fct () =
    try fct () with
    (* Raise again already caught exceptions *)
    | Error msg -> raise (Error msg)
    | Bug msg -> raise (Bug msg)

    (* Catch new exceptions *)
    | Grew_base.Error.Parse (msg, Some loc) -> raise (Error (sprintf "%s %s" (Loc.to_string loc) msg))
    | Grew_base.Error.Parse (msg, None) -> raise (Error (sprintf "%s" msg))
    | Grew_base.Error.Build (msg, Some loc) -> raise (Error (sprintf "%s %s" (Loc.to_string loc) msg))
    | Grew_base.Error.Build (msg, None) -> raise (Error (sprintf "%s" msg))
    | Grew_base.Error.Run (msg, Some loc) -> raise (Error (sprintf "%s %s" (Loc.to_string loc) msg))
    | Grew_base.Error.Run (msg, None) -> raise (Error (sprintf "%s" msg))
    | Conll_error msg -> raise (Error (sprintf "Conll error: %s" (Yojson.Basic.pretty_to_string msg)))
    | Conllx_error msg -> raise (Error (sprintf "Conllx error: %s" (Yojson.Basic.pretty_to_string msg)))

    | Grew_base.Error.Bug (msg, Some loc) -> raise (Bug (sprintf "%s %s" (Loc.to_string loc) msg))
    | Grew_base.Error.Bug (msg, None) -> raise (Bug (sprintf "%s" msg))
    | Grew_base.Timeout.Stop bound -> raise (Error (sprintf "Timeout (running time execeeds %g seconds)" bound))
    | exc -> raise (Bug (sprintf "[Libgrew.%s] UNCAUGHT EXCEPTION: %s" name (Printexc.to_string exc)))

  let get_version () = VERSION

  let set_debug_mode flag = Grew_base.Global.debug := flag

  let set_safe_commands flag = Grew_base.Global.safe_commands := flag

  let set_track_rules flag = Grew_base.Global.track_rules := flag
end

(* ==================================================================================================== *)
(** {2 Domain} *)
(* ==================================================================================================== *)
module Domain = struct
  type t = Grew_domain.Domain.t

  let load filename =
    Libgrew.handle ~name:"Domain.load"
      (fun () ->
         let ast = Grew_loader.Loader.domain filename in
         Grew_grs.Grs.domain_build ast
      ) ()

  let feature_names domain =
    Libgrew.handle ~name:"Domain.feature_names"
      (fun () -> Grew_domain.Domain.feature_names domain)
      ()

  let dump domain =
    Libgrew.handle ~name:"Domain.dump"
      (fun () -> Grew_domain.Domain.dump domain)
      ()

end

(* ==================================================================================================== *)
(** {2 Patterns} *)
(* ==================================================================================================== *)
module Pattern = struct
  type t = Grew_rule.Pattern.t

  let load ?domain ~config file =
    Libgrew.handle ~name:"Pattern.load" (fun () -> Grew_rule.Pattern.of_ast ?domain ~config (Grew_loader.Loader.pattern file)) ()

  let parse ?domain ~config desc =
    Libgrew.handle ~name:"Pattern.load" (fun () -> Grew_rule.Pattern.of_ast ?domain ~config (Grew_loader.Parser.pattern desc)) ()

  let pid_name_list pattern =
    Libgrew.handle ~name:"Pattern.pid_list"
      (fun () -> List.map (fun x -> x) (Grew_rule.Pattern.pid_name_list pattern)
      ) ()
end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching = struct
  type t = Grew_rule.Matching.t

  let to_json ?(all_edges=false) pattern graph t = Grew_rule.Matching.to_json ~all_edges pattern graph t

  let nodes pattern graph matching =
    Libgrew.handle ~name:"Matching.nodes" (fun () ->
        Grew_rule.Matching.node_matching pattern graph matching
      ) ()

  let get_value_opt ~config request pattern graph matching =
    Libgrew.handle ~name:"Matching.get_value_opt" (fun () ->
        Grew_rule.Matching.get_string_value_opt ~config request pattern graph matching
      ) ()
end

(* ==================================================================================================== *)
(** {2 Deco} *)
(* ==================================================================================================== *)
module Deco = struct
  type t = Grew_graph.G_deco.t
  let build pattern matching = Grew_rule.Matching.match_deco pattern matching
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

  let load_gr ?domain ~config file =
    if not (Sys.file_exists file)
    then raise (Libgrew.Error ("File_not_found: " ^ file))
    else
      Libgrew.handle ~name:"Graph.load_gr" ~file
        (fun () ->
           let gr_ast = Grew_loader.Loader.gr file in
           Grew_graph.G_graph.of_ast ?domain ~config gr_ast
        ) ()

  let load_conll ?domain file =
    Libgrew.handle ~name:"Graph.load_conll" ~file
      (fun () ->
         Grew_graph.G_graph.of_conll ?domain (Conll.load file)
      ) ()

  let load_brown ?domain file =
    Libgrew.handle ~name:"Graph.load_brown"
      (fun () ->
         let brown = Grew_base.File.load file in
         Grew_graph.G_graph.of_brown ?domain brown
      ) ()

  let load_pst ?domain file =
    if not (Sys.file_exists file)
    then raise (Libgrew.Error ("File_not_found: " ^ file))
    else
      Libgrew.handle ~name:"load_pst" ~file
        (fun () ->
           let const_ast = Grew_loader.Loader.phrase_structure_tree file in
           Grew_graph.G_graph.of_pst ?domain const_ast
        ) ()

  let load ?domain ~config file =
    Libgrew.handle ~name:"Graph.load_graph" ~file
      (fun () ->
         match Grew_base.File.get_suffix_opt file with
         | Some ".gr" -> load_gr ?domain ~config file
         | Some ".conll" | Some ".conllu" -> load_conll ~config ?domain file
         | Some ".br" | Some ".melt" -> load_brown ~config ?domain file
         | Some ".cst" -> load_pst ?domain file
         | _ ->
           Grew_base.Error.warning "Unknown file format for input graph '%s', try to guess..." file;
           let rec loop = function
             | [] -> Grew_base.Error.bug "[Libgrew.load_graph] Cannot guess input file format of file '%s'." file
             | load_fct :: tail -> try load_fct ?domain file with _ -> loop tail in
           loop [load_gr ~config; load_conll ~config; load_brown ~config; load_pst]
      ) ()

  let of_gr ?domain ~config gr_string =
    Libgrew.handle ~name:"Graph.of_gr" (fun () -> Grew_graph.G_graph.of_ast ?domain ~config (Grew_loader.Parser.gr gr_string)) ()

  let of_conll ?domain ~config conll =
    Libgrew.handle ~name:"Graph.of_conll" (fun () -> Grew_graph.G_graph.of_conll ?domain ~config conll) ()

  let of_pst ?domain pst_string =
    Libgrew.handle ~name:"of_pst"
      (fun () ->
         let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
         (Grew_graph.G_graph.of_pst ?domain pst_ast)
      ) ()

  let sentence_of_pst ?domain pst_string =
    Libgrew.handle ~name:"of_pst"
      (fun () ->
         let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
         let word_list = Grew_ast.Ast.word_list pst_ast in
         Sentence.fr_clean_spaces (String.concat " " word_list)
      ) ()

  let of_json_python ~config json =
    Libgrew.handle ~name:"Graph.of_json_python" (fun () -> Grew_graph.G_graph.of_json_python ~config json) ()

  let of_brown ?domain ~config ?sentid brown =
    Libgrew.handle ~name:"Graph.of_brown" (fun () -> Grew_graph.G_graph.of_brown ?domain ~config ?sentid brown) ()

  let to_dot ?main_feat ~config ?(deco=Grew_graph.G_deco.empty) ?get_url graph =
    Libgrew.handle ~name:"Graph.to_dot" (fun () -> Grew_graph.G_graph.to_dot ?main_feat ?get_url ~config graph ~deco) ()

  let to_dep ?filter ?main_feat ?(deco=Grew_graph.G_deco.empty) ~config graph =
    Libgrew.handle ~name:"Graph.to_dep" (fun () -> Grew_graph.G_graph.to_dep ?filter ?main_feat ~deco ~config graph) ()

  let to_gr ~config graph  =
    Libgrew.handle ~name:"Graph.to_gr" (fun () -> Grew_graph.G_graph.to_gr ~config graph) ()

  let to_json_python graph =
    Libgrew.handle ~name:"Graph.to_json_python" (fun () -> Grew_graph.G_graph.to_json_python graph) ()

  let to_conll ~config graph =
    Libgrew.handle ~name:"Graph.to_conll" (fun () -> Grew_graph.G_graph.to_conll ~config graph) ()

  let of_json graph =
    Libgrew.handle ~name:"Graph.of_json" (fun () -> Grew_graph.G_graph.of_json graph) ()

  let to_json graph =
    Libgrew.handle ~name:"Graph.to_json" (fun () -> Grew_graph.G_graph.to_json graph) ()

  let to_conll_string ?cupt ~config graph =
    Libgrew.handle ~name:"Graph.to_conll_string" (fun () -> Grew_graph.G_graph.to_conll_string ?cupt ~config graph) ()

  let to_sentence ?pivot ?deco gr =
    Libgrew.handle ~name:"Graph.to_sentence"
      (fun () ->
         Grew_graph.G_graph.to_sentence ?pivot ?deco gr
      ) ()

  let to_orfeo ?deco gr =
    Libgrew.handle ~name:"Graph.to_orfeo"
      (fun () ->
         Grew_graph.G_graph.to_orfeo ?deco gr
      ) ()

  (* let save_conll ~config filename graph =
     Libgrew.handle ~name:"Graph.save_conll" (fun () ->
        let out_ch = open_out filename in
        fprintf out_ch "%s" (Grew_graph.G_graph.to_conll_string ~config graph);
        close_out out_ch
      ) () *)

  let search_pattern ?domain ~config pattern graph =
    Libgrew.handle ~name:"Graph.search_pattern" (fun () ->
        Grew_rule.Matching.match_in_graph ?domain ~config pattern graph
      ) ()

  let get_feature_values feature_name t =
    Grew_graph.G_graph.get_feature_values feature_name t
end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs = struct
  type t = Grew_grs.Grs.t

  let empty = Grew_grs.Grs.empty

  let load ~config file =
    Libgrew.handle ~name:"Grs.load" ~file
      (fun () ->
         Grew_grs.Grs.load ~config file
      ) ()

  let parse ~config file =
    Libgrew.handle ~name:"Grs.parse" ~file
      (fun () ->
         Grew_grs.Grs.parse ~config file
      ) ()

  let dump grs =
    Libgrew.handle ~name:"Grs.dump"
      (fun () ->
         Grew_grs.Grs.dump grs
      ) ()

  let domain_opt grs =
    Libgrew.handle ~name:"Grs.domain"
      (fun () ->
         Grew_grs.Grs.domain_opt grs
      ) ()

  let to_json_python ~config grs =
    Libgrew.handle ~name:"Grs.to_json_python"
      (fun () ->
         Grew_grs.Grs.to_json_python ~config grs
      ) ()

  let get_strat_list grs =
    Libgrew.handle ~name:"Grs.get_strat_list"
      (fun () ->
         Grew_grs.Grs.get_strat_list grs
      ) ()
end

(* ==================================================================================================== *)
(** {2 Rewrite} *)
(* ==================================================================================================== *)
module Rewrite = struct
  type display = Libgrew_types.rew_display

  let size = Libgrew_types.rew_display_size

  let set_max_rules bound = Grew_rule.Rule.set_max_rules bound

  let display ~config gr grs strat =
    Libgrew.handle ~name:"Rewrite.display" (fun () -> Grew_grs.Grs.wrd_rewrite ~config grs strat gr) ()

  let set_timeout t = Grew_base.Timeout.timeout := t

  let simple_rewrite ~config gr grs strat =
    Libgrew.handle ~name:"Rewrite.simple_rewrite" (fun () -> Grew_grs.Grs.simple_rewrite ~config grs strat gr) ()

  let onf_rewrite_opt ~config gr grs strat =
    Libgrew.handle ~name:"Rewrite.onf_rewrite_opt" (fun () -> Grew_grs.Grs.onf_rewrite_opt ~config grs strat gr) ()

  let at_least_one grs strat =
    Libgrew.handle ~name:"Rewrite.at_least_one" (fun () -> Grew_grs.Grs.at_least_one grs strat) ()
  let at_most_one grs strat =
    Libgrew.handle ~name:"Rewrite.at_most_one" (fun () -> Grew_grs.Grs.at_most_one grs strat) ()
end

(* ==================================================================================================== *)
(** {2 Corpus} *)
(* ==================================================================================================== *)
module Corpus = struct
  type t = Grew_corpus.Corpus.t

  let size = Grew_corpus.Corpus.size
  let get_domain_opt = Grew_corpus.Corpus.get_domain_opt
  let permut_length = Grew_corpus.Corpus.permut_length

  let get_graph position t =
    Libgrew.handle ~name:"Corpus.get_graph" (fun () -> Grew_corpus.Corpus.get_graph position t) ()

  let get_sent_id position t =
    Libgrew.handle ~name:"Corpus.get_sent_id" (fun () -> Grew_corpus.Corpus.get_sent_id position t) ()

  let is_conll position t =
    Libgrew.handle ~name:"Corpus.is_conll" (fun () -> Grew_corpus.Corpus.is_conll position t) ()

  let get_text position t =
    Libgrew.handle ~name:"Corpus.get_text" (fun () -> Grew_corpus.Corpus.get_text position t) ()

  let fold_left fct init t =
    Libgrew.handle ~name:"Corpus.fold_left" (fun () -> Grew_corpus.Corpus.fold_left fct init t) ()

  let iteri fct t =
    Libgrew.handle ~name:"Corpus.iteri" (fun () -> Grew_corpus.Corpus.iteri fct t) ()

  let from_stdin ?config () =
    Libgrew.handle ~name:"Corpus.from_stdin" (fun () -> Grew_corpus.Corpus.from_stdin ?config ()) ()

  let from_file ?config file =
    Libgrew.handle ~name:"Corpus.from_file" (fun () -> Grew_corpus.Corpus.from_file ?config file) ()

  let from_dir ?config dir =
    Libgrew.handle ~name:"Corpus.from_dir" (fun () -> Grew_corpus.Corpus.from_dir ?config dir) ()

  let merge corpus_list =
    Libgrew.handle ~name:"Corpus.merge" (fun () -> Grew_corpus.Corpus.merge corpus_list) ()

  let singleton graph =
    Libgrew.handle ~name:"Corpus.singleton" (fun () -> Grew_corpus.Corpus.singleton graph) ()
end



module Corpus_desc = struct

  type t = Grew_corpus.Corpus_desc.t

  let get_directory = Grew_corpus.Corpus_desc.get_directory
  let get_id = Grew_corpus.Corpus_desc.get_id
  let get_config = Grew_corpus.Corpus_desc.get_config
  let is_rtl = Grew_corpus.Corpus_desc.is_rtl
  let is_audio = Grew_corpus.Corpus_desc.is_audio

  let build_corpus t =
    Libgrew.handle ~name:"Corpus.build_corpus" (fun () -> Grew_corpus.Corpus_desc.build_corpus t) ()

  let load_corpus_opt t =
    Libgrew.handle ~name:"Corpus.load_corpus_opt" (fun () -> Grew_corpus.Corpus_desc.load_corpus_opt t) ()

  let load_json grs =
    Libgrew.handle ~name:"Corpus.load_json" (fun () -> Grew_corpus.Corpus_desc.load_json grs) ()

  let compile ?grew_match t =
    Libgrew.handle ~name:"Corpus.compile" (fun () -> Grew_corpus.Corpus_desc.compile ?grew_match t) ()

  let clean t =
    Libgrew.handle ~name:"Corpus.clean" (fun () -> Grew_corpus.Corpus_desc.clean t) ()
end
