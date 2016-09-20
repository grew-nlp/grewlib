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
open Conll

let libgrew_debug_mode () = Grew_base.Global.debug := true
let get_version () = VERSION

(* ==================================================================================================== *)
(** {2 Location} *)
(* ==================================================================================================== *)
module Loc = struct
  type t = Grew_base.Loc.t
  let to_string = Grew_base.Loc.to_string
  let to_line = Grew_base.Loc.to_line
end

(* ==================================================================================================== *)
(** {2 Exceptions} *)
(* ==================================================================================================== *)
exception File_not_found of string
exception Parsing_err of string * Loc.t option
exception Build of string * Loc.t option
exception Run of string * Loc.t option
exception Bug of string * Loc.t option

let handle ?(name="") ?(file="No file defined") fct () =
  try fct () with
    (* Raise again already caught exceptions *)
    | Parsing_err (msg,loc_opt) -> raise (Parsing_err (msg,loc_opt))
    | Build (msg,loc_opt) -> raise (Build (msg,loc_opt))
    | Bug (msg, loc_opt) -> raise (Bug (msg,loc_opt))
    | Run (msg, loc_opt) -> raise (Run (msg,loc_opt))
    | File_not_found file -> raise (File_not_found file)

    (* Catch new exceptions *)
    | Grew_base.Error.Parse (msg, loc_opt) -> raise (Parsing_err (msg, loc_opt))
    | Grew_base.Error.Build (msg, loc_opt) -> raise (Build (msg, loc_opt))
    | Grew_base.Error.Bug (msg, loc_opt) -> raise (Bug (msg,loc_opt))
    | Grew_base.Error.Run (msg, loc_opt) -> raise (Run (msg,loc_opt))

    | Conll.Error msg -> raise (Parsing_err (msg,None))

    | exc -> raise (Bug (sprintf "[Libgrew.%s] UNCAUGHT EXCEPTION: %s" name (Printexc.to_string exc), None))


(* ==================================================================================================== *)
(** {2 Domain} *)
(* ==================================================================================================== *)
module Domain = struct
  type t = Grew_types.Domain.t

  let load filename =
    let ast = Grew_loader.Loader.domain filename in
    Grew_grs.Grs.domain_build ast

  let feature_names domain =  handle ~name:"feature_names" (fun () -> Grew_types.Domain.feature_names domain) ()
end

(* ==================================================================================================== *)
(** {2 Patterns} *)
(* ==================================================================================================== *)
module Pattern = struct
  type t = Grew_rule.Rule.pattern

  let load ?domain file =
  handle ~name:"Pattern.load" (fun () -> Grew_rule.Rule.build_pattern ?domain (Grew_loader.Loader.pattern file)) ()

  let parse ?domain desc =
  handle ~name:"Pattern.load" (fun () -> Grew_rule.Rule.build_pattern ?domain (Grew_loader.Parser.pattern desc)) ()
end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching = struct
  type t = Grew_rule.Rule.matching

  let to_python pattern graph t = Grew_rule.Rule.to_python pattern graph t
end

(* ==================================================================================================== *)
(** {2 Deco} *)
(* ==================================================================================================== *)
module Deco = struct
  type t = Grew_graph.G_deco.t
  let build pattern matching = Grew_rule.Rule.match_deco pattern matching
end

(* ==================================================================================================== *)
(** {2 Graph} *)
(* ==================================================================================================== *)
module Graph = struct


type t = Grew_graph.G_graph.t

  let load_gr ?domain file =
    if not (Sys.file_exists file)
    then raise (File_not_found file)
    else
      handle ~name:"Graph.load_gr" ~file
        (fun () ->
          let gr_ast = Grew_loader.Loader.gr file in
          Grew_graph.G_graph.build ?domain gr_ast
        ) ()

  let load_conll ?domain file =
    handle ~name:"Graph.load_conll" ~file
      (fun () ->
        Grew_graph.G_graph.of_conll ?domain (Conll.load file)
      ) ()

  let load_brown ?domain file =
    handle ~name:"Graph.load_brown"
      (fun () ->
        let brown = Grew_base.File.load file in
        Grew_graph.G_graph.of_brown ?domain brown
      ) ()

  let load_pst ?domain file =
    if not (Sys.file_exists file)
    then raise (File_not_found file)
    else
      handle ~name:"load_pst" ~file
        (fun () ->
          let const_ast = Grew_loader.Loader.phrase_structure_tree file in
          Grew_graph.G_graph.of_pst ?domain const_ast
        ) ()

  let load ?domain file =
    handle ~name:"Graph.load_graph" ~file
      (fun () ->
        match Grew_base.File.get_suffix file with
        | Some ".gr" -> load_gr ?domain file
        | Some ".conll" -> load_conll ?domain file
        | Some ".br" | Some ".melt" -> load_brown ?domain file
        | Some ".cst" -> load_pst ?domain file
        | _ ->
            Log.fwarning "Unknown file format for input graph '%s', try to guess..." file;
            let rec loop = function
            | [] -> Log.fcritical "[Libgrew.load_graph] Cannot guess input file format of file '%s'. Use .gr or .conll file extension" file
            | load_fct :: tail -> try load_fct ?domain file with _ -> loop tail in
            loop [load_gr; load_conll; load_brown; load_pst]
      ) ()

  let of_gr ?domain ?(grewpy=false) gr_string =
    handle ~name:"Graph.of_gr" (fun () -> Grew_graph.G_graph.build ?domain ~grewpy (Grew_loader.Parser.gr gr_string)) ()

  let of_conll ?domain conll =
    handle ~name:"Graph.of_conll" (fun () -> Grew_graph.G_graph.of_conll ?domain conll) ()

  let of_pst ?domain pst_string =
    handle ~name:"of_pst"
      (fun () ->
        let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
        (Grew_graph.G_graph.of_pst ?domain pst_ast)
      ) ()

  let sentence_of_pst ?domain pst_string =
    handle ~name:"of_pst"
      (fun () ->
        let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
        let word_list = Grew_ast.Ast.word_list pst_ast in
        Sentence.fr_clean_spaces (String.concat " " word_list)
      ) ()

  let of_brown ?domain ?sentid brown =
    handle ~name:"Graph.of_brown" (fun () -> Grew_graph.G_graph.of_brown ?domain ?sentid brown) ()

  let to_dot ?domain ?main_feat ?(deco=Grew_graph.G_deco.empty) graph =
    handle ~name:"Graph.to_dot" (fun () -> Grew_graph.G_graph.to_dot ?domain ?main_feat graph ~deco) ()

  let to_dep ?domain ?filter ?main_feat ?(deco=Grew_graph.G_deco.empty) graph =
    handle ~name:"Graph.to_dep" (fun () -> Grew_graph.G_graph.to_dep ?domain ?filter ?main_feat ~deco graph) ()

  let to_gr ?domain graph =
    handle ~name:"Graph.to_gr" (fun () -> Grew_graph.G_graph.to_gr ?domain graph) ()

  let to_conll_string ?domain graph =
    handle ~name:"Graph.to_conll_string" (fun () -> Grew_graph.G_graph.to_conll_string ?domain graph) ()

  let to_sentence ?main_feat gr =
    handle ~name:"Graph.to_sentence"
      (fun () ->
        Grew_graph.G_graph.to_sentence ?main_feat gr
      ) ()

  let save_conll ?domain filename graph =
    handle ~name:"Graph.save_conll" (fun () ->
      let out_ch = open_out filename in
      fprintf out_ch "%s" (Grew_graph.G_graph.to_conll_string ?domain graph);
      close_out out_ch
    ) ()

  let search_pattern ?domain pattern graph = Grew_rule.Rule.match_in_graph ?domain pattern graph

  let node_matching pattern graph matching  = Grew_rule.Rule.node_matching pattern graph matching

end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs = struct
  type t = Grew_grs.Grs.t

  let empty = Grew_grs.Grs.empty

  let load file =
    handle ~name:"Grs.load" ~file
      (fun () ->
        if not (Sys.file_exists file)
        then raise (File_not_found file)
        else Grew_grs.Grs.build file
      ) ()

  let get_sequence_names grs =
    handle ~name:"Grs.get_sequence_names"
      (fun () ->
        Grew_grs.Grs.sequence_names grs
      ) ()

  let build_html_doc ?(corpus=false) dir grs =
    handle ~name:"Grs.build_doc [with Dep2pict]"
      (fun () ->
        Grew_html.Html_doc.build ~corpus ~dep:true dir grs;

        (* draw pattern graphs for all rules and all filters *)
        let fct module_ rule_ =
          let dep_code = Grew_rule.Rule.to_dep ?domain:(Grew_grs.Grs.get_domain grs) rule_ in
          let dep_png_file = sprintf "%s/%s_%s-patt.png" dir module_ (Grew_rule.Rule.get_name rule_) in
          let d2p = Dep2pict.Dep2pict.from_dep ~dep:dep_code in
          Dep2pict.Dep2pict.save_png ~filename:dep_png_file d2p in
        Grew_grs.Grs.rule_iter fct grs;
        Grew_grs.Grs.filter_iter fct grs
      ) ()

  let get_domain grs = Grew_grs.Grs.get_domain grs
end

(* ==================================================================================================== *)
(** {2 Rewrite} *)
(* ==================================================================================================== *)
module Rewrite = struct
  type display = Libgrew_types.rew_display
  type history = Grew_grs.Rewrite_history.t

  let set_max_depth_det value = Grew_rule.Rule.set_max_depth_det value
  let set_max_depth_non_det value = Grew_rule.Rule.set_max_depth_non_det value

  let set_debug_loop () = Grew_rule.Rule.set_debug_loop ()

  let display ~gr ~grs ~seq =
    handle ~name:"Rewrite.display" (fun () -> Grew_grs.Grs.build_rew_display grs seq gr) ()

  let set_timeout t = Grew_base.Timeout.timeout := t

  let rewrite ~gr ~grs ~seq =
    handle ~name:"Rewrite.rewrite" (fun () -> Grew_grs.Grs.rewrite grs seq gr) ()

  let get_graphs rh =
    handle ~name:"Rewrite.get_graphs" (fun () -> Grew_grs.Rewrite_history.get_graphs rh) ()

  let is_empty rh =
    handle ~name:"Rewrite.is_empty" (fun () -> Grew_grs.Rewrite_history.is_empty rh) ()

  let num_sol rh =
    handle ~name:"Rewrite.num_sol" (fun () -> Grew_grs.Rewrite_history.num_sol rh) ()

  let write_stat filename rew_hist =
    handle ~name:"Rewrite.write_stat" (fun () -> Grew_html.Gr_stat.save filename (Grew_html.Gr_stat.from_rew_history rew_hist)) ()

  let write_annot ?domain ~title static_dir annot_dir base_name_rew_hist_list =
    handle ~name:"Rewrite.write_annot" (fun () -> Grew_html.Html_annot.build ?domain ~title static_dir annot_dir base_name_rew_hist_list) ()

  let save_index ~dirname ~base_names =
    handle ~name:"Rewrite.save_index" (fun () ->
      let out_ch = open_out (Filename.concat dirname "index") in
      Array.iter (fun f -> fprintf out_ch "%s\n" f) base_names;
      close_out out_ch
    ) ()

  let save_gr ?domain base rew_hist =
    handle ~name:"Rewrite.save_gr" (fun () -> Grew_grs.Rewrite_history.save_gr ?domain base rew_hist) ()

  let save_conll ?domain base rew_hist =
    handle ~name:"Rewrite.save_conll" (fun () -> Grew_grs.Rewrite_history.save_conll ?domain base rew_hist) ()

  let save_full_conll ?domain base rew_hist =
    handle ~name:"Rewrite.save_full_conll" (fun () -> Grew_grs.Rewrite_history.save_full_conll ?domain base rew_hist) ()

  let save_det_gr ?domain base rew_hist =
    handle ~name:"Rewrite.save_det_gr" (fun () -> Grew_grs.Rewrite_history.save_det_gr ?domain base rew_hist) ()

  let save_det_conll ?domain ?header base rew_hist =
    handle ~name:"Rewrite.save_det_conll" (fun () -> Grew_grs.Rewrite_history.save_det_conll ?domain ?header base rew_hist) ()

  let det_dep_string ?domain rew_hist =
    handle ~name:"Rewrite.det_dep_string" (fun () -> Grew_grs.Rewrite_history.det_dep_string ?domain rew_hist) ()

  let conll_dep_string ?domain ?keep_empty_rh rew_hist =
    handle ~name:"Rewrite.conll_dep_string" (fun () -> Grew_grs.Rewrite_history.conll_dep_string ?domain ?keep_empty_rh rew_hist) ()

  let write_html ?domain ?(no_init=false) ?(out_gr=false) ?filter ?main_feat ?dot ~header ?graph_file rew_hist output_base =
    handle ~name:"Rewrite.write_html" (fun () ->
      ignore (
        Grew_html.Html_rh.build ?domain ?filter ?main_feat ?dot ~out_gr ~init_graph: (not no_init) ~header ?graph_file output_base rew_hist
      )
    ) ()

  let error_html ?domain ?(no_init=false) ?main_feat ?dot ~header msg ?init output_base =
    handle ~name:"Rewrite.error_html" (fun () ->
      ignore (
        Grew_html.Html_rh.error ?domain ?main_feat ?dot ~init_graph: (not no_init) ~header output_base msg init
      )
    ) ()

  let make_index ~title ~grs_file ~html ~grs ~seq ~input_dir ~output_dir ~base_names  =
    handle ~name:"Rewrite.make_index" (fun () ->
      let init = Grew_html.Corpus_stat.empty grs seq in
      let corpus_stat =
        Array.fold_left
          (fun acc base_name ->
            Grew_html.Corpus_stat.add_gr_stat base_name (Grew_html.Gr_stat.load (Filename.concat output_dir (base_name^".stat"))) acc
          ) init base_names in
      Grew_html.Corpus_stat.save_html title grs_file input_dir output_dir corpus_stat
    ) ()

  let html_sentences ~title = handle ~name:"Rewrite.html_sentences" (fun () -> Grew_html.Html_sentences.build ~title) ()
end
