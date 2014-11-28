(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

include Libgrew_types

open Printf
open Log

open Grew_fs
open Grew_base
open Grew_types

open Grew_graph
open Grew_rule
open Grew_grs

open Grew_parser
open Grew_html


let css_file = Filename.concat DATA_DIR "style.css"

let empty_grs = Grs.empty

let set_timeout t = Timeout.timeout := t


exception File_dont_exists of string

exception Parsing_err of string
exception Build of string * (string * int) option
exception Run of string * (string * int) option
exception Bug of string * (string * int) option

let handle ?(name="") ?(file="No file defined") fct () =
  try fct () with
    (* Raise again already catched exceptions *)
    | Parsing_err msg -> raise (Parsing_err msg)
    | Build (msg,loc) -> raise (Build (msg,loc))
    | Bug (msg, loc) -> raise (Bug (msg,loc))
    | Run (msg, loc) -> raise (Run (msg,loc))

    (* Catch new exceptions *)
    | Grew_parser.Parse_error (msg,Some (sub_file,l)) ->
        raise (Parsing_err (sprintf "[file:%s, line:%d] %s" sub_file l msg))
    | Grew_parser.Parse_error (msg,None) ->
        raise (Parsing_err (sprintf "[file:%s] %s" file msg))
    | Error.Build (msg,loc) -> raise (Build (msg,loc))
    | Error.Bug (msg, loc) -> raise (Bug (msg,loc))
    | Error.Run (msg, loc) -> raise (Run (msg,loc))

    | exc -> raise (Bug (sprintf "[Libgrew.%s] UNCATCHED EXCEPTION: %s" name (Printexc.to_string exc), None))

let is_empty rh =
  handle ~name:"is_empty" (fun () -> Rewrite_history.is_empty rh) ()

let num_sol rh =
  handle ~name:"num_sol" (fun () -> Rewrite_history.num_sol rh) ()


IFDEF DEP2PICT THEN
let build_html_doc ?(corpus=false) dir grs =
  handle ~name:"build_doc [with Dep2pict]"
    (fun () ->
      Html_doc.build ~corpus ~dep:true dir grs;

      (* draw pattern graphs for all rules and all filters *)
      let fct module_ rule_ =
        let dep_code = Rule.to_dep rule_ in
        let dep_png_file = sprintf "%s/%s_%s-patt.png" dir module_ (Rule.get_name rule_) in
        ignore (Dep2pict.Dep2pict.fromDepStringToPng dep_code dep_png_file) in
      Grs.rule_iter fct grs;
      Grs.filter_iter fct grs
    ) ()
ELSE
let build_html_doc ?(corpus=false) dir grs =
  handle ~name:"build_doc [without Dep2pict]" (fun () -> Html_doc.build ~corpus ~dep:false grs_ dir) ()
END

let load_grs file =
  handle ~name:"load_grs" ~file
    (fun () ->
      if not (Sys.file_exists file)
      then raise (File_dont_exists file)
      else Grs.build file
    ) ()

let to_sentence ?main_feat gr =
  handle ~name:"to_sentence"
    (fun () ->
      let graph = gr.Instance.graph in
      G_graph.to_sentence ?main_feat graph
    ) ()

let get_sequence_names grs =
  handle ~name:"get_sequence_names"
    (fun () ->
      Grs.sequence_names grs
    ) ()

let load_gr file =
  if not (Sys.file_exists file)
  then raise (File_dont_exists file)
  else
    handle ~name:"load_gr" ~file
      (fun () ->
        let gr_ast = Grew_parser.gr_of_file file in
        Instance.from_graph (G_graph.build gr_ast)
      ) ()

let load_conll file =
  handle ~name:"load_conll" ~file
    (fun () ->
      let graph = G_graph.of_conll ~loc:(file,-1) (Conll.load file) in
      Instance.from_graph graph
    ) ()

let of_conll file_name line_list =
  handle ~name:"of_conll"
    (fun () ->
      let graph = G_graph.of_conll (Conll.parse file_name line_list) in
      Instance.from_graph graph
    ) ()

let load_graph file =
  handle ~name:"load_graph" ~file
    (fun () ->
      if Filename.check_suffix file ".gr"
      then load_gr file
      else if Filename.check_suffix file ".conll"
      then load_conll file
      else
        begin
          Log.fwarning "Unknown file format for input graph '%s', try to guess..." file;
          try load_gr file with
              Parsing_err _ ->
                try load_conll file with
                    Parsing_err _ ->
                      Log.fcritical "[Libgrew.load_graph] Cannot guess input file format of file '%s'. Use .gr or .conll file extension" file
        end
    ) ()

let xml_graph xml =
  handle ~name:"xml_graph" (fun () -> Instance.from_graph (G_graph.of_xml xml)) ()

let raw_graph instance =
  handle ~name:"raw_graph" (fun () -> G_graph.to_raw instance.Instance.graph) ()

let rewrite ~gr ~grs ~seq =
  handle ~name:"rewrite" (fun () -> Grs.rewrite grs seq gr) ()

let display ~gr ~grs ~seq =
  handle ~name:"display" (fun () -> Grs.build_rew_display grs seq gr) ()

let write_stat filename rew_hist =
  handle ~name:"write_stat" (fun () -> Gr_stat.save filename (Gr_stat.from_rew_history rew_hist)) ()

let write_annot ~title static_dir annot_dir base_name_rew_hist_list =
  handle ~name:"write_annot" (fun () -> Html_annot.build ~title static_dir annot_dir base_name_rew_hist_list) ()

let save_index ~dirname ~base_names =
  handle ~name:"save_index" (fun () ->
    let out_ch = open_out (Filename.concat dirname "index") in
    List.iter (fun f -> fprintf out_ch "%s\n" f) base_names;
    close_out out_ch
  ) ()

let save_graph_conll filename graph =
  handle ~name:"save_graph_conll" (fun () ->
    let out_ch = open_out filename in
    fprintf out_ch "%s" (Instance.to_conll graph);
    close_out out_ch
  ) ()

let save_gr base rew_hist =
  handle ~name:"save_gr" (fun () -> Rewrite_history.save_gr base rew_hist) ()

let save_conll base rew_hist =
  handle ~name:"save_conll" (fun () -> Rewrite_history.save_conll base rew_hist) ()

let save_det_gr base rew_hist =
  handle ~name:"save_det_gr" (fun () -> Rewrite_history.save_det_gr base rew_hist) ()

let save_det_conll ?header base rew_hist =
  handle ~name:"save_deeeet_conll" (fun () -> Rewrite_history.save_det_conll ?header base rew_hist) ()

let det_dep_string rew_hist =
  handle ~name:"det_dep_string" (fun () -> Rewrite_history.det_dep_string rew_hist) ()

let conll_dep_string ?keep_empty_rh rew_hist =
  handle ~name:"conll_dep_string" (fun () -> Rewrite_history.conll_dep_string ?keep_empty_rh rew_hist) ()

let write_html
    ?(no_init=false)
    ?(out_gr=false)
    ?filter
    ?main_feat
    ?dot
    ~header
    ?graph_file
    rew_hist
    output_base =
  handle ~name:"write_html" (fun () ->
    ignore (
      Html_rh.build
        ?filter
        ?main_feat
        ?dot
        ~out_gr
        ~init_graph: (not no_init)
        ~header
        ?graph_file
        output_base rew_hist
    )
  ) ()

let error_html
    ?(no_init=false)
    ?main_feat
    ?dot
    ~header
    msg
    ?init
    output_base =
  handle ~name:"error_html" (fun () ->
    ignore (
      Html_rh.error
        ?main_feat
        ?dot
        ~init_graph: (not no_init)
        ~header
        output_base msg init
    )
  ) ()

let make_index ~title ~grs_file ~html ~grs ~seq ~input_dir ~output_dir ~base_names  =
  handle ~name:"make_index" (fun () ->
    let init = Corpus_stat.empty grs seq in
    let corpus_stat =
      List.fold_left
        (fun acc base_name ->
          Corpus_stat.add_gr_stat base_name (Gr_stat.load (Filename.concat output_dir (base_name^".stat"))) acc
        ) init base_names in
    Corpus_stat.save_html title grs_file input_dir output_dir corpus_stat
  ) ()

let html_sentences ~title = handle ~name:"html_sentences" (fun () -> Html_sentences.build ~title) ()

let graph_of_instance instance = handle ~name:"graph_of_instance" (fun () -> instance.Instance.graph) ()

let feature_names () =  handle ~name:"feature_names" (fun () -> Domain.feature_names ()) ()

let to_dot_graph ?main_feat ?(deco=G_deco.empty) graph =
  handle ~name:"to_dot_graph" (fun () -> G_graph.to_dot ?main_feat graph ~deco) ()

let to_dep_graph ?filter ?main_feat ?(deco=G_deco.empty) graph =
  handle ~name:"to_dep_graph" (fun () -> G_graph.to_dep ?filter ?main_feat ~deco graph) ()

let to_gr_graph graph =
  handle ~name:"to_gr_graph" (fun () -> G_graph.to_gr graph) ()

let to_conll_graph graph =
  handle ~name:"to_conll_graph" (fun () -> G_graph.to_conll graph) ()

type pattern = Rule.pattern
type matching = Rule.matching

let load_pattern file =
  handle ~name:"load_pattern" (fun () -> Rule.build_pattern (Grew_parser.load_pattern file)) ()

let match_in_graph pattern graph = Rule.match_in_graph pattern graph

let match_deco pattern matching = Rule.match_deco pattern matching