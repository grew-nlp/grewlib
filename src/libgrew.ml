include Grew_types

open Printf
open Log

open Grew_utils
open Grew_graph
open Grew_rule
open Grew_grs

open Grew_parser
open Grew_html



exception File_dont_exists of string

exception Parsing_err of string
exception Build of string * (string * int) option
exception Run of string * (string * int) option
exception Bug of string * (string * int) option

type grs = Grs.t
type gr = Instance.t
type rew_history = Rewrite_history.t

let is_empty = Rewrite_history.is_empty

let num_sol = Rewrite_history.num_sol

let empty_grs = Grs.empty

let set_timeout t = Timeout.timeout := t

IFDEF DEP2PICT THEN
let build_doc file dir grs_ast grs =
  Html.proceed ~dep:true file dir grs_ast;
  
  (* draw pattern graphs for all rules and all filters *)
  let fct module_ rule_ = 
    let dep_code = Rule.to_dep rule_ in
    let dep_svg_file = sprintf "%s/%s_%s-patt.png" dir module_ (Rule.get_name rule_) in
    ignore (Dep2pict.Dep2pict.fromDepStringToPng dep_code dep_svg_file) in
  Grs.rule_iter fct grs;
  Grs.filter_iter fct grs
ELSE
let build_doc file dir grs_ast grs =
  Html.proceed ~dep:false file dir grs_ast
END

let load_grs ?doc_output_dir file =
  if not (Sys.file_exists file)
  then raise (File_dont_exists file)
  else
    try
      let grs_ast = Grew_parser.grs_of_file file in
      let grs = Grs.build grs_ast in
      (match doc_output_dir with
        | None -> ()
        | Some dir -> build_doc file dir grs_ast grs);
      grs
    with
    | Grew_parser.Parse_error (msg,Some (sub_file,l)) -> 
        raise (Parsing_err (sprintf "[file:%s, line:%d] %s" sub_file l msg))
    | Grew_parser.Parse_error (msg,None) -> 
        raise (Parsing_err (sprintf "[file:%s] %s" file msg))
    | Error.Build (msg,loc) -> raise (Build (msg,loc))
    | Error.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (sprintf "[Libgrew.load_grs] UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))

let to_sentence ?main_feat gr =
  let graph = gr.Instance.graph in
  G_graph.to_sentence ?main_feat graph

let get_sequence_names grs = Grs.sequence_names grs

let load_gr file =
  if (Sys.file_exists file) then (
    try
      let gr_ast = Grew_parser.gr_of_file file in
      Instance.from_graph (G_graph.build gr_ast)
    with
    | Grew_parser.Parse_error (msg,Some (sub_file,l)) -> 
        raise (Parsing_err (sprintf "[file:%s, line:%d] %s" sub_file l msg))
    | Grew_parser.Parse_error (msg,None) -> 
        raise (Parsing_err (sprintf "[file:%s] %s" file msg))
    | Error.Build (msg,loc) -> raise (Build (msg,loc))
    | Error.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (sprintf "[Libgrew.load_gr] UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))

   ) else (
    raise (File_dont_exists file)
   )

let load_conll file =
  try
    let graph = G_graph.of_conll ~loc:(file,-1) (Conll.load file) in
    Instance.from_graph graph
  with
    | Grew_parser.Parse_error (msg,Some (sub_file,l)) -> 
        raise (Parsing_err (sprintf "[file:%s, line:%d] %s" sub_file l msg))
    | Grew_parser.Parse_error (msg,None) -> 
        raise (Parsing_err (sprintf "[file:%s] %s" file msg))
  | Error.Build (msg,loc) -> raise (Build (msg,loc))
  | Error.Bug (msg, loc) -> raise (Bug (msg,loc))
  | exc -> raise (Bug (sprintf "[Libgrew.load_conll] UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))

let load_graph file = 
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

let rewrite ~gr ~grs ~seq = 
  try Grs.rewrite grs seq gr
  with
  | Error.Run (msg,loc) -> raise (Run (msg,loc))
  | Error.Bug (msg, loc) -> raise (Bug (msg,loc))
  | exc -> raise (Bug (sprintf "[Libgrew.rewrite] UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))

let display ~gr ~grs ~seq =
  try Grs.build_rew_display grs seq gr
  with
  | Error.Run (msg,loc) -> raise (Run (msg,loc))
  | Error.Bug (msg, loc) -> raise (Bug (msg,loc))
  | Error.Build (msg, loc) -> raise (Build (msg,loc))
  | exc -> raise (Bug (sprintf "[Libgrew.display] UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))

let write_stat filename rew_hist = Gr_stat.save filename (Gr_stat.from_rew_history rew_hist) 

let save_index ~dirname ~base_names =
  let out_ch = open_out (Filename.concat dirname "index") in
  List.iter (fun f -> fprintf out_ch "%s\n" f) base_names;
  close_out out_ch

let save_gr base rew_hist = 
  Rewrite_history.save_gr base rew_hist

let write_html 
    ?(no_init=false)
    ?(out_gr=false)
    ?main_feat 
    ?dot
    ~header
    ~graph_file
    rew_hist
    output_base =
  ignore (
  Rewrite_history.save_html 
    ?main_feat
    ?dot
    ~out_gr
    ~init_graph: (not no_init)
    ~header
    ~graph_file
    output_base rew_hist
    )

let error_html 
    ?(no_init=false) 
    ?main_feat 
    ?dot
    ~header
    msg 
    ?init
    output_base =
  ignore (
  Rewrite_history.error_html 
    ?main_feat
    ?dot
    ~init_graph: (not no_init)
    ~header
    output_base msg init
    )

let make_index ~title ~grs_file ~html ~grs ~seq ~input_dir ~output_dir ~base_names  =
  let init = Corpus_stat.empty grs seq in
  let corpus_stat =
    List.fold_left
      (fun acc base_name -> 
        Corpus_stat.add_gr_stat base_name (Gr_stat.load (Filename.concat output_dir (base_name^".stat"))) acc
      ) init base_names in
  Corpus_stat.save_html title grs_file input_dir output_dir corpus_stat

let html_sentences = Html.html_sentences

let get_css_file = Filename.concat DATA_DIR "style.css"

let graph_of_instance instance = instance.Instance.graph
