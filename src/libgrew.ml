include Grew_types

open Printf
open Log
open Dep2pict

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

let empty_grs = Grs.empty

let set_timeout t = Timeout.timeout := t

let load_grs ?doc_output_dir file =
  if not (Sys.file_exists file)
  then raise (File_dont_exists file)
  else
    try
      let grs_ast = Grew_parser.grs_of_file file in
      let grs = Grs.build grs_ast in
      (match doc_output_dir with
      | None -> ()
      | Some dir -> 
          Html.proceed dir grs_ast;
          Grs.rule_iter
            (fun modul_name rule ->
              let dep_code = Rule.to_dep rule in
              let dep_svg_file = sprintf "%s/%s_%s-patt.png" dir modul_name (Rule.get_name rule) in
              ignore (Dep2pict.fromDepStringToPng dep_code dep_svg_file)            
            ) grs
      );
      grs
    with
    | Grew_parser.Parse_error (msg,Some (sub_file,l)) -> 
        raise (Parsing_err (sprintf "[file:%s, line:%d] %s" sub_file l msg))
    | Grew_parser.Parse_error (msg,None) -> 
        raise (Parsing_err (sprintf "[file:%s] %s" file msg))
    | Error.Build (msg,loc) -> raise (Build (msg,loc))
    | Error.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (sprintf "[Libgrew.load_grs] UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))


let get_sequence_names grs = Grs.sequence_names grs

let empty_gr = Instance.empty

let load_gr file =
  if (Sys.file_exists file) then (
    try
      let gr_ast = Grew_parser.gr_of_file file in
      Instance.build gr_ast
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
    (* let lines = File.read file in *)
    (* Instance.of_conll (List.map Conll.parse lines) *)
    Instance.of_conll ~loc:(file,-1) (Conll.load file)
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

let write_html 
    ?(no_init=false) ?main_feat 
    ~header
    rew_hist
    output_base =
IFDEF DEP2PICT THEN
  ignore (
  Rewrite_history.save_html 
    ?main_feat 
    ~init_graph: (not no_init)
    ~header
    output_base rew_hist
    )
ELSE
    Log.critical "[write_html] The \"libcaml-grew\" library is compiled without Dep2pict"
ENDIF

let error_html 
    ?(no_init=false) ?main_feat 
    ~header
    msg ?init
    output_base =
IFDEF DEP2PICT THEN
  ignore (
  Rewrite_history.error_html 
    ?main_feat 
    ~init_graph: (not no_init)
    ~header
    output_base msg init
    )
ELSE
    Log.critical "[error_html] The \"libcaml-grew\" library is compiled without Dep2pict"
ENDIF

let make_index ~title ~grs_file ~html ~grs ~seq ~output_dir ~base_names  =
  let init = Corpus_stat.empty grs seq in
  let corpus_stat =
    List.fold_left
      (fun acc base_name -> 
        Corpus_stat.add_gr_stat base_name (Gr_stat.load (Filename.concat output_dir (base_name^".stat"))) acc
      ) init base_names in
  Corpus_stat.save_html title grs_file html output_dir corpus_stat

let get_css_file = Filename.concat DATA_DIR "style.css"

let graph_of_instance instance = instance.Instance.graph
