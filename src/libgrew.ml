include Grew_types

open Printf
open Log

open Utils
open Grew_parser
open Checker
open Grs
open Graph
open Rule
open HTMLer



exception Parsing_err of string
exception File_dont_exists of string

exception Build of string * (string * int) option
exception Run of string * (string * int) option
exception Bug of string * (string * int) option

type grs = Grs.t
type gr = Instance.t
type rew_history = Rewrite_history.t

let empty_grs = Grs.empty

let load_grs ?doc_output_dir file =
  if not (Sys.file_exists file)
  then raise (File_dont_exists file)
  else
    try
      let ast = Grew_parser.parse_file_to_grs file in
      (* Checker.check_grs ast; *)
      (match doc_output_dir with
      | None -> ()
      | Some dir -> HTMLer.proceed dir ast);
      Grs.build ast
    with
    | Grew_parser.Parse_error msg -> raise (Parsing_err msg)
    | Utils.Build (msg,loc) -> raise (Build (msg,loc))
    | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))


let get_available_seq grs = Grs.sequences grs

let empty_gr = Instance.empty

let load_gr file =
  if (Sys.file_exists file) then (
    try
      let ast = Grew_parser.parse_file_to_gr file in
      (* Checker.check_gr ast;*)
      Instance.build ast
    with
    | Grew_parser.Parse_error msg -> raise (Parsing_err msg)
    | Utils.Build (msg,loc) -> raise (Build (msg,loc))
    | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))

   ) else (
    raise (File_dont_exists file)
   )

let rewrite ~gr ~grs ~seq = Grs.rewrite grs seq gr

let display ~gr ~grs ~seq =
  try
    Grs.build_rew_display grs seq gr
  with
  | Utils.Run (msg,loc) -> raise (Run (msg,loc))
  | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
  | exc -> raise (Bug (sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))


let write_stat filename rew_hist = Gr_stat.save filename (Gr_stat.from_rew_history rew_hist) 


(* let rules_stat grs seq gr_file = *)
(*   try *)
(*     let gr = Instance.build (Grew_parser.parse_file_to_gr gr_file) in *)
(*     let rew_hist = Grs.rewrite grs seq gr in *)
(*     StringMap.fold *)
(*       (fun key value acc -> *)
(*         (key,value)::acc *)
(*       ) *)
(*       (Gr_stat.from_rew_history rew_hist)  *)
(*       [] *)
(*   with *)
(*   | Utils.Run (msg, Some (loc_file,loc_line)) -> *)
(*       Log.fmessage "[file: %s, line: %d] Utils.run: %s\n" loc_file loc_line msg; [] *)
(*   | exc -> *)
(*       Log.fmessage "Unexpected exception: %s\n" (Printexc.to_string exc); [] *)


let write_html 
    ?(no_init=false) ?main_feat 
    ~header
    rew_hist
    output_base =
ignore (
  Rewrite_history.save_html 
    ?main_feat 
    ~init_graph: (not no_init)
    ~header
    output_base rew_hist
    )
        IFDEF DEP2PICT THEN
let dummy = ()


(* let rewrite_to_html_intern  *)
(*     ?(no_init=false)  *)
(*     ?main_feat  *)
(*     grs_file  *)
(*     grs seq  *)
(*     gr_file *)
(*     output_base *)
(*     nb_sentence  *)
(*     previous  *)
(*     next = *)

(*   let header = "" in *)
  
(*   let _ =  *)
(*     Sys.command (sprintf "cp %s %s"  *)
(*                    gr_file  *)
(*                    (Filename.concat (Filename.dirname output_base) (Filename.basename gr_file)) *)
(*                 ) in *)
  
(*   try *)
(*     let init = Instance.build (Grew_parser.parse_file_to_gr gr_file) in *)
(*     try *)
(*       let rew_hist = Grs.rewrite grs seq init in *)
      
(*       ignore ( (\* FIXME: ingore inutile *\) *)
(*       Rewrite_history.save_html  *)
(*         ?main_feat  *)
(*         ~init_graph: (not no_init) *)
(*         ~header *)
(*         output_base rew_hist *)
(*      ) *)
        
(*     with *)
(*     | Utils.Run (msg, Some (loc_file,loc_line)) -> *)
(*         let html_ch = open_out (sprintf "%s.html" output_base) in *)
(*         Html.enter html_ch ~header output_base; *)
(*         fprintf html_ch "<h6>Initial graph</h6>\n"; *)
(*         Instance.save_dep_png ?main_feat output_base init; *)
(*         fprintf html_ch "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>\n" (Filename.basename output_base); *)
(*         fprintf html_ch "<h2>ERROR during rewriting:</h2>\n"; *)
(*         fprintf html_ch "<p>Message: %s</p>\n" msg; *)
(*         fprintf html_ch "<p>File: %s</p>\n" loc_file; *)
(*         fprintf html_ch "<p>Line: %d</p>\n" loc_line; *)
(*         Html.leave html_ch; *)
(*         close_out html_ch with *)
(*   | exc -> *)
(*       let html_ch = open_out (sprintf "%s.html" output_base) in *)
(*       Html.enter html_ch ~header output_base; *)
(*       fprintf html_ch "<h1>UNEXPECTED EXCEPTION: %s</h1>" (Printexc.to_string exc); *)
(*       Html.leave html_ch; *)
(*       close_out html_ch *)
        


(* let rewrite_to_html ?main_feat input_dir output_dir no_init grs_file grs seq title = *)
(*   try *)
(*     (\* get ALL gr files *\) *)
(*     let all_files = Array.to_list (Sys.readdir input_dir) in *)
(*     let gr_files = List.sort (fun a b -> compare a b)  *)
(*         (List.filter (fun file -> Filename.check_suffix file ".gr") all_files) in *)
(*     let nb_files = List.length gr_files in *)
(*     let ratio nb = (float nb) /. (float nb_files) *. 100. in *)

(*     (\* create html files *\) *)
(*     ignore (Sys.command (sprintf "cp %s %s" grs_file  *)
(*                            (Filename.concat output_dir (Filename.basename grs_file)))); *)

(*     let sentence_counter = ref 1 in *)


(*     List.iter *)
(*       (fun input -> *)
(*         Log.fmessage "Computing %s" input; *)
(*         let rules = rewrite_to_html_intern *)
(*             ~no_init *)
(*             grs_file *)
(*             grs *)
(*             seq *)
(*             (Filename.concat input_dir input) *)
(*             (Filename.concat output_dir (Filename.chop_extension input)) *)
(*             ?main_feat *)
(*             !sentence_counter *)
(*             (if !sentence_counter > 1 then (Filename.chop_extension (List.nth gr_files (!sentence_counter-2))) else "") *)
(*             (if !sentence_counter < nb_files then (Filename.chop_extension (List.nth gr_files (!sentence_counter)))  else "") *)
(*         in *)
(*         incr sentence_counter; *)
(*         (\* match rules with *\) *)
(*         (\* | Some module_list -> *\) *)
(*         (\*     List.iter *\) *)
(*         (\*       (fun (module_name, rule_list) -> *\) *)
(*         (\*         List.iter *\) *)
(*         (\*           (fun rule -> *\) *)
(*         (\*             stats := Corpus_stat.add module_name rule input 1 !stats *\) *)
(*         (\*           ) rule_list *\) *)
(*         (\*         (\\* let old_rule_list = *\\) *\) *)
(*         (\*         (\\*   try ref (StringMap.find module_name !stats) *\\) *\) *)
(*         (\*         (\\*   with Not_found -> ref StringMap.empty in *\\) *\) *)
(*         (\*         (\\* List.iter *\\) *\) *)
(*         (\*         (\\*   (fun rule -> *\\) *\) *)
(*         (\*         (\\*     let old = try StringMap.find rule !old_rule_list with Not_found -> [] in *\\) *\) *)
(*         (\*         (\\*     old_rule_list := StringMap.add rule (input::old) !old_rule_list *\\) *\) *)
(*         (\*         (\\*   ) rule_list; *\\) *\) *)
(*         (\*         (\\* stats := StringMap.add module_name !old_rule_list !stats *\\) *\) *)
(*         (\*       ) module_list *\) *)
(*         (\* | None -> errors := input :: !errors *\) *)
(*       ) gr_files; *)



(*     (\* let out_ch = open_out (Filename.concat output_dir "index.html") in *\) *)

(*     (\* let css = "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">" in *\) *)

(*     (\* ignore(Sys.command("cp "^(Filename.concat DATA_DIR "style.css")^" "^(Filename.concat output_dir "style.css"))); *\) *)

(*     (\* fprintf out_ch "<head>\n%s\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" /></head>\n" css title; *\) *)
(*     (\* fprintf out_ch "<h1>%s</h1>\n" title; *\) *)
(*     (\* fprintf out_ch "<b>Grs file</b>:%s\n<br/>\n" (Filename.basename grs_file); *\) *)
(*     (\* fprintf out_ch "<b>%d Sentences</b><br/>\n<br/>\n" nb_files; *\) *)
(*     (\* fprintf out_ch "<center><table cellpadding=10 cellspacing=0 width=90%%>\n"; *\) *)
(*     (\* StringMap.iter *\) *)
(*     (\*   (fun modul rules -> *\) *)
(*     (\*     fprintf out_ch "<tr><td colspan=5><h6>Module %s</h6></td>\n" modul; *\) *)
(*     (\*     fprintf out_ch "<tr><th class=\"first\" width=10>Rule</th><th width=10>#occ</th><th width=10>#files</th><th width=10>Ratio</th><th width=10>Files</th></tr>\n"; *\) *)
(*     (\*     StringMap.iter *\) *)
(*     (\*       (fun rule (occ_num, file_set) -> *\) *)
(*     (\*         let file_list = StringSet.elements file_set in *\) *)

(*     (\*         let tmp = ref "" in *\) *)
(*     (\*         let counter = ref 0 in *\) *)
(*     (\*         let rec compute list = match list with *\) *)
(*     (\*         | [] -> () *\) *)
(*     (\*         | h::[] -> *\) *)
(*     (\*             if (!counter = 10) then ( *\) *)
(*     (\*               tmp := sprintf "%s<div id=\"%s_%s\" style=\"display:none;\">\n" !tmp modul rule *\) *)
(*     (\*              ); *\) *)
(*     (\*             incr counter; *\) *)
(*     (\*             tmp := sprintf "%s<a href=\"%s\">%s</a>" !tmp ((Filename.chop_extension h)^".html") (Filename.chop_extension h) *\) *)
(*     (\*         | h::t -> *\) *)
(*     (\*             if (not (List.mem h t)) then ( (\\*avoid doublons*\\) *\) *)
(*     (\*               if (!counter = 10) then ( *\) *)
(*     (\*                 tmp := sprintf "%s<div id=\"%s_%s\" style=\"display:none;\">\n" !tmp modul rule *\) *)
(*     (\*                ); *\) *)
(*     (\*               incr counter; *\) *)
(*     (\*               tmp := sprintf "%s<a href=\"%s\">%s</a><br/>" !tmp ((Filename.chop_extension h)^".html") (Filename.chop_extension h) *\) *)
(*     (\*              ); *\) *)
(*     (\*             compute t *\) *)
(*     (\*         in compute (List.rev file_list); *\) *)

(*     (\*         let file_num = List.length file_list in *\) *)

(*     (\*         fprintf out_ch "<tr>\n"; *\) *)
(*     (\*         fprintf out_ch "<td class=\"first_stats\" width=10 valign=top>%s</td>\n" rule; *\) *)
(*     (\*         fprintf out_ch "<td class=\"stats\" width=10 valign=top>%d</td>\n" occ_num; *\) *)
(*     (\*         fprintf out_ch "<td class=\"stats\" width=10 valign=top>%d</td>\n" file_num; *\) *)
(*     (\*         fprintf out_ch "<td class=\"stats\" width=10 valign=top>%.2f%%</td>\n" (ratio file_num); *\) *)
            
(*     (\*         fprintf out_ch "<td class=\"stats\">%s" !tmp; *\) *)
(*     (\*         if (!counter > 10) *\) *)
(*     (\*         then ( *\) *)
(*     (\*           fprintf out_ch "</div><a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s_%s').style.display == 'none') { %s } else { %s }\"><b><p id=\"p_%s_%s\">+ Show more +</p></b></a>\n" *\) *)
(*     (\*             modul rule *\) *)
(*     (\*             (sprintf "document.getElementById('%s_%s').style.display = 'block'; document.getElementById('p_%s_%s').innerHTML = '- Show less -';" modul rule modul rule) *\) *)
(*     (\*             (sprintf "document.getElementById('%s_%s').style.display = 'none';; document.getElementById('p_%s_%s').innerHTML = '+ Show more +';" modul rule modul rule) *\) *)
(*     (\*             modul rule; *\) *)
(*     (\*          ); *\) *)
(*     (\*         fprintf out_ch "</td></tr>\n"; *\) *)
(*     (\*       ) rules; *\) *)
(*     (\*   ) !stats; *\) *)

(*     (\* (\\* add a subtalbe for sentence that produces an error *\\) *\) *)
(*     (\* let nb_errors = List.length !errors in *\) *)
(*     (\* fprintf out_ch "<tr><td colspan=5><h6>ERRORS</h6></td>\n"; *\) *)
(*     (\* fprintf out_ch "<tr><th class=\"first\" width=10>Rule</th><th colspan=2 width=20>#files</th><th width=10>Ratio</th><th>Files</th></tr>\n"; *\) *)

(*     (\* fprintf out_ch "<tr>\n"; *\) *)
(*     (\* fprintf out_ch "<td class=\"first_stats\">Errors</td>\n"; *\) *)
(*     (\* fprintf out_ch "<td class=\"stats\" colspan=2>%d</td>\n" nb_errors; *\) *)
(*     (\* fprintf out_ch "<td class=\"stats\">%.2f%%</td>\n" (ratio nb_errors); *\) *)
(*     (\* fprintf out_ch "<td class=\"stats\">"; *\) *)
(*     (\* List.iter *\) *)
(*     (\*   (fun err -> *\) *)
(*     (\*     fprintf out_ch "<a href=\"%s.html\">%s</a><br/>" (Filename.chop_extension err) (Filename.chop_extension err) *\) *)
(*     (\*   ) (List.rev !errors); *\) *)
(*     (\* fprintf out_ch "</td>\n"; *\) *)
(*     (\* fprintf out_ch "</tr>"; *\) *)

(*     (\* fprintf out_ch "</table></center>\n"; *\) *)

(*     (\* close_out out_ch; *\) *)
(*     (\* () *\) *)

(*   with *)
(*   | Utils.Run (msg,loc) -> raise (Run (msg,loc)) *)
(*   | Utils.Bug (msg, loc) -> raise (Bug (msg,loc)) *)
(*   | exc -> raise (Bug (sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None)) *)
        ENDIF

(* (\* read all stats in [dir] and produce an html file with full stats *\) *)
(* let collect_stats init dir = *)
(*   let all_files = Array.to_list (Sys.readdir dir) in *)
(*   let stat_files = List.filter (fun f -> Filename.check_suffix f ".stat") all_files in *)
(*   List.fold_left *)
(*     (fun acc stat_file ->  *)
(*       List.fold_left  *)
(*         (fun acc2 line -> *)
(*           match Str.split (Str.regexp "\\.\\|:") line with *)
(*           | [modul; rule; num] -> (\* FIXME *\) acc2  *)
(*           | _ -> Log.fcritical "invalid stat line: %s" line *)
(*         ) acc (File.read stat_file) *)
(*     ) init stat_files *)


let make_index ~title ~grs_file ~html ~grs ~output_dir ~base_names  =
  let init = Corpus_stat.empty grs in
  let corpus_stat =
    List.fold_left
      (fun acc base_name -> 
        Corpus_stat.add_gr_stat base_name (Gr_stat.load (Filename.concat output_dir (base_name^".stat"))) acc
      ) init base_names in
  Corpus_stat.save_html title grs_file html output_dir corpus_stat

  

let get_css_file = Filename.concat DATA_DIR "style.css"

