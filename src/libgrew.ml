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

let empty_grs = Grs.empty

let grs file doc_output_dir = 
  if (Sys.file_exists file) then (
    try
      let ast = Grew_parser.parse_file_to_grs file in
      (* Checker.check_grs ast; *)
      let grs = Grs.build ast in
      HTMLer.proceed doc_output_dir ast;
      grs
    with 
    | Grew_parser.Parse_error msg -> raise (Parsing_err msg)
    | Utils.Build (msg,loc) -> raise (Build (msg,loc))
    | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))
   ) else (
    raise (File_dont_exists file)
   )
   
let grs_only file = 
  if (Sys.file_exists file) then (
    try
      let ast = Grew_parser.parse_file_to_grs file in
      (* Checker.check_grs ast; *)
      let grs = Grs.build ast in
      grs
    with 
    | Grew_parser.Parse_error msg -> raise (Parsing_err msg)
    | Utils.Build (msg,loc) -> raise (Build (msg,loc))
    | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))
   ) else (
    raise (File_dont_exists file)
   )
      
let get_available_seq grs = Grs.sequences grs
    
    
let empty_gr = Instance.empty

let gr file =
  if (Sys.file_exists file) then (
    try
      let ast = Grew_parser.parse_file_to_gr file in
(*                        Checker.check_gr ast;*)
      Instance.build ast
    with
    | Grew_parser.Parse_error msg -> raise (Parsing_err msg)
    | Utils.Build (msg,loc) -> raise (Build (msg,loc))
    | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
    | exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))

   ) else (
    raise (File_dont_exists file)
   )

let rewrite ~gr ~grs ~seq =
  try
    Grs.build_rew_display grs seq gr
  with
  | Utils.Run (msg,loc) -> raise (Run (msg,loc))
  | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
  | exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))
        
        
IFDEF DEP2PICT THEN
let rewrite_to_html_intern ?(no_init=false) ?main_feat grs_file grs seq input output nb_sentence previous next = 
  let buff = Buffer.create 16 in

  let head = Printf.sprintf "
      <div class=\"navbar\">%s<a href=\"index.html\">Up</a>%s</div><br/>" 
          (if previous <> "" then (Printf.sprintf "<a href=\"%s.html\">Sentence %d</a> -- " previous (nb_sentence-1)) else "") 
          (if next <> "" then (Printf.sprintf " -- <a href=\"%s.html\">Sentence %d</a>" next (nb_sentence+1)) else "") in


  Printf.bprintf buff "%s\n" head;
  Printf.bprintf buff "<b>GRS file</b>: <a href=\"file:///%s\">%s</a></h2><br/>\n" (Filename.basename grs_file) (Filename.basename grs_file);
  Printf.bprintf buff "<b>Input file</b>: <a href=\"file:///%s\">%s</a></h2>\n" (Filename.basename input) (Filename.basename input);
  ignore(Sys.command(Printf.sprintf "cp %s %s" input (Filename.concat (Filename.dirname output) (Filename.basename input))));

  try
  	let init = Instance.build (Grew_parser.parse_file_to_gr input) in
  	try
      let rew_hist = Grs.rewrite grs seq init in
      (* let _ = Grs.build_rew_display grs seq init in *)
      let stats = 
        if no_init
        then Some (Rewrite_history.save_html ?main_feat ~init_graph:false ~header:(Buffer.contents buff) output rew_hist)
        else Some (Rewrite_history.save_html ?main_feat ~header:(Buffer.contents buff) output rew_hist) in
      stats 
    with 
      | Utils.Run (msg, Some (loc_file,loc_line)) -> 
          let html_ch = open_out (sprintf "%s.html" output) in
          let () = Html.enter html_ch ~header:(Buffer.contents buff) output in
          fprintf html_ch "<h6>Initial graph</h6>\n";
          Instance.save_dep_png ?main_feat output init;
          fprintf html_ch "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>\n" (Filename.basename output);
          fprintf html_ch "<h2>ERROR during rewriting:</h2>\n";
          fprintf html_ch "<p>Message: %s</p>\n" msg;
          fprintf html_ch "<p>File: %s</p>\n" loc_file;
          fprintf html_ch "<p>Line: %d</p>\n" loc_line;
          Html.leave html_ch;
          close_out html_ch;
          None
  with 
    | exc -> 
      let html_ch = open_out (sprintf "%s.html" output) in
      let () = Html.enter html_ch ~header:(Buffer.contents buff) output in
      fprintf html_ch "<h1>UNEXPECTED EXCEPTION: %s</h1>" (Printexc.to_string exc);
      Html.leave html_ch;
      close_out html_ch;
      None
  	  
 


let rewrite_to_html ?main_feat input_dir grs output_dir no_init current_grs_file current_grs seq title =
  try
    
    (* get ALL gr files *)
    let all_files = Array.to_list (Sys.readdir input_dir) in
    let gr_files = List.sort (fun a b -> compare a b) (List.filter (fun file -> Filename.check_suffix file ".gr") all_files) in
    let nb_files = List.length gr_files in
    let ratio nb = (float nb) /. (float nb_files) *. 100. in

    (* create html files *)  
    ignore(Sys.command(Printf.sprintf "cp %s %s" grs (Filename.concat output_dir (Filename.basename grs))));
    let sentence_counter = ref 1 in
    
    let stats = ref Utils.StringMap.empty in
    let errors = ref [] in

    List.iter 
      (fun input -> 
        Log.fmessage "Computing %s" input;
        let rules = rewrite_to_html_intern
            ~no_init
            current_grs_file
            current_grs
            seq
            (Filename.concat input_dir input)
            (Filename.concat output_dir (Filename.chop_extension input))
            ?main_feat
            !sentence_counter
            (if !sentence_counter > 1 then (Filename.chop_extension (List.nth gr_files (!sentence_counter-2))) else "")
            (if !sentence_counter < nb_files then (Filename.chop_extension (List.nth gr_files (!sentence_counter)))  else "") 
        in
        incr sentence_counter;
        match rules with
        | Some module_list -> 
            List.iter 
              (fun (module_name, rule_list) -> 
                let old_rule_list = 
                  try ref (Utils.StringMap.find module_name !stats) 
                  with Not_found -> ref Utils.StringMap.empty in
                List.iter 
                  (fun rule ->
                    let old = try Utils.StringMap.find rule !old_rule_list with Not_found -> [] in
                    old_rule_list := Utils.StringMap.add rule (input::old) !old_rule_list
                  ) rule_list;
                stats := Utils.StringMap.add module_name !old_rule_list !stats
              ) module_list
        | None -> errors := input :: !errors
      ) gr_files;


    
    let out_ch = open_out (Filename.concat output_dir "index.html") in
    
    let css = "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">" in
    
    ignore(Sys.command("cp "^(Filename.concat DATA_DIR "style.css")^" "^(Filename.concat output_dir "style.css")));
    
    Printf.fprintf out_ch "<head>\n%s\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" /></head>\n" css title;
    Printf.fprintf out_ch "<h1>%s</h1>\n" title;
    Printf.fprintf out_ch "<b>Grs file</b>:%s\n<br/>\n" (Filename.basename current_grs_file);
    Printf.fprintf out_ch "<b>%d Sentences</b><br/>\n<br/>\n" nb_files;
    Printf.fprintf out_ch "<center><table cellpadding=10 cellspacing=0 width=90%%>\n";
    Utils.StringMap.iter 
      (fun modul rules ->
        Printf.fprintf out_ch "<tr><td colspan=5><h6>Module %s</h6></td>\n" modul;
        Printf.fprintf out_ch "<tr><th class=\"first\" width=10>Rule</th><th width=10>#occ</th><th width=10>#files</th><th width=10>Ratio</th><th width=10>Files</th></tr>\n";
        Utils.StringMap.iter 
          (fun rule files ->
            let tmp = ref "" in
            let counter = ref 0 in
            let rec compute list = match list with
            | [] -> ()
            | h::[] ->
                if (!counter = 10) then (
                  tmp := Printf.sprintf "%s<div id=\"%s_%s\" style=\"display:none;\">\n" !tmp modul rule
                 );
                incr counter;
                tmp := Printf.sprintf "%s<a href=\"%s\">%s</a>" !tmp ((Filename.chop_extension h)^".html") (Filename.chop_extension h)
            | h::t -> 
                if (not (List.mem h t)) then ( (*avoid doublons*)
                  if (!counter = 10) then (
                    tmp := Printf.sprintf "%s<div id=\"%s_%s\" style=\"display:none;\">\n" !tmp modul rule
                   );
                  incr counter;
                  tmp := Printf.sprintf "%s<a href=\"%s\">%s</a><br/>" !tmp ((Filename.chop_extension h)^".html") (Filename.chop_extension h)
                 );
                compute t
            in compute (List.rev files);
            Printf.fprintf out_ch "<tr><td class=\"first_stats\" width=10 valign=top>%s</td><td class=\"stats\" width=10 valign=top>%d</td><td class=\"stats\" width=10 valign=top>%d</td><td class=\"stats\" width=10 valign=top>%.2f%%</td>" rule (List.length files) !counter (ratio !counter);
            Printf.fprintf out_ch "<td class=\"stats\">%s" !tmp;
            if (!counter > 10) 
            then (
              Printf.fprintf out_ch "</div><a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s_%s').style.display == 'none') { %s } else { %s }\"><b><p id=\"p_%s_%s\">+ Show more +</p></b></a>\n"
                modul rule 
                (Printf.sprintf "document.getElementById('%s_%s').style.display = 'block'; document.getElementById('p_%s_%s').innerHTML = '- Show less -';" modul rule modul rule)
                (Printf.sprintf "document.getElementById('%s_%s').style.display = 'none';; document.getElementById('p_%s_%s').innerHTML = '+ Show more +';" modul rule modul rule)
                modul rule;
             );
            Printf.fprintf out_ch "</td></tr>\n";
          ) rules;
      ) !stats;

    (* add a subtalbe for sentence that produces an error *)
    let nb_errors = List.length !errors in
    Printf.fprintf out_ch "<tr><td colspan=5><h6>ERRORS</h6></td>\n";
    Printf.fprintf out_ch "<tr><th class=\"first\" width=10>Rule</th><th colspan=2 width=20>#files</th><th width=10>Ratio</th><th>Files</th></tr>\n";

    Printf.fprintf out_ch "<tr>\n";
    Printf.fprintf out_ch "<td class=\"first_stats\">Errors</td>\n";
    Printf.fprintf out_ch "<td class=\"stats\" colspan=2>%d</td>\n" nb_errors;
    Printf.fprintf out_ch "<td class=\"stats\">%.2f%%</td>\n" (ratio nb_errors);
    Printf.fprintf out_ch "<td class=\"stats\">";
    List.iter 
      (fun err -> 
        Printf.fprintf out_ch "<a href=\"%s.html\">%s</a><br/>" (Filename.chop_extension err) (Filename.chop_extension err)
      ) (List.rev !errors);
    Printf.fprintf out_ch "</td>\n";
    Printf.fprintf out_ch "</tr>";


  
  
    Printf.fprintf out_ch "</table></center>\n";

    close_out out_ch;
    ()

  with
  | Utils.Run (msg,loc) -> raise (Run (msg,loc))
  | Utils.Bug (msg, loc) -> raise (Bug (msg,loc))
  | exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc), None))
ENDIF
let get_css_file = Filename.concat DATA_DIR "style.css"

