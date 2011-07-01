open Printf 
open Log

open Utils

open Grew_parser
open Checker

open Graph
open Rule
open Grs


let _ = Log.set_active_levels [`WARNING; `MESSAGE]
let _ = Log.set_write_to_log_file false
let _ = Log.set_show_time true

let absolute s =
  if Filename.is_relative s
  then Filename.concat (Sys.getcwd ()) s
  else s

let interactive = ref false

let out_file = ref None
let usage = "Usage: grew [-dep <ext>|-png <ext>|-html] <option> input_file"
let head = ref ""
let title = ref ""

let input_file = ref "" 
let grs_file = ref ""
let sequence = ref ""

let _ =
  Arg.parse
    [ 
      "-grs", Arg.String (fun s -> grs_file := absolute s), " <file> select the GRS file";
      "-seq", Arg.String (fun s -> sequence := s), " <string> select the module sequence to use for rewriting";

      "-head", Arg.String (fun s -> head := s), " set an header for the html output)";
      "-title", Arg.String (fun s -> title := s), " set an title for the html output)";

      "-o", Arg.String (fun s -> out_file := Some s ), " base name used for output files";

      "-debug", Arg.Unit (fun () -> Log.set_active_levels [`DEBUG; `MESSAGE; `INFO; `WARNING];), " turn the debug mode on";
      "-verbose",  Arg.Unit (fun () -> Log.set_active_levels [`MESSAGE; `INFO; `WARNING];), " turn the verbose mode on";
      "-silent",  Arg.Unit (fun () -> Log.set_active_levels [];), " hide warnings";

      "-i", Arg.Unit (fun () -> interactive := true), " Interactive mode";
    ]
    (fun file -> input_file := file)
    usage
    
let load_grs () =     
  match !grs_file with
  | "" -> Log.critical "No grs file defined, Abort"
  | file -> 
      let ast = Grew_parser.parse_file_to_grs file in
      (* Checker.check_grs ast; *) 
      Grs.build ast

let load_graph file = 
  let ast_gr = Grew_parser.parse_file_to_gr file in
  (* Checker.check_gr ast_gr; *)
  Instance.build ast_gr
  
let rewrite ?(head = "") ?(title = "") grs seq input output =
  let buff = Buffer.create 16 in
  
  bprintf buff "%s\n" head;
  bprintf buff "<h2>GRS file: <a href=\"file:///%s\">%s</a></h2>\n" !grs_file (Filename.basename !grs_file);
  bprintf buff "<h2>Input file: <a href=\"file:///%s\">%s</a></h2>\n" input (Filename.basename input);
  
  let init = load_graph input in
  let rew_hist = Grs.rewrite grs seq init in
  (* let _ = Grs.build_rew_display grs seq init in *)
  Rewrite_history.save_html ~mode:Rewrite_history.Normal ~header:(Buffer.contents buff) ~title output rew_hist
    
let _ = 
  try
    match !input_file with
    | "" ->
	(* Printf.printf "==============================\n%!"; *)
	(* let xxx = Lexing.from_string "without { A -> B }" in *)
	(* let _ = Gr_grs_parser.neg_item Lexer.global xxx in *)
	()
	(* Log.fcritical "[GREW] No input file given\n%s\n" usage *)
    | file ->
	let grs = load_grs () in
	let seq = !sequence in 
	
	if !interactive
	then
	  try
	    while true do
	      let l = read_line () in
	      match Str.split (Str.regexp_string "##") l with
	      | [head; title; in_file; out_file] -> rewrite ~head ~title grs seq in_file out_file  
	      | _ -> Log.fcritical "[GREW]: CANNOT understand request line >>>%s<<<%!" l
	    done
	  with End_of_file ->  Log.message "[GREW] Bye !!%!\n"; exit 0
	else	
	  (match !out_file with 
	  | None -> ()
	  | Some output -> rewrite grs seq file (Filename.chop_suffix output ".html")
	  )
	  
  with
  (* | Checker.Module_already_defined (m,(file,line)) -> *)
  (*     Log.fcritical "[GRS] Module '%s' already defined (file: %s, line: %d)!" m file line *)
  (* | Checker.Rule_already_defined (m,r,(file,line)) -> *)
  (*     Log.fcritical "[GRS] Rule '%s.%s' already defined (file: %s, line: %d)!" m r file line *)
  (* | Checker.Invalid_feature (mname,(mfile,mline),rname,(rfile,rline),nodename,(nfile,nline),fname,file,line) -> *)
  (*     Log.fcritical "[GRS] Invalid feature '%s' (file: %s, line: %d)\nIn node '%s' (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)"  *)
  (* 		      fname file line  *)
  (* 		      nodename nfile nline  *)
  (* 		      rname rfile rline *)
  (* 		      mname mfile mline *)
  (* | Checker.Node_already_defined (mname,(mfile,mline),rname,(rfile,rline),nodename,(nfile,nline)) -> *)
  (*     Log.fcritical "[GRS] Node '%s' already defined (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)"  *)
  (* 		      nodename nfile nline  *)
  (* 		      rname rfile rline *)
  (* 		      mname mfile mline *)
  (* | Checker.Edge_already_defined (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline)) -> *)
  (*     Log.fcritical "[GRS] Edge '%s' already defined (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)"  *)
  (* 		      eid efile eline  *)
  (* 		      rname rfile rline *)
  (* 		      mname mfile mline *)
  (* | Checker.Node_not_defined (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline),nodename) -> *)
  (*     Log.fcritical "[GRS] The node '%s' isn't defined%s (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)"  *)
  (* 		      nodename *)
  (* 		      (match eid with Some id -> " in edge '"^id^"'" | None -> "") efile eline *)
  (* 		      rname rfile rline *)
  (* 		      mname mfile mline *)
  (* | Checker.Unavailable_label (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline),label) -> *)
  (*     Log.fcritical "[GRS] The label '%s' is unavailable%s (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)"  *)
  (* 		      label *)
  (* 		      (match eid with Some id -> " in edge '"^id^"'" | None -> "") efile eline *)
  (* 		      rname rfile rline *)
  (* 		      mname mfile mline *)
  (* | Checker.Forbidden_label (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline),label) -> *)
  (*     Log.fcritical "[GRS] The label '%s' is forbidden%s (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)"  *)
  (* 		      label *)
  (* 		      (match eid with Some id -> " in edge '"^id^"'" | None -> "") efile eline *)
  (* 		      rname rfile rline *)
  (* 		      mname mfile mline *)
  (* | Checker.Undefined_module (sname,(sfile,sline),mname) -> *)
  (*     Log.fcritical "[GRS] Undefined module '%s' in sequence '%s' (file: %s, line: %d)"  *)
  (* 		      mname *)
  (* 		      sname sfile sline *)
  | Grew_parser.Parse_error msg -> 
      Log.fcritical "[GRS] %s" msg
  | exc -> Log.fbug "uncaught exception '%s'" (Printexc.to_string exc); exit 2
