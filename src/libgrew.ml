include Grew_types

open Log

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
exception Bug of string

type grs = Grs.t
type gr = Instance.t

let empty_grs = Grs.empty

let grs file doc_output_dir = 
	if (Sys.file_exists file) then (
		try
			let ast = Grew_parser.parse_file_to_grs file in
(*			Checker.check_grs ast;*)
			let grs = Grs.build ast in
			HTMLer.proceed doc_output_dir ast;
			grs
		with 
		| Grew_parser.Parse_error msg -> raise (Parsing_err msg)
		| Utils.Build (msg,loc) -> raise (Build (msg,loc))
		| Utils.Bug msg -> raise (Bug msg)
		| exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc)))
	) else (
		raise (File_dont_exists file)
	)
	
let get_available_seq grs = Grs.sequences grs
	
	
let empty_gr = Instance.empty

let gr file =
	if (Sys.file_exists file) then (
		try
			let ast = Grew_parser.parse_file_to_gr file in
(*			Checker.check_gr ast;*)
  			Instance.build ast
		with
		| Grew_parser.Parse_error msg -> raise (Parsing_err msg)
		| Utils.Build (msg,loc) -> raise (Build (msg,loc))
		| Utils.Bug msg -> raise (Bug msg)
		| exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc)))

	) else (
		raise (File_dont_exists file)
	)

let rewrite ~gr ~grs ~seq =
  try
    Grs.build_rew_display grs seq gr
  with
  | Utils.Run (msg,loc) -> raise (Run (msg,loc))
  | Utils.Bug msg -> raise (Bug msg)
  | exc -> raise (Bug (Printf.sprintf "UNCATCHED EXCEPTION: %s" (Printexc.to_string exc)))
  
  
let rewrite_to_html ?main_feat input_dir grs output_dir no_init current_grs_file current_grs seq title =
  let rewrite_to_html_intern ?(no_init=false) grs_file grs seq input output nb_sentence previous next = 
    
    let buff = Buffer.create 16 in
    
    let head = Printf.sprintf "<div class=\"navbar\">%s<a href=\"index.html\">Up</a>%s</div><br/>" 
	(if previous <> "" then (Printf.sprintf "<a href=\"%s.html\">Sentence %d</a> -- " previous (nb_sentence-1)) else "") 
	(if next <> "" then (Printf.sprintf " -- <a href=\"%s.html\">Sentence %d</a>" next (nb_sentence+1)) else "")
    in

    let title = "Sentence "^(string_of_int nb_sentence) in

		let buff = Buffer.create 16 in

		let head = Printf.sprintf "
		<div class=\"navbar\">%s<a href=\"index.html\">Up</a>%s</div><br/>" 
			(if previous <> "" then (Printf.sprintf "<a href=\"%s.html\">Sentence %d</a> -- " previous (nb_sentence-1)) else "") 
			(if next <> "" then (Printf.sprintf " -- <a href=\"%s.html\">Sentence %d</a>" next (nb_sentence+1)) else "")
		in

		let title = "Sentence "^(string_of_int nb_sentence) in

		Printf.bprintf buff "%s\n" head;
		Printf.bprintf buff "<b>GRS file</b>: <a href=\"file:///%s\">%s</a></h2><br/>\n" (Filename.concat (Filename.dirname output) (Filename.basename grs_file)) (Filename.basename grs_file);
		Printf.bprintf buff "<b>Input file</b>: <a href=\"file:///%s\">%s</a></h2>\n" (Filename.concat (Filename.dirname output) (Filename.basename input)) (Filename.basename input);
		ignore(Sys.command(Printf.sprintf "cp %s %s" input (Filename.concat (Filename.dirname output) (Filename.basename input))));

		let init = 
		let ast_gr = 
			Grew_parser.parse_file_to_gr input in	
		 	(* Checker.check_gr ast_gr; *)
			Instance.build ast_gr 
		in
		let rew_hist = Grs.rewrite grs seq init in
		(* let _ = Grs.build_rew_display grs seq init in *)
		let stats = if (no_init) then (
			Rewrite_history.save_html ~mode:Rewrite_history.Only_nfs ~header:(Buffer.contents buff) ~title output rew_hist
		) else (
			Rewrite_history.save_html ~mode:Rewrite_history.Normal ~header:(Buffer.contents buff) ~title output rew_hist
		) in
		stats
	in
	
  (* get ALL gr files *)
  let gr_files = Array.to_list (Sys.readdir input_dir) in
  let gr_files = (List.sort (fun a b -> compare a b) (List.filter (fun file -> Filename.check_suffix file ".gr") gr_files)) in
  
  (* create html files *)
  
  ignore(Sys.command(Printf.sprintf "cp %s %s" grs (Filename.concat output_dir (Filename.basename grs))));
  let sentence_counter = ref 1 in
  
  let stats = ref Utils.StringMap.empty in
  
  List.iter (fun input -> 
    Log.fmessage "Computing %s" input;
    let rules = rewrite_to_html_intern
	~no_init
	current_grs_file
	current_grs
	seq
	(Filename.concat input_dir input)
	(Filename.concat output_dir (Filename.chop_extension input))
	!sentence_counter
	(if !sentence_counter>1 then (Filename.chop_extension (List.nth gr_files (!sentence_counter-2))) else "")
	(if !sentence_counter<(List.length gr_files) then (Filename.chop_extension (List.nth gr_files (!sentence_counter)))  else "") 
    in
    incr sentence_counter;
    List.iter (fun rules ->
      let ruls = try ref (Utils.StringMap.find (fst rules) !stats) with Not_found -> ref Utils.StringMap.empty in
      List.iter (fun r ->
	let old = try Utils.StringMap.find r !ruls with Not_found -> [] in
	ruls := Utils.StringMap.add r (input::old) !ruls
		) (snd rules);
      stats := Utils.StringMap.add (fst rules) !ruls !stats
	      ) rules;
	    ) gr_files;
  
	let out_ch = open_out (Filename.concat output_dir "index.html") in
	
	let css = "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">" in
	
	ignore(Sys.command("cp "^(Filename.concat DATA_DIR "style.css")^" "^(Filename.concat output_dir "style.css")));
	
	Printf.fprintf out_ch "<head>\n%s\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" /></head>\n" css title;
	Printf.fprintf out_ch "<h1>%s</h1>\n" title;
	Printf.fprintf out_ch "<b>Grs file</b>:%s\n<br/>\n" (Filename.basename current_grs_file);
	Printf.fprintf out_ch "<b>%d Sentences</b><br/>\n<br/>\n" (List.length gr_files);
	Printf.fprintf out_ch "<center><table cellpadding=10 cellspacing=0 width=90%%>\n";
	Utils.StringMap.iter (fun modul rules ->
		Printf.fprintf out_ch "<tr><td colspan=5><h6>Module %s</h6></td>\n" modul;
		Printf.fprintf out_ch "<tr><th class=\"first\" width=10>Rule</th><th width=10>#occ</th><th width=10>#files</th><th width=10>Ratio</th><th width=10>Files</th></tr>\n";
		Utils.StringMap.iter (fun rule files ->
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
			Printf.fprintf out_ch "<tr><td class=\"first_stats\" width=10 valign=top>%s</td><td class=\"stats\" width=10 valign=top>%d</td><td class=\"stats\" width=10 valign=top>%d</td><td class=\"stats\" width=10 valign=top>%.2f%%</td>" rule (List.length files) !counter (float_of_int !counter/.(float_of_int (List.length gr_files))*.100.);
			Printf.fprintf out_ch "<td class=\"stats\">%s" !tmp;
			if (!counter > 10) then (
				Printf.fprintf out_ch "</div><a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s_%s').style.display == 'none') { %s } else { %s }\"><b><p id=\"p_%s_%s\">+ Show more +</p></b></a>\n"
					modul rule 
					(Printf.sprintf "document.getElementById('%s_%s').style.display = 'block'; document.getElementById('p_%s_%s').innerHTML = '- Show less -';" modul rule modul rule)
					(Printf.sprintf "document.getElementById('%s_%s').style.display = 'none';; document.getElementById('p_%s_%s').innerHTML = '+ Show more +';" modul rule modul rule)
					modul rule
				;
			);
			Printf.fprintf out_ch "</td></tr>\n";
		) rules;
	) !stats;
	Printf.fprintf out_ch "</table></center>\n";
	
	close_out out_ch;
	()
  
  
  
  
let get_css_file = Filename.concat DATA_DIR "style.css"

