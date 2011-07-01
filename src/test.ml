open Grew_parser
open Checker
open HTMLer

let test =
	let gr_file_to_parse = ref None in
	let file_to_parse = ref None in
	let file_to_check = ref None in
	let html_out_dir = ref None in
	let args = [
		"-test_gr_parser", Arg.String (fun s -> gr_file_to_parse := Some s), "<file> the gr file to parse";
		"-test_grs_parser", Arg.String (fun s -> file_to_parse := Some s), "<file> the grs file to parse";
		"-test_grs_check", Arg.String (fun s -> file_to_check := Some s), "<file> the grs file to parse+check";
		"-test_grs_html_out_dir", Arg.String (fun s -> html_out_dir := Some s), "<out_dir> the dir where to put html files\n"
	] in
	Arg.parse args (fun opt -> ()) "\n\nTest options";
	
	begin
	match !gr_file_to_parse with
		| Some file ->
			begin try
				let ast = Grew_parser.parse_file_to_gr file in
				(* begin try *)
				(* 	Checker.check_gr ast *)
				(* with *)
				(* 	| Checker.Node_already_defined_in_graph (id,(file,line)) -> *)
				(* 		Printf.printf "Node '%s' already defined (file: %s, line: %d)!" id file line; *)
				(* 		exit 1 *)
				(* 	| Checker.Index_already_defined_in_graph (id,(file,line)) -> *)
				(* 		Printf.printf "Index '%s' already defined (file: %s, line: %d)!" id file line; *)
				(* 		exit 1 *)
				(* 	| Checker.Edge_already_defined_in_graph (n1,n2,(file,line)) -> *)
				(* 		Printf.printf "Edge '%s->%s' already defined (file: %s, line: %d)!" n1 n2 file line; *)
				(* 		exit 1 *)
				(* end; *)
(*				Printf.printf "%s\n%!" (Ast.Grew_string.to_string_gr ast);*)
(*				Printf.printf "%s\n%!" (Ast.Grew_dot.to_dot_gr ast);*)
(*				Printf.printf "%s\n%!" (Ast.Grew_dep2pict.to_dep2pict_gr ast)*)
				()
			with Grew_parser.Parse_error msg ->
				Printf.eprintf "%s\n%!" msg; exit 1
			end;
			exit 0;
		| None -> ()
	end;
	begin
	match !file_to_parse with
		| None -> ()
		| Some file ->
			try
				let ast = Grew_parser.parse_file_to_grs file in
				begin
				match !html_out_dir with
					| None -> ()
					| Some d -> HTMLer.proceed d ast;
				end
			with Grew_parser.Parse_error msg ->
				Printf.eprintf "%s\n%!" msg; exit 1
	end;
	begin
	match !file_to_check with
		| None -> ()
		| Some file ->
			try
				let ast = Grew_parser.parse_file_to_grs file  in ()
				(* try *)
				(* 	Checker.check_grs ast *)
				(* with *)
				(* 	| Checker.Module_already_defined (m,(file,line)) -> *)
				(* 		Printf.printf "Module '%s' already defined (file: %s, line: %d)!" m file line *)
				(* 	| Checker.Rule_already_defined (m,r,(file,line)) -> *)
				(* 		Printf.printf "Rule '%s.%s' already defined (file: %s, line: %d)!" m r file line *)
				(* 	| Checker.Invalid_feature (mname,(mfile,mline),rname,(rfile,rline),nodename,(nfile,nline),fname,file,line) -> *)
				(* 		Printf.printf "Invalid feature '%s' (file: %s, line: %d)\nIn node '%s' (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)\n" *)
				(* 			fname file line *)
				(* 			nodename nfile nline *)
				(* 			rname rfile rline *)
				(* 			mname mfile mline *)
					
				(* 	| Checker.Node_already_defined (mname,(mfile,mline),rname,(rfile,rline),nodename,(nfile,nline)) -> *)
				(* 		Printf.printf "Node '%s' already defined (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)\n" *)
				(* 			nodename nfile nline *)
				(* 			rname rfile rline *)
				(* 			mname mfile mline *)
				(* 	| Checker.Edge_already_defined (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline)) -> *)
				(* 		Printf.printf "Edge '%s' already defined (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)\n" *)
				(* 			eid efile eline *)
				(* 			rname rfile rline *)
				(* 			mname mfile mline *)
				(* 	| Checker.Node_not_defined (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline),nodename) -> *)
				(* 		Printf.printf "The node '%s' isn't defined%s (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)\n" *)
				(* 			nodename *)
				(* 			(match eid with Some id -> " in edge '"^id^"'" | None -> "") efile eline *)
				(* 			rname rfile rline *)
				(* 			mname mfile mline *)
				(* 	| Checker.Unavailable_label (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline),label) -> *)
				(* 		Printf.printf "The label '%s' is unavailable%s (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)\n" *)
				(* 			label *)
				(* 			(match eid with Some id -> " in edge '"^id^"'" | None -> "") efile eline *)
				(* 			rname rfile rline *)
				(* 			mname mfile mline *)
				(* 	| Checker.Forbidden_label (mname,(mfile,mline),rname,(rfile,rline),eid,(efile,eline),label) -> *)
				(* 		Printf.printf "The label '%s' is forbidden%s (file: %s, line: %d)\nIn rule '%s' (file: %s, line: %d)\nIn module '%s' (file: %s, line: %d)\n" *)
				(* 			label *)
				(* 			(match eid with Some id -> " in edge '"^id^"'" | None -> "") efile eline *)
				(* 			rname rfile rline *)
				(* 			mname mfile mline *)
				(* 	| Checker.Undefined_module (sname,(sfile,sline),mname) -> *)
				(* 		Printf.printf "Undefined module '%s' in sequence '%s' (file: %s, line: %d)" *)
				(* 			mname *)
				(* 			sname sfile sline *)
			with Grew_parser.Parse_error msg ->
				Printf.eprintf "%s\n%!" msg; exit 1
	end;
