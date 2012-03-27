open Grew_ast
module Html = struct

let index_text table = "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<a href=features.html>Features</a><br/>
		<a href=labels.html>Globals labels</a><br/>
		<a href=modules.html>Index of modules</a><br/>
		<a href=sequences.html>Index of sequences</a><br/>
		<br/><br/>
		<table class=\"indextable\">"^
		table^
		"</table>
	</body>
</html>"

let module_page_text previous next m ast = "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<div class=\"navbar\">"^
		(match previous with None -> "" | Some p -> "&nbsp;<a href=\""^p^".html\">Previous</a> ")^
		"&nbsp;<a href=\"index.html\">Up</a> "^
		(match next with None -> "" | Some p -> "&nbsp;<a href=\""^p^".html\">Next</a> ")^
		"</div>
		<center><h1>Module <div class=\"module_title\">"^m.Ast.module_id^"</div></h1></center><br/><br/>"^
		m.Ast.module_doc^

		
		
		"<h6>Rules</h6>"^
		"<table class=\"indextable\">"^
		
		(let rec compute tab = match tab with
			| [] -> ""
			| h::t ->
				"<tr><td width=\"200px\"><a href=\""^m.Ast.module_id^"_"^h.Ast.rule_id^".html\">"^h.Ast.rule_id^"</a></td><td>"^(
				let splitted = Str.split (Str.regexp "\n") h.Ast.rule_doc in
				if (List.length splitted > 0) then (List.hd splitted^" ...") else (h.Ast.rule_doc)
				)^"</td></tr>\n"^compute t
		in compute m.Ast.rules)^
		"</table>
		<br/> "^
		
	"</body>
</html>"

let rule_page_text previous next rule m ast file = "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<div class=\"navbar\">"^
		(match previous with None -> "" | Some p -> "&nbsp;<a href=\""^m.Ast.module_id^"_"^p^".html\">Previous</a> ")^
		"&nbsp;<a href=\""^m.Ast.module_id^".html\">Up</a> "^
		(match next with None -> "" | Some p -> "&nbsp;<a href=\""^m.Ast.module_id^"_"^p^".html\">Next</a> ")^
		"</div>
		<center><h1>Rule <a href=\""^m.Ast.module_id^".html\">"^m.Ast.module_id^"</a>.<div class=\"module_title\">"^rule.Ast.rule_id^"</div></h1></center>
		<br/><br/><div id=doc>"^rule.Ast.rule_doc^"</div>"^


"
	</pre></code><br/><h6>Code</h6><pre>"^
	(AST_HTML.to_html_rules [rule])^
	"</pre><br/>
	
	</body>
</html>"



let sequences_text ast = 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<div class=\"navbar\">
		&nbsp;<a href=\"index.html\">Up</a>
		</div>
		<center><h1>Index of sequences</h1></center><table width=100%>"^
		(let abc = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'] in
		let rec to_html tab = match tab with 
			| [] -> ""
			| h::t -> "<tr><td width=\"200px\"><a href=\""^h.Ast.seq_name^".html\">"^h.Ast.seq_name^"</a></td><td>"^
				h.Ast.seq_doc^
				"</td></tr>"^
				to_html t
		in
		let rec compute abc = match abc with 
			[] -> ""
			| h::t -> 
				let tmp = List.filter (fun seq -> Char.uppercase seq.Ast.seq_name.[0] = h) ast.Ast.sequences in
				if (List.length tmp > 0) then (
					"<tr><td colspan=2 ><br/><br/><h6>"^(Char.escaped h)^"</h6></td></tr> "^
					(to_html tmp)^
					compute t 
				) else ( compute t )
		in (compute abc)^"</table>";
		)^
	"</body>
</html>"
  
let index_modules_text ast = 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<div class=\"navbar\">
		&nbsp;<a href=\"index.html\">Up</a>
		</div>
		<center><h1>Index of modules</h1></center><table width=100%>"^
		(let abc = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'] in
		let rec to_html tab = match tab with 
			| [] -> ""
			| h::t -> "<tr><td width=\"200px\"><a href=\""^h.Ast.module_id^".html\">"^h.Ast.module_id^"</a></td><td>"^h.Ast.module_doc^"</td></tr>"^
				to_html t
		in
		let rec compute abc = match abc with 
			[] -> ""
			| h::t -> 
				let tmp = List.filter (fun m -> Char.uppercase m.Ast.module_id.[0] = h) ast.Ast.modules in
				if (List.length tmp > 0) then (
					"<tr><td colspan=2 ><br/><br/><h6>"^(Char.escaped h)^"</h6></td></tr> "^
					(to_html tmp)^
					compute t 
				) else ( compute t )
		in (compute abc)^"</table>";
		)^
	"</body>
</html>"

let features_domain_text ast = 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<div class=\"navbar\">
		&nbsp;<a href=\"index.html\">Up</a>
		</div>
		<center><h1>Features</h1></center>"^
		"<code class=\"code\">"^
			(let rec compute tab = match tab with
				| [] -> ""
				| h::t -> begin match h with Ast.Open a -> "<b>"^a^"</b> : *<br/>"^compute t | Ast.Closed (name,values)  -> "<b>"^name^"</b> : "^(AST_HTML.feat_values_tab_to_html values)^"<br/>"^compute t; end; 
			in compute ast.Ast.domain)^
		"</code>"^
	"</body>
</html>"

let globals_labels_text ast = 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<div class=\"navbar\">
		&nbsp;<a href=\"index.html\">Up</a>
		</div>
		<center><h1>Globals labels</h1></center>"^
		"<code class=\"code\">"^
		(
		let rec tab_to_html tab = match tab with
			| [] -> ""
			| h::[] -> h
			| h::t -> h^", "^(tab_to_html t)
		in tab_to_html (List.map fst ast.Ast.labels)
		)^
		"</code>"^
	"</body>
</html>"

let sequence_page_text previous next sequence = 
let rec tab_to_html_arrow tab = match tab with
	| [] -> ""
	| h::[] -> "<a href=\""^h^".html\">"^h^"</a>"
	| h::t -> "<a href=\""^h^".html\">"^h^"</a> ⇨ "^(tab_to_html_arrow t)
in 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> 
<html> 
	<head> 
		<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"> 
	</head>
	<body>
		<div class=\"navbar\">"^
		(match previous with None -> "" | Some p -> "&nbsp;<a href=\""^p^".html\">Previous</a> ")^
		"&nbsp;<a href=\"sequences.html\">Up</a> "^
		(match next with None -> "" | Some p -> "&nbsp;<a href=\""^p^".html\">Next</a> ")^
		"</div>
		<center><h1>Sequence <div class=\"module_title\">"^sequence.Ast.seq_name^"</div></h1></center>
		<br/><br/>"^sequence.Ast.seq_doc^"<br/>"^
		"<h6>Sequence</h6>"^
		"<div class=\"code\">"^(tab_to_html_arrow sequence.Ast.seq_mod)^"</div>"^
		"</body>
</html>"

  let rec create_modules_table modules =
    match modules with
    | [] -> ""
    | h::t ->
	"<tr><td width=\"200px\"><a href=\""^h.Ast.module_id^".html\">"^h.Ast.module_id^"</a></td><td>"^h.Ast.module_doc^"</td></tr>\n"^
	create_modules_table t
	  

	let proceed output_dir ast = 
		ignore(Sys.command ("rm -rf "^output_dir));
		ignore(Sys.command ("mkdir "^output_dir));
		ignore(Sys.command ("cp "^DATA_DIR^"/style.css "^output_dir));
		
		(** index.html **)
		let index = Filename.concat output_dir "index.html" in
		
		
		let table = create_modules_table ast.Ast.modules in
			
		let index_out_ch = open_out index in
		output_string index_out_ch (index_text table);
		close_out index_out_ch;
		
		(** Sequences.html **)
		let sequences = Filename.concat output_dir "sequences.html" in
			
		let sequences_out_ch = open_out sequences in
		output_string sequences_out_ch (sequences_text ast);
		close_out sequences_out_ch;
		
		let sequences_array = Array.of_list (List.sort (fun a b -> String.compare a.Ast.seq_name b.Ast.seq_name) ast.Ast.sequences) in
		for i = 0 to (Array.length sequences_array -1) do
			let page = Filename.concat output_dir (sequences_array.(i).Ast.seq_name^".html") in
			let page_out_ch = open_out page in
			output_string page_out_ch (sequence_page_text (try Some (sequences_array.(i-1).Ast.seq_name) with _ -> None) (try Some (sequences_array.(i+1).Ast.seq_name) with _ -> None) sequences_array.(i));
			close_out page_out_ch;
		done;
		
		(** Modules.html **)
		let modules = Filename.concat output_dir "modules.html" in
			
		let modules_out_ch = open_out modules in
		output_string modules_out_ch (index_modules_text ast);
		close_out modules_out_ch;
		
		(** features.html **)
		let features = Filename.concat output_dir "features.html" in
			
		let features_out_ch = open_out features in
		output_string features_out_ch (features_domain_text ast);
		close_out features_out_ch;
		
		(** labels.html **)
		let labels = Filename.concat output_dir "labels.html" in
			
		let labels_out_ch = open_out labels in
		output_string labels_out_ch (globals_labels_text ast);
		close_out labels_out_ch;
		
		(** Modules + rules **)
		let modules_array = Array.of_list ast.Ast.modules in
		for i = 0 to (Array.length modules_array -1) do
			let page = Filename.concat output_dir (modules_array.(i).Ast.module_id^".html") in
			let page_out_ch = open_out page in
			output_string page_out_ch (module_page_text (try Some (modules_array.(i-1).Ast.module_id) with _ -> None) (try Some (modules_array.(i+1).Ast.module_id) with _ -> None) modules_array.(i) ast);
			close_out page_out_ch;
			
			let rules_array = Array.of_list modules_array.(i).Ast.rules in
			for j = 0 to (Array.length rules_array -1) do
				
			
				let page = Filename.concat output_dir (modules_array.(i).Ast.module_id^"_"^rules_array.(j).Ast.rule_id^".html") in
				let page_out_ch = open_out page in
				output_string page_out_ch 
                                  (rule_page_text 
                                     (try Some (rules_array.(j-1).Ast.rule_id) with _ -> None)
                                     (try Some (rules_array.(j+1).Ast.rule_id) with _ -> None) 
                                     rules_array.(j) 
                                     modules_array.(i) 
                                     ast 
                                     (modules_array.(i).Ast.module_id^"_"^rules_array.(j).Ast.rule_id^".html")
                                  );
				close_out page_out_ch;
				
				
			done;
		done;



end
