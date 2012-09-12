open Printf
open Log

open Grew_utils
open Grew_ast
open Grew_graph
open Grew_rule
open Grew_grs


let html_header ?title buff =
  let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

  wnl "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">";

  wnl "<html>";
  wnl "  <head>";
  wnl "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n";
  wnl "    <link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">";
  (match title with
    | Some t -> wnl "    <title>%s</title>" (Str.global_replace (Str.regexp "#") " " t)
    | None -> ()
  );
  wnl "  </head>";

(* ====================================================================================================*)
module Html_doc = struct

  let string_of_concat_item = function
    | Ast.Qfn_item (n,f) -> sprintf "%s.%s" n f
    | Ast.String_item s -> sprintf "\"%s\"" s
    | Ast.Param_item var -> sprintf "%s" var

  let string_of_qfn (node, feat_name) = sprintf "%s.%s" node feat_name

  let buff_html_command ?(li_html=false) buff (u_command,_) =
    bprintf buff "      ";
    if li_html then bprintf buff "<li>";
    (match u_command with
    | Ast.Del_edge_expl (n1,n2,label) -> bprintf buff "del_edge %s -[%s]-> %s" n1 label n2
    | Ast.Del_edge_name name -> bprintf buff "del_edge %s" name
    | Ast.Add_edge (n1,n2,label) -> bprintf buff "add_edge %s -[%s]-> %s" n1 label n2
    | Ast.Shift_in (n1,n2) -> bprintf buff "shift_in %s ==> %s" n1 n2
    | Ast.Shift_out (n1,n2) -> bprintf buff "shift_out %s ==> %s" n1 n2
    | Ast.Shift_edge (n1,n2) -> bprintf buff "shift %s ==> %s" n1 n2
    | Ast.Merge_node (n1,n2) -> bprintf buff "merge %s ==> %s" n1 n2
    | Ast.New_neighbour (n1,n2,label) -> bprintf buff "add_node %s: <-[%s]- %s" n1 label n2
    | Ast.Del_node n -> bprintf buff "del_node %s" n
    | Ast.Update_feat (qfn,item_list) -> bprintf buff "%s = %s" (string_of_qfn qfn) (List_.to_string string_of_concat_item " + " item_list)
    | Ast.Del_feat qfn -> bprintf buff "del_feat %s" (string_of_qfn qfn)
    );
    if li_html then bprintf buff "</li>\n" else bprintf buff ";\n"

  let html_feature (u_feature,_) =
    match u_feature.Ast.kind with
    | Ast.Equality values ->
        sprintf "%s=%s" u_feature.Ast.name (List_.to_string (fun x->x) "|" values)
    | Ast.Disequality [] ->
        sprintf "%s=*" u_feature.Ast.name
    | Ast.Disequality values ->
        sprintf "%s<>%s" u_feature.Ast.name (List_.to_string (fun x->x) "|" values)
    | Ast.Param index ->
        sprintf "%s=%s" u_feature.Ast.name index

  let buff_html_node buff (u_node,_) =
    bprintf buff "      %s [" u_node.Ast.node_id;
    bprintf buff "%s" (String.concat ", " (List.map html_feature u_node.Ast.fs));
    bprintf buff "];\n"

  let buff_html_edge buff (u_edge,_) =
    bprintf buff "      ";
    bprintf buff "%s" (match u_edge.Ast.edge_id with Some n -> n^": " | None -> "");
    bprintf buff "%s -[%s%s]-> %s;\n"
      u_edge.Ast.src
      (if u_edge.Ast.negative then "^" else "")
      (List_.to_string (fun x->x) "|" u_edge.Ast.edge_labels)
      u_edge.Ast.tar

  let buff_html_const buff (u_const,_) =
    bprintf buff "      ";
    (match u_const with
    | Ast.Start (id,labels) -> bprintf buff "%s -[%s]-> *" id (List_.to_string (fun x->x) "|" labels)
    | Ast.Cst_out id -> bprintf buff "%s -> *" id
    | Ast.End (id,labels) -> bprintf buff "* -[%s]-> %s" (List_.to_string (fun x->x) "|" labels) id
    | Ast.Cst_in id -> bprintf buff "* -> %s" id
    | Ast.Feature_eq (qfn_l, qfn_r) -> bprintf buff "%s = %s" (string_of_qfn qfn_l) (string_of_qfn qfn_r));
    bprintf buff "\n"

  let buff_html_pos_pattern buff pos_pattern =
    bprintf buff "    <font color=\"purple\">match</font> <b>{</b>\n";
    List.iter (buff_html_node buff) pos_pattern.Ast.pat_nodes;
    List.iter (buff_html_edge buff) pos_pattern.Ast.pat_edges;
    List.iter (buff_html_const buff) pos_pattern.Ast.pat_const;
    bprintf buff "    <b>}</b>\n"

  let buff_html_neg_pattern buff neg_pattern =
    bprintf buff "    <font color=\"purple\">without</font> <b>{</b>\n";
    List.iter (buff_html_node buff) neg_pattern.Ast.pat_nodes;
    List.iter (buff_html_edge buff) neg_pattern.Ast.pat_edges;
    List.iter (buff_html_const buff) neg_pattern.Ast.pat_const;
    bprintf buff "    <b>}</b>\n"

  let to_html_rules rules =
    let buff = Buffer.create 32 in
    List.iter
      (fun rule ->
        (* the first line: (lex_)rule / filter *)
        (match (rule.Ast.commands, rule.Ast.param) with
          | ([], None) ->
            bprintf buff "<font color=\"purple\">filter</font> %s <b>{</b>\n" rule.Ast.rule_id
          | (_,None) ->
            bprintf buff "<font color=\"purple\">rule</font> %s <b>{</b>\n" rule.Ast.rule_id
          | (_,Some (files, vars)) ->
            let param =
              match files with
                | [] -> sprintf "(feature %s)" (String.concat ", " vars)
                | l ->  sprintf "(feature %s; %s)"
                  (String.concat ", " vars)
                  (String.concat ", " (List.map (fun f -> sprintf "file \"%s\"" f) l)) in
            bprintf buff "<font color=\"purple\">lex_rule</font> %s %s <b>{</b>\n" rule.Ast.rule_id param
        );

        (* the match part *)
        buff_html_pos_pattern buff rule.Ast.pos_pattern;

        (* the without parts *)
        List.iter (buff_html_neg_pattern buff) rule.Ast.neg_patterns;

        (*  the commands part *)
        (match rule.Ast.commands with
          | [] -> ()  (* filter *)
          | list ->
            bprintf buff "    <font color=\"purple\">commands</font> <b>{</b>\n";
            List.iter (buff_html_command buff) list;
            bprintf buff "    <b>}</b>\n");

        bprintf buff "<b>}</b>\n";
      ) rules;
    Buffer.contents buff


  let doc_to_html string =
    if Str.string_match (Str.regexp "^  \\* ") string 0
    then sprintf "<font color=\"green\"><i>%s</i></font>" (String.sub string 4 ((String.length string)-4))
    else
      List.fold_left
        (fun acc (re,str) -> Str.global_replace (Str.regexp re) str acc)
        string
        [
          "\\[", "<b>";
          "\\]", "</b>";
          "~", "&nbsp;";
        ]

  let of_opt_color = function
    | None -> "black"
    | Some c -> c

  let module_page_text prev next module_ =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt  = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    let title = sprintf "Grew -- Module %s" module_.Ast.module_id in
    html_header ~title buff;

    wnl "  <body>";
    wnl "    <div class=\"navbar\">";
    w "      ";
    (match prev with Some p -> w "&nbsp;<a href=\"%s.html\">Previous</a>" p | _ -> ());
    w "&nbsp;<a href=\"index.html\">Up</a>";
    (match next with Some n -> w "&nbsp;<a href=\"%s.html\">Next</a>" n | _ -> ());
    wnl "    </div>";

    wnl "    <center><h1>Module <div class=\"module_title\">%s</div></h1></center><br/>" module_.Ast.module_id;
    List.iter (fun s -> wnl "    %s<br/>" (doc_to_html s)) module_.Ast.module_doc;
    wnl "    <h6>%d Rules</h6>" (List.length module_.Ast.rules);
    wnl "    <table class=\"indextable\">";
    List.iter
      (fun rule ->
        wnl "      <tr>";
        wnl "        <td width=\"200px\"><a href=\"%s_%s.html\">%s</a></td>" module_.Ast.module_id rule.Ast.rule_id rule.Ast.rule_id;
        (match rule.Ast.rule_doc with [] -> () | l::_ -> wnl "        <td>%s</td>" (doc_to_html l));
        wnl "      </tr>";
      ) module_.Ast.rules;
    wnl "    </table>";
    wnl "  </body>";
    wnl "</html>";
    Buffer.contents buff

  let rule_page_text ~dep prev next rule_ module_ =
    let rid = rule_.Ast.rule_id in
    let mid = module_.Ast.module_id in

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt  = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    let title = sprintf "Grew -- Rule %s/%s" mid rid in
    html_header ~title buff;

    wnl "  <body>";
    wnl "    <div class=\"navbar\">";
    w "      ";
    (match prev with Some p -> w "&nbsp;<a href=\"%s_%s.html\">Previous</a>" mid p | _ -> ());
    w "&nbsp;<a href=\"%s.html\">Up</a>" mid;
    (match next with Some n -> w "&nbsp;<a href=\"%s_%s.html\">Next</a>" mid n | _ -> ());
    wnl "    </div>";

    wnl "<center><h1>Rule <a href=\"%s.html\">%s</a>.<div class=\"module_title\">%s</div></h1></center>" mid mid rid;
    List.iter (fun s -> wnl "    %s<br/>" (doc_to_html s)) rule_.Ast.rule_doc;

    wnl "<h6>Code</h6>";
    wnl "<pre>";
    w "%s" (to_html_rules [rule_]);
    wnl "</pre>";

    if dep
    then
      begin
        wnl "<h6>Pattern</h6>";
        wnl "<pre>";
        w "<IMG src=\"%s\">" (sprintf "%s_%s-patt.png" mid rid);
        wnl "</pre>"
      end;

    let output_table args lines =
      wnl "    <table border=\"1\" cellspacing=\"0\" cellpadding=\"3\">";
      wnl "    <tr>%s</tr>" (List_.to_string (fun x -> sprintf "<th bgcolor=\"#cccccc\">%s</th>" x) "" args);
      List.iter
        (fun l -> wnl "<tr>%s</tr>"
          (List_.to_string (fun x -> sprintf "<td>%s</td>" x) "" (Str.split (Str.regexp "#+") l))
        ) lines;
      wnl "    </table>" in

    (match rule_.Ast.param with
      | None -> ()
      | Some (files, args) ->
        wnl "<h6>Lexical parameters</h6>";

        (* output local lexical parameters (if any) *)
        (match rule_.Ast.lp with
          | None -> ()
          | Some lines ->
            wnl "<b>Local parameters</b></br>";
            output_table args lines
        );

        (* output external lexical parameters (if any) *)
        List.iter
          (fun file ->
            let filename = Filename.concat module_.Ast.mod_dir file in
            wnl "<b>File:</b> %s</br>" file;
            let lines =
              try File.read filename
              with Sys_error msg -> wnl "<font color=\"red\">Error: %s</font>" msg; [] in
            output_table args lines
          ) files
    );
    wnl "  </body>";
    wnl "</html>";
    Buffer.contents buff


  let sequences_text ast =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Grew -- List of sequences" in
    html_header ~title buff;

    wnl "  <body>";
    wnl "  <div class=\"navbar\">&nbsp;<a href=\"index.html\">Up</a></div>";
    wnl "  <center><h1>List of sequences</h1></center>";
    List.iter
      (fun seq ->
        wnl "<h6>%s</h6>" seq.Ast.seq_name;
        List.iter (fun l -> wnl "<p>%s</p>" (doc_to_html l)) seq.Ast.seq_doc;

        wnl "<div class=\"code\">";
        wnl "%s" (String.concat " ⇨ " (List.map (fun x -> sprintf "<a href=\"%s.html\">%s</a>" x x) seq.Ast.seq_mod));
        wnl "</div>";

      ) ast.Ast.sequences;
    wnl "  </body>";
    wnl "</html>";
    Buffer.contents buff



  let index_modules_text ast =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Grew -- Index of modules" in
    html_header ~title buff;

    wnl "  <body>";
    wnl "  <div class=\"navbar\">&nbsp;<a href=\"index.html\">Up</a></div>";
    wnl "  <center><h1>Index of modules</h1></center>";
    wnl "  <table width=100%%>";
    List.iter
      (fun initial ->
        match List.filter (fun mod_ -> Char.uppercase mod_.Ast.module_id.[0] = initial) ast.Ast.modules  with
          | [] -> ()
          | l ->
            wnl "<tr><td colspan=2 ><h6>%s</h6></td></tr>" (Char.escaped initial);
            List.iter
              (fun mod_ ->
                wnl "<tr>";
                wnl "<td width=\"200px\"><a href=\"%s.html\">%s</a></td>" mod_.Ast.module_id mod_.Ast.module_id;
                (match mod_.Ast.module_doc with [] -> () | h::_ -> wnl "<td>%s</td>\n" (doc_to_html h));
                wnl "</tr>";
              ) l
      ) ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'];
    wnl "  </body>";
    wnl "</html>";
    Buffer.contents buff


  let domain_text ast =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    let title = sprintf "Grew -- Features domain" in
    html_header ~title buff;

    wnl "  <body>";
    wnl "  <div class=\"navbar\">&nbsp;<a href=\"index.html\">Up</a></div>";

    wnl "  <h6>Features</h6>";
    wnl "  <code class=\"code\">";
    List.iter
      (function
        | Ast.Open feat_name -> wnl "    <b>%s</b> : *<br/>" feat_name
        | Ast.Closed (feat_name,values) -> wnl "<b>%s</b> : %s<br/>" feat_name (String.concat " | " values)
      ) ast.Ast.domain;
    wnl "  </code>";

    wnl "  <h6>Labels</h6>";
    wnl "  <code class=\"code\">";
    (match ast.Ast.labels with
      | [] -> wnl "No labels defined!"
      | (l,c)::t -> w "<font color=\"%s\">%s</font>" (of_opt_color c) l;
        List.iter
          (fun (lab,color) ->
            w ", <font color=\"%s\">%s</font>" (of_opt_color color) lab;
          ) t;
        wnl "");
    wnl "  </code>";

    wnl "  </body>";
    wnl "</html>";
    Buffer.contents buff

  (* dep is a flag which is true iff dep file are shown in doc (iff dep2pict is available) *)
  let build ~dep file output_dir ast =
    ignore(Sys.command ("rm -rf "^output_dir));
    ignore(Sys.command ("mkdir "^output_dir));
    ignore(Sys.command ("cp "^DATA_DIR^"/style.css "^output_dir));

    (** index.html **)
    let index = Filename.concat output_dir "index.html" in

    (* let table = create_modules_table ast.Ast.modules in *)

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Grew -- Graph Rewriting System: %s" (Filename.basename file) in
    html_header ~title buff;

    wnl "  <body>";
    wnl "<a href=\"../sentences.html\">Sentences</a> -- <a href=\"../index.html\">Rewriting stats</a> -- GRS documentation";

    wnl "<h1>Graph Rewriting System: %s</h1>" (Filename.basename file);
    wnl "<center><b>full path</b>: %s</center>" file;

    wnl "<a href=domain.html>Domain</a><br/>";
    wnl "<a href=modules.html>Index of modules</a><br/>";
    wnl "<a href=sequences.html>List of sequences</a><br/>";

    wnl "<h6>Modules</h6>";
    wnl "<table class=\"indextable\">";
    List.iter
      (fun m ->
        wnl "<tr>";
        wnl "<td width=\"200px\"><a href=\"%s.html\">%s</a></td>" m.Ast.module_id m.Ast.module_id;
        (match m.Ast.module_doc with [] -> () | h::_ -> wnl "<td>%s</td>\n" (doc_to_html h));
        wnl "</tr>"
      ) ast.Ast.modules;

    wnl "</table>";
    wnl "</body>";
    wnl "</html>";

    let index_out_ch = open_out index in
    output_string index_out_ch (Buffer.contents buff);
    close_out index_out_ch;

    (** Sequences.html **)
    let sequences = Filename.concat output_dir "sequences.html" in

    let sequences_out_ch = open_out sequences in
    output_string sequences_out_ch (sequences_text ast);
    close_out sequences_out_ch;

    (** Modules.html **)
    let modules = Filename.concat output_dir "modules.html" in

    let modules_out_ch = open_out modules in
    output_string modules_out_ch (index_modules_text ast);
    close_out modules_out_ch;

    (** domain.html **)
    let domain = Filename.concat output_dir "domain.html" in

    let domain_out_ch = open_out domain in
    output_string domain_out_ch (domain_text ast);
    close_out domain_out_ch;

    (** Modules + rules **)
    let modules_array = Array.of_list ast.Ast.modules in
    for i = 0 to (Array.length modules_array -1) do
      let page = Filename.concat output_dir (modules_array.(i).Ast.module_id^".html") in
      let page_out_ch = open_out page in
      output_string page_out_ch
        (module_page_text
           (try Some (modules_array.(i-1).Ast.module_id) with _ -> None)
           (try Some (modules_array.(i+1).Ast.module_id) with _ -> None)
           modules_array.(i)
        );
      close_out page_out_ch;

      let rules_array = Array.of_list modules_array.(i).Ast.rules in
      for j = 0 to (Array.length rules_array -1) do

        let page = Filename.concat output_dir (modules_array.(i).Ast.module_id^"_"^rules_array.(j).Ast.rule_id^".html") in
        let page_out_ch = open_out page in
        output_string page_out_ch
          (rule_page_text
             ~dep
             (try Some (rules_array.(j-1).Ast.rule_id) with _ -> None)
             (try Some (rules_array.(j+1).Ast.rule_id) with _ -> None)
             rules_array.(j)
             modules_array.(i)
          );
        close_out page_out_ch;
      done;
    done
end

module Html_rh = struct
  let build ?main_feat ?(dot=false) ?(init_graph=true) ?(out_gr=false) ?header ~graph_file prefix t =

    (* remove files from previous runs *)
    let _ = Unix.system (sprintf "rm -f %s*.html" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.dep" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.png" prefix) in

    (if init_graph then Instance.save_dep_png ?main_feat prefix t.Rewrite_history.instance);

    let nf_files = Rewrite_history.save_nfs ?main_feat ~dot prefix t in

    let l = List.length nf_files in

    let local = Filename.basename prefix in

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Sentence: %s --- %d Normal form%s" local l (if l>1 then "s" else "") in
    html_header ~title buff;

    wnl "<body>";
    wnl "<a href=\"sentences.html\">Sentences</a> -- <a href=\"index.html\">Rewriting stats</a> -- <a href=\"doc/index.html\">GRS documentation</a>";

    wnl "<h1>%s</h1>" title;

    begin
      match header with
        | Some h -> wnl "%s</br>" h
        | None -> ()
    end;

    wnl "<b>Input file</b>: <a href=\"%s\">%s</a><br/>"
      graph_file (Filename.basename graph_file);

    wnl "<b>Input sentence</b>: <font color=\"green\"><i>%s</i></font></p><br/>"
      (G_graph.to_sentence ?main_feat t.Rewrite_history.instance.Instance.graph);

    if init_graph
    then
      begin
        wnl "<h6>Initial graph</h6>";
        wnl "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>" local
      end;

    List_.iteri
      (fun i (rules_list,file_name) ->
        wnl "<h6>Solution %d</h6>" (i+1);

        let local_name = Filename.basename file_name in

        if out_gr
        then wnl "<p><a href=\"%s.gr\">gr file</a>" local_name;

        (* the png file *)
        wnl "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>" local_name;

        (* the modules list *)
        wnl "<b>Modules applied</b>: %d<br/>" (List.length rules_list);

        let id = sprintf "id_%d" (i+1) in

        wnl "<a style=\"cursor:pointer;\"";
        wnl "  onClick=\"if (document.getElementById('%s').style.display == 'none')" id;
        wnl "      { document.getElementById('%s').style.display = 'block'; document.getElementById('p_%s').innerHTML = 'Hide applied rules'; }" id id;
        wnl " else { document.getElementById('%s').style.display = 'none';; document.getElementById('p_%s').innerHTML = 'Show applied rules'; }\">" id id;
        wnl "         <b><p id=\"p_%s\">Show applied rules</p></b>" id;
        wnl "</a>";

        wnl " <div id=\"%s\" style=\"display:none;\">" id;

        List.iter
          (fun (mod_name,rules) ->
            wnl "<p><b><font color=\"red\">%s: </font></b><font color=\"green\">%s</font></p>"
              mod_name
              (List_.to_string (fun x -> x) ", " rules);
          )
          rules_list;
        wnl " </div>"

      ) nf_files;

    wnl "</body>";
    wnl "</html>";

    let out_ch = open_out (sprintf "%s.html" prefix) in
    fprintf out_ch "%s" (Buffer.contents buff);
    close_out out_ch



  let error ?main_feat ?(dot=false) ?(init_graph=true) ?header prefix msg inst_opt =
    (* remove files from previous runs *)
    let _ = Unix.system (sprintf "rm -f %s*.html" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.dep" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.png" prefix) in

    (match inst_opt, init_graph with
      | (Some inst, true) when dot -> Instance.save_dot_png ?main_feat prefix inst
      | (Some inst, true) -> Instance.save_dep_png ?main_feat prefix inst
      | _ -> ()
    );

    let local = Filename.basename prefix in

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Sentence: %s --- ERROR" local in
    html_header ~title buff;

    wnl "<body>";
    wnl "<a href=\"sentences.html\">Sentences</a> -- <a href=\"index.html\">Rewriting stats</a> -- <a href=\"doc/index.html\">GRS documentation</a>";

    wnl "<h1>%s</h1>" title;

    if init_graph
    then
      begin
        wnl "<h6>Initial graph</h6>";
        wnl "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>" local
      end;

    wnl "<h2>ERROR: %s</h2>" msg;
    wnl "</body>\n</html>";

    let out_ch = open_out (sprintf "%s.html" prefix) in
    fprintf out_ch "%s" (Buffer.contents buff);
    close_out out_ch
end

module Html_sentences = struct
  let build output_dir sentences =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    html_header ~title:"Sentence list" buff;

    wnl "  <body>";
    wnl "Sentences -- <a href=\"index.html\">Rewriting stats</a> -- <a href=\"doc/index.html\">GRS documentation</a>";
    wnl "<h2>Sentences list</h2>";

    wnl "<center><table cellpadding=3 cellspacing=0 width=\"95%%\">";
    wnl "<tr><th class=\"first\">Number of normal forms</th><th>Sentence Id</th><th>Sentence</th></tr>";

    List.iter
      (fun (rewrited, base_name, amb, sentence) ->
        wnl "<tr>";
        wnl "    <td class=\"first_stats\">%d</td>" amb;
        if rewrited
        then wnl "    <td class=\"stats\"><a href=\"%s.html\">%s</a></td>" base_name base_name
        else wnl "    <td class=\"stats\">%s</td>" base_name;
        wnl "  <td class=\"stats\">%s</td>" sentence;
        wnl "</tr>";
      ) sentences;

    wnl "</table></center>";
    wnl "</body>";
    wnl "</html>";

    let out_ch = open_out (Filename.concat output_dir "sentences.html") in
    fprintf out_ch "%s" (Buffer.contents buff);
    close_out out_ch
end (* module Html_sentences *)




module Gr_stat = struct

  (** the type [gr] stores the stats for the rewriting of one gr file *)
  type t =
    | Stat of ((int * int) StringMap.t * int) (* map: rule_name |-> (min,max) occ, number of solution *)
    | Error of string

  let opt_incr = function None -> Some 1 | Some x -> Some (x+1)
  let add_one_module modul_opt rules stat =
    match modul_opt with
      | Some modul ->
        List.fold_left
          (fun acc rule ->
            let key = sprintf "%s.%s" modul rule in
            let (old_min, old_max) = try StringMap.find key acc with Not_found -> (None, None) in
            StringMap.add key (opt_incr old_min, opt_incr old_max) acc
          ) stat rules
      | None when rules = [] -> stat
      | None -> Log.fcritical "Unconsistent rewrite history"

  let max_stat stat1 stat2 =
    StringMap.fold
      (fun key value acc ->
        let old = try StringMap.find key acc with Not_found -> 0 in
        StringMap.add key (max old value) acc
      ) stat1 stat2

  let opt_max x y = match (x,y) with
    | None, v | v, None -> v
    | Some v1, Some v2 -> Some (max v1 v2)

  let opt_min x y = match (x,y) with
    | None, v | v, None -> v
    | Some v1, Some v2 -> Some (min v1 v2)

  let min_max_stat stat1 stat2 =
    StringMap.fold
      (fun key (vmin, vmax) acc ->
        let (old_min, old_max) = try StringMap.find key acc with Not_found -> (Some 0, Some 0) in
        StringMap.add key (opt_min old_min vmin, opt_max old_max vmax) acc
      ) stat1 stat2



  let from_rew_history rew_history =
    let rec loop prev_module rh =
      let sub_stat =
        match List.map (loop (Some rh.Rewrite_history.module_name)) rh.Rewrite_history.good_nf with
          | [] -> StringMap.empty
          | h::t -> List.fold_left min_max_stat h t in
      add_one_module prev_module rh.Rewrite_history.instance.Instance.rules sub_stat
    in
    Stat
      (StringMap.map
         (function | Some i, Some j -> (i,j) | _ -> Log.critical "None in stat")
         (loop None rew_history),
       (Rewrite_history.num_sol rew_history)
      )

  let from_rew_history rew_history =
    let rec loop prev_module rh =
      let sub_stat =
        match (rh.Rewrite_history.good_nf, rh.Rewrite_history.bad_nf) with
          | [],[] -> Some (StringMap.empty)
          | [], _ -> None
          | l, _ ->
            match List_.opt_map (loop (Some rh.Rewrite_history.module_name)) l with
              | [] -> None
              | h::t -> Some (List.fold_left min_max_stat h t) in
      match sub_stat with
        | None -> None
        | Some stat -> Some (add_one_module prev_module rh.Rewrite_history.instance.Instance.rules stat)
    in
    match loop None rew_history with
      | None -> Stat (StringMap.empty, Rewrite_history.num_sol rew_history)
      | Some map ->
        Stat
          (
            StringMap.map (function Some i, Some j -> (i,j) | _ -> Log.critical "None in stat") map,
            Rewrite_history.num_sol rew_history
          )



  let save stat_file t =
    let out_ch = open_out stat_file in
    (match t with
      | Error msg -> fprintf out_ch "ERROR\n%s" msg
      | Stat (map, num) ->
        fprintf out_ch "NUM_SOL:%d\n%!" num;
        StringMap.iter
          (fun rule_name (min_occ,max_occ) ->  fprintf out_ch "%s:%d:%d\n%!" rule_name min_occ max_occ) map
    );

    close_out out_ch

  let load stat_file =
    let sol = ref 0 in
    try
      let lines = File.read stat_file in
      match lines with
        | "ERROR" :: msg_lines -> Error (List_.to_string (fun x->x) "\n" msg_lines)
        | _ ->
          let map =
            List.fold_left
              (fun acc line ->
                match Str.split (Str.regexp ":") line with
                  | ["NUM_SOL"; num] -> sol := int_of_string num; acc
                  | [modu_rule; vmin; vmax] -> StringMap.add modu_rule (int_of_string vmin, int_of_string vmax) acc
                  | _ -> Log.fcritical "invalid stat line: %s" line
              ) StringMap.empty lines in
          Stat (map, !sol)
    with Sys_error msg -> Error (sprintf "Sys_error: %s" msg)
end (* module Gr_stat *)

module Corpus_stat = struct
  (** the [t] type stores stats for a corpus of gr_files *)
  (*
     first key: [m] module name
     second key: [r] rule name
     value: [occ_num, file_list] the total number of rule applications and the set of gr files concerned *)
  type t = {
    modules: Modul.t list;                                   (* ordered list of modules in the sequence *)
    map: ((int*int) * StringSet.t) StringMap.t StringMap.t;  (* map: see above *)
      amb: StringSet.t IntMap.t;                               (* key: nb of sols |-> set: sentence concerned *)
      error: (string * string) list;                           (* (file, msg) *)
    num: int;                                                (* an integer id relative to the corpus *)
  }

  let empty ~grs ~seq =
    (* let modules = try List.assoc seq grs.Grs.sequences with Not_found -> [seq] in *)
    let modules = Grs.modules_of_sequence grs seq in
    let map = List.fold_left
      (fun acc modul ->
        if List.exists (fun m -> modul.Modul.name = m.Modul.name) modules
        then
          let rule_map =
            List.fold_left
              (fun acc2 rule ->
                StringMap.add (Rule.get_name rule) ((0,0),StringSet.empty) acc2
              ) StringMap.empty modul.Modul.rules in
          StringMap.add modul.Modul.name rule_map acc
        else acc
      ) StringMap.empty (Grs.get_modules grs) in
    { modules=modules; map = map; amb = IntMap.empty; error = []; num = 0 }

  let add modul rule file (min_occ,max_occ) map =
    let old_rule_map = StringMap.find modul map in
    let ((old_min,old_max), old_file_set) = StringMap.find rule old_rule_map in
    StringMap.add
      modul
      (StringMap.add
         rule
           ((old_min + min_occ, old_max + max_occ), StringSet.add file old_file_set)
             old_rule_map
      ) map

  let add_gr_stat base_name gr_stat t =
    match gr_stat with
      | Gr_stat.Error msg -> { t with error = (base_name, msg) :: t.error; num = t.num+1 }
      | Gr_stat.Stat (map, sol) ->
        let new_map =
          StringMap.fold
            (fun modul_rule (min_occ,max_occ) acc ->
              match Str.split (Str.regexp "\\.") modul_rule with
                | [modul; rule] -> add modul rule base_name (min_occ,max_occ) acc
                | _ -> Log.fcritical "illegal modul_rule spec \"%s\"" modul_rule
            ) map t.map in
        let new_amb =
          let old = try IntMap.find sol t.amb with Not_found -> StringSet.empty in
          IntMap.add sol (StringSet.add base_name old) t.amb in
        { t with map = new_map; num = t.num+1; amb=new_amb; }



  let unfoldable_set output_dir out_ch ?(bound=10) id file_set =
    let counter = ref 0 in

    StringSet.iter
      (fun file ->
        if !counter = bound
        then fprintf out_ch "<div id=\"%s\" style=\"display:none;\">\n" id;
        incr counter;

        let link =
          if Sys.file_exists (Filename.concat output_dir (sprintf "%s.html" file))
          then sprintf "<a href=\"%s.html\">%s</a>" file file
          else file in

        fprintf out_ch "%s &nbsp;&nbsp;\n" link
      ) file_set;

    if (!counter > bound)
    then
      begin
        fprintf out_ch "</div>\n";
        let if_part = sprintf "document.getElementById('%s').style.display = 'block'; document.getElementById('p_%s').innerHTML = '- Show first %d -';" id id bound in
        let else_part = sprintf "document.getElementById('%s').style.display = 'none'; document.getElementById('p_%s').innerHTML = '+ Show all +';" id id in
        fprintf out_ch "  <div>\n";
        fprintf out_ch "    <a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s').style.display == 'none') { %s } else { %s }\">\n" id if_part else_part;
        fprintf out_ch "      <b><p id=\"p_%s\">+ Show all +</p></b>\n" id;
        fprintf out_ch "    </a>\n";
        fprintf out_ch "  </div>\n";
      end





  let save_html ~title ~grs_file ~input_dir ~output_dir t =
   (*  a fucntion to get the ration wrt the full set [t] *)
    let ratio nb = (float nb) /. (float t.num) *. 100. in

   (* put the css file the [output_dir] *)
    ignore(Sys.command("cp "^(Filename.concat DATA_DIR "style.css")^" "^(Filename.concat output_dir "style.css")));

   (* output of index.html *)
    let out_ch = open_out (Filename.concat output_dir "index.html") in

    fprintf out_ch "<head>\n";
    fprintf out_ch "  <link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">\n";
    fprintf out_ch "  <title>%s</title>\n" title;
    fprintf out_ch "  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n";
    fprintf out_ch "</head>\n";

    fprintf out_ch "<a href=\"sentences.html\">Sentences</a> -- Rewriting stats -- <a href=\"doc/index.html\">GRS documentation</a>\n";

    fprintf out_ch "<h1>%s</h1>\n" (Str.global_replace (Str.regexp "#") " " title);
    fprintf out_ch "<h2>Rewriting stats</h2>\n";

    fprintf out_ch "<center><table cellpadding=3 cellspacing=0 width=\"95%%\">\n";
    List.iter
      (fun modul ->
        let modul_name = modul.Modul.name in
        let rules = StringMap.find modul_name t.map in
        fprintf out_ch "<tr><td colspan=\"5\" style=\"padding: 0px;\"><h6>Module %s</h6></td></tr>\n" modul_name;
        fprintf out_ch "<tr><th class=\"first\">Rule</th><th>#occ(min/max)</th><th>#files</th><th>Ratio</th><th>Files</th></tr>\n";
        let ((min_occ, max_occ), full_sent) =
          StringMap.fold
            (fun _ ((v_min,v_max), file_set) ((acc_min,acc_max), acc_sent) ->
              ((acc_min+v_min, acc_max+v_max), StringSet.union acc_sent file_set)
            )
            rules ((0,0),StringSet.empty) in
        let tot_sent = StringSet.cardinal full_sent in
        fprintf out_ch "<tr>\n";
        fprintf out_ch "<td class=\"first_total\">Total for module</td>\n";
        fprintf out_ch "<td class=\"total\">%d/%d</td>" min_occ max_occ;
        fprintf out_ch "<td class=\"total\">%d</td>" tot_sent;
        fprintf out_ch "<td class=\"total\">%.2f%%</td>" (ratio tot_sent);
        fprintf out_ch "<td class=\"total\">&nbsp;</td>\n";
        fprintf out_ch "</tr>\n";

        List.iter (* iteration on list to keep the same order in html output and in grs input *)
          (fun rule ->
            let rule_name = Rule.get_name rule in
            let ((min_occ, max_occ), file_set) = StringMap.find rule_name rules in

            let id = sprintf "%s_%s" modul_name rule_name in
            let file_num = StringSet.cardinal file_set in

            fprintf out_ch "<tr>\n";
            fprintf out_ch "  <td class=\"first_stats\"  valign=top><a href=\"doc/%s.html\">%s</a></td>\n"
              id
              rule_name;
            fprintf out_ch "  <td class=\"stats\"  valign=top>%d/%d</td>\n" min_occ max_occ;
            fprintf out_ch "  <td class=\"stats\"  valign=top>%d</td>\n" file_num;
            fprintf out_ch "  <td class=\"stats\"  valign=top>%.2f%%</td>\n" (ratio file_num);

            fprintf out_ch "  <td class=\"stats\">\n";
            (if file_num = 0
             then fprintf out_ch "  &nbsp;"
             else unfoldable_set output_dir out_ch id file_set);
            fprintf out_ch "  </td>\n";
            fprintf out_ch "</tr>\n";
          ) modul.Modul.rules
      ) t.modules;

   (* add a subtable for sentence ambiguity *)
    if not (IntMap.is_empty t.amb)
    then
      begin
        fprintf out_ch "<tr><td colspan=5><h6>Rewriting ambiguity</h6></td></tr>\n";
        fprintf out_ch "<tr><th class=\"first\" >Number of normal forms</th><th colspan=2 width=20>#files</th><th >Ratio</th><th>Files</th></tr>\n";

        IntMap.iter
          (fun num set ->
            let id = sprintf "amb_%d" num in
            let num_files = StringSet.cardinal set in
            fprintf out_ch "<tr>\n";
            fprintf out_ch "  <td class=\"first_stats\">%d</td>\n" num;
            fprintf out_ch "  <td class=\"stats\" colspan=2>%d</td>\n" num_files;
            fprintf out_ch "  <td class=\"stats\">%.2f%%</td>\n" (ratio num_files);
            fprintf out_ch "  <td class=\"stats\">";

            unfoldable_set output_dir out_ch id set;

            fprintf out_ch "  </td>\n";
            fprintf out_ch "</tr>\n") t.amb
      end;

    (* add a subtable for sentence that produces an error *)
    (match List.length t.error with
      | 0 -> ()
      | nb_errors ->
        fprintf out_ch "<tr><td colspan=5><h6>ERRORS</h6></td></tr>\n";
        fprintf out_ch "<tr><th class=\"first\" >Rule</th><th colspan=2 width=20>#files</th><th >Ratio</th><th>Files</th></tr>\n";

        fprintf out_ch "<tr>\n";
        fprintf out_ch "<td class=\"first_stats\">Errors</td>\n";
        fprintf out_ch "<td class=\"stats\" colspan=2>%d</td>\n" nb_errors;
        fprintf out_ch "<td class=\"stats\">%.2f%%</td>\n" (ratio nb_errors);
        fprintf out_ch "<td class=\"stats\">";

        match t.error with
          | [] -> fprintf out_ch "&nbsp;"
          | l ->
            List.iter
              (fun (file,err) ->
                if Sys.file_exists (Filename.concat output_dir (sprintf "%s.html" file))
                then fprintf out_ch "<a href=\"%s.html\">%s</a>: %s<br/>" file file err
                else fprintf out_ch "%s: %s<br/>" file err
              )
              (List.rev l);

            fprintf out_ch "</td>\n";
            fprintf out_ch "</tr>");

    fprintf out_ch "</table></center>\n";
    close_out out_ch
end (* module Stat *)
