open Printf

open Grew_utils
open Grew_ast

(* ====================================================================================================*)
module Html = struct

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

  let header ?title buff =
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    wnl "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">";

    wnl "<html>";
    wnl "  <head>";
    wnl "    <link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">";
    (match title with
      | Some t -> wnl "    <title>%s</title>" (Str.global_replace (Str.regexp "#") " " t)
      | None -> ()
    );
    wnl "  </head>";
    wnl "  <body>"

  let module_page_text prev next module_ =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt  = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    let title = sprintf "Grew -- Module %s" module_.Ast.module_id in
    header ~title buff;

    wnl "    <div class=\"navbar\">";
    w "      ";
    (match prev with Some p -> w "&nbsp;<a href=\"%s.html\">Previous</a> " p | _ -> ());
    w "&nbsp;<a href=\"index.html\">Up</a> ";
    (match next with Some n -> w "&nbsp;<a href=\"%s.html\">Next</a> " n | _ -> ());
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
    header ~title buff;

    wnl "    <div class=\"navbar\">";
    w "      ";
    (match prev with Some p -> w "&nbsp;<a href=\"%s_%s.html\">Previous</a> " mid p | _ -> ());
    w "&nbsp;<a href=\"%s.html\">Up</a>" mid;
    (match next with Some n -> w "&nbsp;<a href=\"%s_%s.html\">Next</a> " mid n | _ -> ());
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
    header ~title buff;

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
    header ~title buff;

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
    header ~title buff;

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
  let proceed ~dep file output_dir ast =
    ignore(Sys.command ("rm -rf "^output_dir));
    ignore(Sys.command ("mkdir "^output_dir));
    ignore(Sys.command ("cp "^DATA_DIR^"/style.css "^output_dir));

    (** index.html **)
    let index = Filename.concat output_dir "index.html" in

    (* let table = create_modules_table ast.Ast.modules in *)

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Grew -- Graph Rewriting System: %s" (Filename.basename file) in
    header ~title buff;

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


  let html_sentences output_dir sentences =
    let buff = Buffer.create 32 in
    header ~title:"Sentence list" buff;

    bprintf buff "Sentences -- <a href=\"index.html\">Rewriting stats</a> -- <a href=\"doc/index.html\">GRS documentation</a>\n";
    bprintf buff "<h2>Sentences list</h2>\n";

    bprintf buff "<center><table cellpadding=3 cellspacing=0 width=95%%>\n";
    bprintf buff "<tr><th class=\"first\">Number of normal forms</th><th>Sentence</th></tr>\n";

    List.iter
      (fun (base_name_opt, amb, sentence) ->
        bprintf buff "<tr>\n";
        bprintf buff "    <td class=\"first_stats\">%d</td>\n" amb;
        (match base_name_opt with
          | Some base_name -> bprintf buff "  <td class=\"stats\"><a href=\"%s.html\">%s</a></td>\n" base_name sentence
          | None -> bprintf buff "  <td class=\"stats\">%s</td>\n" sentence);
        bprintf buff "</tr>\n";
      ) sentences;

    bprintf buff "</table></center>\n";

    let out_ch = open_out (Filename.concat output_dir "sentences.html") in
    fprintf out_ch "%s" (Buffer.contents buff);
    close_out out_ch

end (* module Html *)

