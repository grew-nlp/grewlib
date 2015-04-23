(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Log

open Grew_base
open Grew_types
open Grew_ast
open Grew_graph
open Grew_rule
open Grew_grs


let html_header ?css_file ?title ?(add_lines=[]) buff =
  let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

  wnl "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">";

  wnl "<html>";
  wnl "  <head>";
  wnl "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">";
  (match css_file with
    | Some file -> wnl "    <link rel=\"stylesheet\" href=\"%s\" type=\"text/css\">" file
    | None -> ()
  );
  (match title with
    | Some t -> wnl "    <title>%s</title>" (Str.global_replace (Str.regexp "#") " " t)
    | None -> ()
  );
  List.iter (fun line -> wnl "    %s" line) add_lines;
  wnl "  </head>";

(* ================================================================================*)
module Html_doc = struct

  let string_of_concat_item = function
    | Ast.Qfn_item id -> sprintf "%s" (Ast.dump_feature_ident id)
    | Ast.String_item s -> sprintf "\"%s\"" s
    | Ast.Param_item var -> sprintf "%s" var

  let buff_html_command ?(li_html=false) buff (u_command,_) =
    bprintf buff "      ";
    if li_html then bprintf buff "<li>";
    (match u_command with
    | Ast.Del_edge_expl (n1,n2,label) ->
      bprintf buff "del_edge %s -[%s]-> %s" (Ast.dump_command_node_ident n1) label (Ast.dump_command_node_ident n2)
    | Ast.Del_edge_name name -> bprintf buff "del_edge %s" name
    | Ast.Add_edge (n1,n2,label) ->
      bprintf buff "add_edge %s -[%s]-> %s" (Ast.dump_command_node_ident n1) label (Ast.dump_command_node_ident n2)

    | Ast.Shift_in (n1,n2,([],true)) ->
      bprintf buff "shift_in %s ==> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2)
    | Ast.Shift_in (n1,n2,(labels,false)) ->
      bprintf buff "shift_in %s =[%s]=> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2) (List_.to_string (fun x->x) "|" labels)
    | Ast.Shift_in (n1,n2,(labels,true)) ->
      bprintf buff "shift_in %s =[^%s]=> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2) (List_.to_string (fun x->x) "|" labels)

    | Ast.Shift_out (n1,n2,([],true)) ->
      bprintf buff "shift_out %s ==> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2)
    | Ast.Shift_out (n1,n2,(labels,false)) ->
      bprintf buff "shift_out %s =[%s]=> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2) (List_.to_string (fun x->x) "|" labels)
    | Ast.Shift_out (n1,n2,(labels,true)) ->
      bprintf buff "shift_out %s =[^%s]=> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2) (List_.to_string (fun x->x) "|" labels)

    | Ast.Shift_edge (n1,n2,([],true)) ->
      bprintf buff "shift %s ==> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2)
    | Ast.Shift_edge (n1,n2,(labels,false)) ->
      bprintf buff "shift %s =[%s]=> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2) (List_.to_string (fun x->x) "|" labels)
    | Ast.Shift_edge (n1,n2,(labels,true)) ->
      bprintf buff "shift %s =[^%s]=> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2) (List_.to_string (fun x->x) "|" labels)

    | Ast.Merge_node (n1,n2) -> bprintf buff "merge %s ==> %s" (Ast.dump_command_node_ident n1) (Ast.dump_command_node_ident n2)
    | Ast.New_neighbour (n1,n2,label) -> bprintf buff "add_node %s: <-[%s]- %s" n1 label (Ast.dump_command_node_ident n2)
    | Ast.Activate act_id -> bprintf buff "activate %s" (Ast.dump_command_node_ident act_id)
    | Ast.Del_node act_id -> bprintf buff "del_node %s" (Ast.dump_command_node_ident act_id)
    | Ast.Update_feat ((act_id, feat_name),item_list) ->
      bprintf buff "%s.%s = %s" (Ast.dump_command_node_ident act_id) feat_name (List_.to_string string_of_concat_item " + " item_list)
    | Ast.Del_feat (act_id, feat_name) ->
      bprintf buff "del_feat %s.%s" (Ast.dump_command_node_ident act_id) feat_name
    );
    if li_html then bprintf buff "</li>\n" else bprintf buff ";\n"

  let html_feature (u_feature,_) =
    match u_feature.Ast.kind with
    | Ast.Equality values ->
        sprintf "%s=%s" u_feature.Ast.name (List_.to_string (fun x->x) "|" values)
    | Ast.Disequality [] ->
        sprintf "%s=*" u_feature.Ast.name
    | Ast.Absent ->
        sprintf "!%s" u_feature.Ast.name
    | Ast.Disequality values ->
        sprintf "%s<>%s" u_feature.Ast.name (List_.to_string (fun x->x) "|" values)
    | Ast.Equal_param index ->
        sprintf "%s=%s" u_feature.Ast.name index

  let buff_html_node buff (u_node,_) =
    bprintf buff "      %s [" u_node.Ast.node_id;
    bprintf buff "%s" (String.concat ", " (List.map html_feature u_node.Ast.fs));
    bprintf buff "];\n"

  let buff_html_edge buff (u_edge,_) =
    bprintf buff "      ";
    bprintf buff "%s" (match u_edge.Ast.edge_id with Some n -> n^": " | None -> "");
    match u_edge.Ast.edge_label_cst with
    | (l,true) -> bprintf buff "%s -[^%s]-> %s;\n" u_edge.Ast.src (List_.to_string (fun x->x) "|" l) u_edge.Ast.tar
    | (l,false) -> bprintf buff "%s -[%s]-> %s;\n" u_edge.Ast.src (List_.to_string (fun x->x) "|" l) u_edge.Ast.tar

  let buff_html_const buff (u_const,_) =
    bprintf buff "      ";
    (match u_const with
    | Ast.Start (ident,labels) ->
      bprintf buff "%s -[%s]-> *" ident (List_.to_string (fun x->x) "|" labels)
    | Ast.Cst_out ident ->
      bprintf buff "%s -> *" ident
    | Ast.End (ident,labels) ->
      bprintf buff "* -[%s]-> %s" (List_.to_string (fun x->x) "|" labels) ident
    | Ast.Cst_in ident -> bprintf buff "* -> %s" ident
    | Ast.Feature_eq (feat_id_l, feat_id_r) ->
      bprintf buff "%s = %s" (Ast.dump_feature_ident feat_id_l) (Ast.dump_feature_ident feat_id_r);
    | Ast.Feature_diseq (feat_id_l, feat_id_r) ->
      bprintf buff "%s <> %s" (Ast.dump_feature_ident feat_id_l) (Ast.dump_feature_ident feat_id_r);
    | Ast.Feature_ineq (ineq, feat_id_l, feat_id_r) ->
      bprintf buff "%s %s %s" (Ast.dump_feature_ident feat_id_l) (Ast.string_of_ineq ineq) (Ast.dump_feature_ident feat_id_r)
    );
    bprintf buff "\n"

  let buff_html_pos_basic buff pos_basic =
    bprintf buff "    <font color=\"purple\">match</font> <b>{</b>\n";
    List.iter (buff_html_node buff) pos_basic.Ast.pat_nodes;
    List.iter (buff_html_edge buff) pos_basic.Ast.pat_edges;
    List.iter (buff_html_const buff) pos_basic.Ast.pat_const;
    bprintf buff "    <b>}</b>\n"

  let buff_html_neg_basic buff neg_basic =
    bprintf buff "    <font color=\"purple\">without</font> <b>{</b>\n";
    List.iter (buff_html_node buff) neg_basic.Ast.pat_nodes;
    List.iter (buff_html_edge buff) neg_basic.Ast.pat_edges;
    List.iter (buff_html_const buff) neg_basic.Ast.pat_const;
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
        buff_html_pos_basic buff rule.Ast.pos_basic;

        (* the without parts *)
        List.iter (buff_html_neg_basic buff) rule.Ast.neg_basics;

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
    | [] -> "black"
    | c::_ -> String.sub c 1 ((String.length c) - 1)

  let module_page_text ~corpus prev next module_ =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt  = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    let title = sprintf "Grew -- Module %s" module_.Ast.module_id in
    html_header ~css_file:"style.css" ~title buff;

    wnl "  <body>";
    if corpus
    then wnl "<a href=\"../sentences.html\">Sentences</a> -- <a href=\"../index.html\">Rewriting stats</a> -- GRS documentation";

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

  let rule_page_text ~corpus ~dep prev next rule_ module_ =
    let rid = rule_.Ast.rule_id in
    let mid = module_.Ast.module_id in

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt  = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    let title = sprintf "Grew -- Rule %s/%s" mid rid in
    html_header ~css_file:"style.css" ~title buff;

    wnl "  <body>";
    if corpus
    then wnl "<a href=\"../sentences.html\">Sentences</a> -- <a href=\"../index.html\">Rewriting stats</a> -- GRS documentation";

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
        (match rule_.Ast.lex_par with
          | None -> ()
          | Some lines ->
            wnl "<b>Local parameters</b><br/>";
            output_table args lines
        );

        (* output external lexical parameters (if any) *)
        List.iter
          (fun file ->
            let filename = Filename.concat module_.Ast.mod_dir file in
            wnl "<b>File:</b> %s<br/>" file;
            let lines =
              try File.read filename
              with Sys_error msg -> wnl "<font color=\"red\">Error: %s</font>" msg; [] in
            output_table args lines
          ) files
    );
    wnl "  </body>";
    wnl "</html>";
    Buffer.contents buff


  let sequences_text ~corpus ast =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Grew -- List of sequences" in
    html_header ~css_file:"style.css" ~title buff;

    wnl "  <body>";
    if corpus
    then wnl "<a href=\"../sentences.html\">Sentences</a> -- <a href=\"../index.html\">Rewriting stats</a> -- GRS documentation";

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
    html_header ~css_file:"style.css" ~title buff;

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


  let domain_text ~corpus ast =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    let title = sprintf "Grew -- Features domain" in
    html_header ~css_file:"style.css" ~title buff;

    wnl "  <body>";
    if corpus
    then wnl "<a href=\"../sentences.html\">Sentences</a> -- <a href=\"../index.html\">Rewriting stats</a> -- GRS documentation";

    wnl "  <div class=\"navbar\">&nbsp;<a href=\"index.html\">Up</a></div>";

    wnl "  <h6>Features</h6>";
    wnl "  <code class=\"code\">";
    List.iter
      (function
        | Domain.Closed (feat_name,values) -> wnl "<b>%s</b> : %s<br/>" feat_name (String.concat " | " values)
        | Domain.Open feat_name -> wnl "    <b>%s</b> : *<br/>" feat_name
        | Domain.Num feat_name -> wnl "    <b>%s</b> : #<br/>" feat_name
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

  let build ~dep ~corpus output_dir grs =
    let filename = Grs.get_filename grs in
    let ast = Grs.get_ast grs in
    ignore(Sys.command ("rm -rf "^output_dir));
    ignore(Sys.command ("mkdir "^output_dir));
    ignore(Sys.command ("cp "^DATA_DIR^"/style.css "^output_dir));

    (** index.html **)
    let index = Filename.concat output_dir "index.html" in

    (* let table = create_modules_table ast.Ast.modules in *)

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Grew -- Graph Rewriting System: %s" (Filename.basename filename) in
    html_header ~css_file:"style.css" ~title buff;

    wnl "  <body>";
    if corpus
    then wnl "<a href=\"../sentences.html\">Sentences</a> -- <a href=\"../index.html\">Rewriting stats</a> -- GRS documentation";

    wnl "<h1>Graph Rewriting System: %s</h1>" (Filename.basename filename);
    wnl "<center><b>full path</b>: %s</center>" filename;

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
    output_string sequences_out_ch (sequences_text ~corpus ast);
    close_out sequences_out_ch;

    (** Modules.html **)
    let modules = Filename.concat output_dir "modules.html" in

    let modules_out_ch = open_out modules in
    output_string modules_out_ch (index_modules_text ast);
    close_out modules_out_ch;

    (** domain.html **)
    let domain = Filename.concat output_dir "domain.html" in

    let domain_out_ch = open_out domain in
    output_string domain_out_ch (domain_text ~corpus ast);
    close_out domain_out_ch;

    (** Modules + rules **)
    let modules_array = Array.of_list ast.Ast.modules in
    for i = 0 to (Array.length modules_array -1) do
      let page = Filename.concat output_dir (modules_array.(i).Ast.module_id^".html") in
      let page_out_ch = open_out page in
      output_string page_out_ch
        (module_page_text ~corpus
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
          (rule_page_text ~corpus
             ~dep
             (try Some (rules_array.(j-1).Ast.rule_id) with _ -> None)
             (try Some (rules_array.(j+1).Ast.rule_id) with _ -> None)
             rules_array.(j)
             modules_array.(i)
          );
        close_out page_out_ch;
      done;
    done
end (* module Html_doc *)

(* ================================================================================ *)
module Html_rh = struct
  let build ?filter ?main_feat ?(dot=false) ?(init_graph=true) ?(out_gr=false) ?header ?graph_file prefix t =

    (* remove files from previous runs *)
    let _ = Unix.system (sprintf "rm -f %s*.html" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.dep" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.png" prefix) in

    (
      if init_graph
      then ignore (Instance.save_dep_png ?filter ?main_feat prefix t.Rewrite_history.instance)
    );

    let nf_files = Rewrite_history.save_nfs ?filter ?main_feat ~dot prefix t in

    let l = List.length nf_files in

    let local = Filename.basename prefix in

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Sentence: %s --- %d Normal form%s" local l (if l>1 then "s" else "") in
    html_header ~css_file:"style.css" ~title buff;

    wnl "<body>";
    wnl "<a href=\"sentences.html\">Sentences</a> -- <a href=\"index.html\">Rewriting stats</a> -- <a href=\"doc/index.html\">GRS documentation</a>";

    wnl "<h1>%s</h1>" title;

    begin
      match header with
        | Some h -> wnl "%s<br/>" h
        | None -> ()
    end;

    begin
      match graph_file with
        | Some gf ->
          wnl "<b>Input file</b>: <a href=\"%s\">%s</a><br/>"
            gf (Filename.basename gf)
        | None -> ()
    end;

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
      | (Some inst, true) -> ignore (Instance.save_dep_png ?main_feat prefix inst)
      | _ -> ()
    );

    let local = Filename.basename prefix in

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    let title = sprintf "Sentence: %s --- ERROR" local in
    html_header ~css_file:"style.css" ~title buff;

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
end (* module Html_rh *)

(* ================================================================================*)
module Html_sentences = struct
  let build ~title output_dir sentences =
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in

    html_header ~css_file:"style.css" ~title buff;

    wnl "  <body>";
    wnl "Sentences -- <a href=\"index.html\">Rewriting stats</a> -- <a href=\"doc/index.html\">GRS documentation</a>";
    wnl "<h1>%s</h1>" (Str.global_replace (Str.regexp "#") " " title);

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

(* ================================================================================*)
module Gr_stat = struct

  (** the type [gr] stores the stats for the rewriting of one gr file *)
  type t =
    | Stat of ((int * int) String_map.t * int) (* map: rule_name |-> (min,max) occ, number of solution *)
    | Error of string

  let opt_incr = function None -> Some 1 | Some x -> Some (x+1)
  let add_one_module modul_opt rules stat =
    match modul_opt with
      | Some modul ->
        List.fold_left
          (fun acc rule ->
            let key = sprintf "%s.%s" modul rule in
            let (old_min, old_max) = try String_map.find key acc with Not_found -> (None, None) in
            String_map.add key (opt_incr old_min, opt_incr old_max) acc
          ) stat rules
      | None when rules = [] -> stat
      | None -> Log.fcritical "Unconsistent rewrite history"

  let max_stat stat1 stat2 =
    String_map.fold
      (fun key value acc ->
        let old = try String_map.find key acc with Not_found -> 0 in
        String_map.add key (max old value) acc
      ) stat1 stat2

  let opt_max x y = match (x,y) with
    | None, v | v, None -> v
    | Some v1, Some v2 -> Some (max v1 v2)

  let opt_min x y = match (x,y) with
    | None, v | v, None -> v
    | Some v1, Some v2 -> Some (min v1 v2)

  let min_max_stat stat1 stat2 =
    String_map.fold
      (fun key (vmin, vmax) acc ->
        let (old_min, old_max) = try String_map.find key acc with Not_found -> (Some 0, Some 0) in
        String_map.add key (opt_min old_min vmin, opt_max old_max vmax) acc
      ) stat1 stat2



  let from_rew_history rew_history =
    let rec loop prev_module rh =
      let sub_stat =
        match List.map (loop (Some rh.Rewrite_history.module_name)) rh.Rewrite_history.good_nf with
          | [] -> String_map.empty
          | h::t -> List.fold_left min_max_stat h t in
      add_one_module prev_module rh.Rewrite_history.instance.Instance.rules sub_stat
    in
    Stat
      (String_map.map
         (function | Some i, Some j -> (i,j) | _ -> Log.critical "None in stat")
         (loop None rew_history),
       (Rewrite_history.num_sol rew_history)
      )

  let from_rew_history rew_history =
    let rec loop prev_module rh =
      let sub_stat =
        match (rh.Rewrite_history.good_nf, rh.Rewrite_history.bad_nf) with
          | [],[] -> Some (String_map.empty)
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
      | None -> Stat (String_map.empty, Rewrite_history.num_sol rew_history)
      | Some map ->
        Stat
          (
            String_map.map (function Some i, Some j -> (i,j) | _ -> Log.critical "None in stat") map,
            Rewrite_history.num_sol rew_history
          )



  let save stat_file t =
    let out_ch = open_out stat_file in
    (match t with
      | Error msg -> fprintf out_ch "ERROR\n%s" msg
      | Stat (map, num) ->
        fprintf out_ch "NUM_SOL:%d\n%!" num;
        String_map.iter
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
                  | [modu_rule; vmin; vmax] -> String_map.add modu_rule (int_of_string vmin, int_of_string vmax) acc
                  | _ -> Log.fcritical "invalid stat line: %s" line
              ) String_map.empty lines in
          Stat (map, !sol)
    with Sys_error msg -> Error (sprintf "Sys_error: %s" msg)
end (* module Gr_stat *)

(* ================================================================================*)
module Corpus_stat = struct
  (** the [t] type stores stats for a corpus of gr_files *)
  (*
     first key: [m] module name
     second key: [r] rule name
     value: [occ_num, file_list] the total number of rule applications and the set of gr files concerned *)
  type t = {
    modules: Modul.t list;                                   (* ordered list of modules in the sequence *)
    map: ((int*int) * String_set.t) String_map.t String_map.t;  (* map: see above *)
    amb: String_set.t Int_map.t;                               (* key: nb of sols |-> set: sentence concerned *)
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
                String_map.add (Rule.get_name rule) ((0,0),String_set.empty) acc2
              ) String_map.empty modul.Modul.rules in
          String_map.add modul.Modul.name rule_map acc
        else acc
      ) String_map.empty (Grs.get_modules grs) in
    { modules=modules; map = map; amb = Int_map.empty; error = []; num = 0 }

  let add modul rule file (min_occ,max_occ) map =
    let old_rule_map = String_map.find modul map in
    let ((old_min,old_max), old_file_set) = String_map.find rule old_rule_map in
    String_map.add
      modul
      (String_map.add
         rule
           ((old_min + min_occ, old_max + max_occ), String_set.add file old_file_set)
             old_rule_map
      ) map

  let add_gr_stat base_name gr_stat t =
    match gr_stat with
      | Gr_stat.Error msg -> { t with error = (base_name, msg) :: t.error; num = t.num+1 }
      | Gr_stat.Stat (map, sol) ->
        let new_map =
          String_map.fold
            (fun modul_rule (min_occ,max_occ) acc ->
              match Str.split (Str.regexp "\\.") modul_rule with
                | [modul; rule] -> add modul rule base_name (min_occ,max_occ) acc
                | _ -> Log.fcritical "illegal modul_rule spec \"%s\"" modul_rule
            ) map t.map in
        let new_amb =
          let old = try Int_map.find sol t.amb with Not_found -> String_set.empty in
          Int_map.add sol (String_set.add base_name old) t.amb in
        { t with map = new_map; num = t.num+1; amb=new_amb; }

  let unfoldable_set output_dir buff ?(bound=10) id file_set =
    let counter = ref 0 in

    String_set.iter
      (fun file ->
        if !counter = bound
        then bprintf buff "<div id=\"%s\" style=\"display:none;\">\n" id;
        incr counter;

        let link =
          if Sys.file_exists (Filename.concat output_dir (sprintf "%s.html" file))
          then sprintf "<a href=\"%s.html\">%s</a>" file file
          else file in

        bprintf buff "%s &nbsp;&nbsp;\n" link
      ) file_set;

    if (!counter > bound)
    then
      begin
        bprintf buff "</div>\n";
        let if_part = sprintf "document.getElementById('%s').style.display = 'block'; document.getElementById('p_%s').innerHTML = '- Show first %d -';" id id bound in
        let else_part = sprintf "document.getElementById('%s').style.display = 'none'; document.getElementById('p_%s').innerHTML = '+ Show all +';" id id in
        bprintf buff "  <div>\n";
        bprintf buff "    <a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s').style.display == 'none') { %s } else { %s }\">\n" id if_part else_part;
        bprintf buff "      <b><p id=\"p_%s\">+ Show all +</p></b>\n" id;
        bprintf buff "    </a>\n";
        bprintf buff "  </div>\n";
      end





  let save_html ~title ~grs_file ~input_dir ~output_dir t =
   (*  a fucntion to get the ration wrt the full set [t] *)
    let ratio nb = (float nb) /. (float t.num) *. 100. in

   (* put the css file the [output_dir] *)
    ignore(Sys.command("cp "^(Filename.concat DATA_DIR "style.css")^" "^(Filename.concat output_dir "style.css")));

    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    let w fmt  = Printf.ksprintf (fun x -> Printf.bprintf buff "%s" x) fmt in

    html_header ~css_file:"style.css" ~title buff;

    wnl "<a href=\"sentences.html\">Sentences</a> -- Rewriting stats -- <a href=\"doc/index.html\">GRS documentation</a>";

    wnl "<h1>%s</h1>" (Str.global_replace (Str.regexp "#") " " title);
    wnl "<h2>Rewriting stats</h2>";

    wnl "<center><table cellpadding=3 cellspacing=0 width=\"95%%\">";
    List.iter
      (fun modul ->
        let modul_name = modul.Modul.name in
        let rules = String_map.find modul_name t.map in
        wnl "<tr><td colspan=\"5\" style=\"padding: 0px;\"><h6>Module %s</h6></td></tr>" modul_name;
        wnl "<tr><th class=\"first\">Rule</th><th>#occ(min/max)</th><th>#files</th><th>Ratio</th><th>Files</th></tr>";
        let ((min_occ, max_occ), full_sent) =
          String_map.fold
            (fun _ ((v_min,v_max), file_set) ((acc_min,acc_max), acc_sent) ->
              ((acc_min+v_min, acc_max+v_max), String_set.union acc_sent file_set)
            )
            rules ((0,0),String_set.empty) in
        let tot_sent = String_set.cardinal full_sent in
        wnl "<tr>";
        wnl "  <td class=\"first_total\">Total for module</td>";
        wnl "  <td class=\"total\">%d/%d</td>" min_occ max_occ;
        wnl "  <td class=\"total\">%d</td>" tot_sent;
        wnl "  <td class=\"total\">%.2f%%</td>" (ratio tot_sent);
        wnl "  <td class=\"total\">&nbsp;</td>";
        wnl "</tr>";

        List.iter (* iteration on list to keep the same order in html output and in grs input *)
          (fun rule ->
            let rule_name = Rule.get_name rule in
            let ((min_occ, max_occ), file_set) = String_map.find rule_name rules in

            let id = sprintf "%s_%s" modul_name rule_name in
            let file_num = String_set.cardinal file_set in

            wnl "<tr>";
            wnl "  <td class=\"first_stats\"  valign=top><a href=\"doc/%s.html\">%s</a></td>" id rule_name;
            wnl "  <td class=\"stats\"  valign=top>%d/%d</td>" min_occ max_occ;
            wnl "  <td class=\"stats\"  valign=top>%d</td>" file_num;
            wnl "  <td class=\"stats\"  valign=top>%.2f%%</td>" (ratio file_num);

            wnl "  <td class=\"stats\">";
            (if file_num = 0
             then w "  &nbsp;"
             else unfoldable_set output_dir buff id file_set);
            wnl "  </td>";
            wnl "</tr>";
          ) modul.Modul.rules
      ) t.modules;

   (* add a subtable for sentence ambiguity *)
    if (List.for_all (fun m -> m.Modul.confluent) t.modules) || (Int_map.is_empty t.amb)
    then ()
    else
      begin
        wnl "<tr><td colspan=5><h6>Rewriting ambiguity</h6></td></tr>";
        wnl "<tr><th class=\"first\" >Number of normal forms</th><th colspan=2 width=20>#files</th><th >Ratio</th><th>Files</th></tr>";

        Int_map.iter
          (fun num set ->
            let id = sprintf "amb_%d" num in
            let num_files = String_set.cardinal set in
            wnl "<tr>";
            wnl "  <td class=\"first_stats\">%d</td>" num;
            wnl "  <td class=\"stats\" colspan=2>%d</td>" num_files;
            wnl "  <td class=\"stats\">%.2f%%</td>" (ratio num_files);
            w "  <td class=\"stats\">";

            unfoldable_set output_dir buff id set;

            wnl "  </td>";
            wnl "</tr>") t.amb
      end;

    (* add a subtable for sentence that produces an error *)
    (match List.length t.error with
      | 0 -> ()
      | nb_errors ->
        wnl "<tr><td colspan=5><h6>ERRORS</h6></td></tr>";
        wnl "<tr><th class=\"first\" >Rule</th><th colspan=2 width=20>#files</th><th >Ratio</th><th>Files</th></tr>";

        wnl "<tr>";
        wnl "<td class=\"first_stats\">Errors</td>";
        wnl "<td class=\"stats\" colspan=2>%d</td>" nb_errors;
        wnl "<td class=\"stats\">%.2f%%</td>" (ratio nb_errors);
        w "<td class=\"stats\">";

        match t.error with
          | [] -> w "&nbsp;"
          | l ->
            List.iter
              (fun (file,err) ->
                if Sys.file_exists (Filename.concat output_dir (sprintf "%s.html" file))
                then w "<a href=\"%s.html\">%s</a>: %s<br/>" file file err
                else wnl "%s: %s<br/>" file err
              )
              (List.rev l);

            wnl "</td>";
            wnl "</tr>");

    wnl "</table></center>";
    wnl "  </body>";
    wnl "</html>";

    let out_ch = open_out (Filename.concat output_dir "index.html") in
    fprintf out_ch "%s" (Buffer.contents buff);
    close_out out_ch
end (* module Stat *)

module Html_annot = struct

  let script_lines static_dir = [
    "<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.8.0/jquery.min.js\"></script>";
    sprintf "<script type=\"text/JavaScript\" src=\"%s\"></script>" (Filename.concat static_dir "annot.js")
  ]

  let build ~title static_dir annot_dir bn_rh_list =
    let alt_list = List_.flat_map
      (fun (base_name, rew_hist) ->
        List.mapi
          (fun i alt ->
            (sprintf "%s_%d" base_name i, alt)
          ) (Rewrite_history.save_annot annot_dir base_name rew_hist)
      ) bn_rh_list in

    let db_buff = Buffer.create 32 in
    let len = List.length alt_list in
    let cpt = ref 0 in
    List_.prev_next_iter
      (fun ?prev ?next (base_name,(sentid,i,(afn,apos),(bfn,bpos),(hpa,hpb))) ->
        incr cpt;
        let init_pos = match (hpa,hpb) with
          | (Some p, Some q) -> max 0. ((min p q) -. 500.)
          | (Some p, None) | (None, Some p) -> max 0. (p -. 500.)
          | _ -> 0. in

        (* all entries are "skipped" by default *)
        bprintf db_buff "N%s\n" base_name;

        let a = sprintf "%s_%d_A" sentid i
        and b = sprintf "%s_%d_B" sentid i in
        let next_php = match next with
          | None -> "index.php"
          | Some (bn,_) -> bn^".php" in

        let buff = Buffer.create 32 in
        let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
        let title = sprintf "Annotation Hit: %d/%d (%s)" !cpt len base_name in
        wnl "<?php require '%s'; ?>" (Filename.concat static_dir "record_choice.php");
        wnl "<?php require '%s'; ?>" (Filename.concat static_dir "annot_utils.php");
        html_header ~css_file:(Filename.concat static_dir "annot.css") ~add_lines:(script_lines static_dir) ~title buff;
        wnl "<body onload=\"set_view_pos(%g);\">" init_pos;
        wnl "<h2>%s</h2>" title;
        wnl "<div id=\"top\"><embed src=\"%s.svg\" type=\"image/svg+xml\"/></div>" a;
        wnl "<div id=\"bottom\"><embed src=\"%s.svg\" type=\"image/svg+xml\"/></div>" b;
        wnl "";
        wnl "<div id=\"middle\">";

        wnl "<table>";
        wnl "<tr>";
        wnl "<td><form action=\"index.php\"><input type=\"submit\" value=\"Index\"></form></td>";
        wnl "<td>";
        wnl "<form action=\"%s\" method=\"post\">" next_php;
        wnl "<input type=\"hidden\" name=\"to_log\" value=\"%s#%g#%s\" />" sentid apos afn;
        wnl "<input type=\"hidden\" name=\"hit_id\" value=\"%s_%d\" />" sentid i;
        wnl "<input type=\"hidden\" name=\"choice\" value=\"U\" />";
        wnl "<input type=\"submit\" value=\"Choose Up\" >";
        wnl "</form>";
        wnl "</td>";
        wnl "<td/>"; (* empty upper right *)
        wnl "</tr>";
        wnl "";
        wnl "<tr>";
        (match prev with
          | Some (bn,_) -> wnl "<td><form action=\"%s.php\"><input type=\"submit\" value=\"<--Prev--\"></form></td>" bn
          | None -> wnl "<td> <form> <input type=\"submit\" value=\"<--Prev--\" disabled> </form> </td>";
        );
        wnl "<td>";
        wnl "<form action=\"%s\" method=\"post\">" next_php;
        wnl "<input type=\"hidden\" name=\"to_log\" value=\"%s#%g#No_choice\" />" sentid bpos;
        wnl "<input type=\"hidden\" name=\"hit_id\" value=\"%s_%d\" />" sentid i;
        wnl "<input type=\"hidden\" name=\"choice\" value=\"N\" />";
        wnl "<input type=\"submit\" value=\"Don't choose\" >";
        wnl "</form>";
        wnl "</td>";
        (match next with
          | Some (bn,_) -> wnl "<td><form action=\"%s.php\"><input type=\"submit\" value=\"--Next-->\"></form></td>" bn
          | None -> wnl "<td> <form> <input type=\"submit\" value=\"--Next-->\" disabled> </form> </td>";
        );
        wnl "</tr>";
        wnl "";
        wnl "<tr>";
        wnl "<td/>"; (* empty lower left *)
        wnl "<td>";
        wnl "<form action=\"%s\" method=\"post\">" next_php;
        wnl "<input type=\"hidden\" name=\"to_log\" value=\"%s#%g#%s\" />" sentid bpos bfn;
        wnl "<input type=\"hidden\" name=\"hit_id\" value=\"%s_%d\" />" sentid i;
        wnl "<input type=\"hidden\" name=\"choice\" value=\"D\" />";
        wnl "<input type=\"submit\" value=\"Choose Down\" >";
        wnl "</form>";
        wnl "</td>";
        wnl "<td/>"; (* empty lower right *)
        wnl "</tr>";
        wnl "</table>";
        wnl "</div>";
        wnl "<?php highlight(\"%s_%d\") ?>" sentid i;
        wnl "</body>";
        wnl "</html>";

        let out_ch = open_out (sprintf "%s.php" (Filename.concat annot_dir base_name)) in
        fprintf out_ch "%s" (Buffer.contents buff);
        close_out out_ch
      ) alt_list;

    let out_ch = open_out (Filename.concat annot_dir "status.db") in
    fprintf out_ch "%s" (Buffer.contents db_buff);
    close_out out_ch;

    (* creation of the file index.php *)
    let buff = Buffer.create 32 in
    let wnl fmt = Printf.ksprintf (fun x -> Printf.bprintf buff "%s\n" x) fmt in
    wnl "<?php require '%s'; ?>" (Filename.concat static_dir "record_choice.php");
    wnl "<?php require '%s'; ?>" (Filename.concat static_dir "annot_utils.php");

    html_header ~css_file:(Filename.concat static_dir "annot.css") ~add_lines:(script_lines static_dir) ~title buff;
    wnl "<h2>%s</h2>" title;
    wnl "<?php index_table() ?>";

    let out_ch = open_out (Filename.concat annot_dir "index.php") in
    fprintf out_ch "%s" (Buffer.contents buff);
    close_out out_ch;
    ()
end (* module Html_annot *)
