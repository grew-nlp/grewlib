open Printf
open Log

open Grew_fs
open Grew_utils
open Grew_ast
open Grew_edge 
open Grew_command
open Grew_graph
open Grew_rule


module Rewrite_history = struct

  type t = {
      instance: Instance.t;
      module_name: string; 
      good_nf: t list; 
      bad_nf: Instance.t list;
    }

  let rec is_empty t = 
    (t.instance.Instance.rules = []) && List.for_all is_empty t.good_nf

  let rec num_sol = function
    | { good_nf = [] } -> 1
    | { good_nf = l} -> List.fold_left (fun acc t -> acc + (num_sol t)) 0 l
      
IFDEF DEP2PICT THEN

  (** [save_nfs ?main_feat base_name t] does two things:
      - write PNG files of normal forms
      - returns a list of couples (rules, file)
   *)
  let save_nfs ?main_feat base_name t = 
    let rec loop file_name rules t =
      match t.good_nf with
      | [] -> Instance.save_dep_png ?main_feat file_name t.instance; [rules, file_name] 
      | l -> 
          List_.foldi_left
            (fun i acc son -> 
              (* Instance.save_dep_png ?main_feat (sprintf "%s_%d" file_name i) son.instance; *)
              let nfs = loop 
                  (sprintf "%s_%d" file_name i) 
                  (rules @ [t.module_name, son.instance.Instance.rules]) 
                  son in
              nfs @ acc
            )
            [] l
    in loop base_name [] t

  let error_html ?main_feat ?(init_graph=true) ?header prefix msg inst_opt =
    (* remove files from previous runs *)
    let _ = Unix.system (sprintf "rm -f %s*.html" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.dep" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.png" prefix) in

    (match inst_opt, init_graph with
    | (Some inst, true) -> Instance.save_dep_png ?main_feat prefix inst
    | _ -> ());

    let local = Filename.basename prefix in
    
    (* All normal forms view *)
    let html_ch = open_out (sprintf "%s.html" prefix) in

    let title = sprintf "Sentence: %s --- ERROR" local in
    let () = Html.enter html_ch ~title ?header prefix in

    if init_graph
    then
      begin
        fprintf html_ch "<h6>Initial graph</h6>\n";
        fprintf html_ch "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>\n" local
      end;
    fprintf html_ch "<h2>ERROR: %s</h2>\n" msg;
    Html.leave html_ch;
    close_out html_ch

  let save_html ?main_feat ?(init_graph=true) ?header prefix t =
    (* remove files from previous runs *)
    let _ = Unix.system (sprintf "rm -f %s*.html" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.dep" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.png" prefix) in
    
    (if init_graph then Instance.save_dep_png ?main_feat prefix t.instance);

    let nf_files = save_nfs ?main_feat prefix t in
    
    let l = List.length nf_files in

    let local = Filename.basename prefix in
    
    (* All normal forms view *)
    let html_ch = open_out (sprintf "%s.html" prefix) in

    let title = sprintf "Sentence: %s --- %d Normal form%s" local l (if l>1 then "s" else "") in
    let () = Html.enter html_ch ~title ?header prefix in

    if init_graph
    then
      begin
        fprintf html_ch "<h6>Initial graph</h6>\n";
        fprintf html_ch "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>\n" local
      end;
    
    List_.iteri 
      (fun i (rules_list,file_name) -> 
        fprintf html_ch "<h6>Solution %d</h6>\n" (i+1);

        let local_name = Filename.basename file_name in
        
        (* the png file *)
        fprintf html_ch "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>\n" local_name;

        (* the modules list *)
        fprintf html_ch "<b>Modules applied</b>: %d<br/>\n" (List.length rules_list);
        
        let id = sprintf "id_%d" (i+1) in
        
        (* fprintf html_ch "<a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s').style.display == 'none') { document.getElementById('%s').style.display = 'block'; document.getElementById('p_%s').innerHTML = 'Hide applied rules'; } else { document.getElementById('%s').style.display = 'none';; document.getElementById('p_%s').innerHTML = 'Show applied rules'; }\"><b><p id=\"p_%s\">Show applied rules</p></b></a>\n" id id id id id id; *)

        fprintf html_ch "<a style=\"cursor:pointer;\"\n";
        fprintf html_ch "  onClick=\"if (document.getElementById('%s').style.display == 'none')\n" id;
        fprintf html_ch "      { document.getElementById('%s').style.display = 'block'; document.getElementById('p_%s').innerHTML = 'Hide applied rules'; }\n" id id;
        fprintf html_ch " else { document.getElementById('%s').style.display = 'none';; document.getElementById('p_%s').innerHTML = 'Show applied rules'; }\">" id id;
        fprintf html_ch "         <b><p id=\"p_%s\">Show applied rules</p></b>\n" id;
        fprintf html_ch "</a>\n";

        fprintf html_ch " <div id=\"%s\" style=\"display:none;\">\n" id;

        List.iter 
          (fun (mod_name,rules) -> 
            fprintf html_ch "<p><b><font color=\"red\">%s: </font></b><font color=\"green\">%s</font></p>\n" 
              mod_name
              (List_.to_string (fun x -> x) ", " rules);
          )
          rules_list;
        fprintf html_ch " </div>\n"

        
      ) nf_files;
    Html.leave html_ch;
    close_out html_ch
ENDIF
end




module Modul = struct
  type t = {
      name: string;
      local_labels: (string * string option) array;
      bad_labels: Label.t list;
      rules: Rule.t list;
      filters: Rule.t list;
      confluent: bool;
      loc: Loc.t;
    }

  let check t =
    (* check for duplicate rules *)
    let rec loop already_defined = function
      | [] -> ()
      | r::_ when List.mem (Rule.get_name r) already_defined -> 
          Error.build ~loc:(Rule.get_loc r) "Rule '%s' is defined twice in the same module" (Rule.get_name r)
      | r::tail -> loop ((Rule.get_name r) :: already_defined) tail in
    loop [] t.rules

  let build ast_module =
    let locals = Array.of_list ast_module.Ast.local_labels in
    Array.sort compare locals;
    let rules_or_filters = List.map (Rule.build ~locals ast_module.Ast.mod_dir) ast_module.Ast.rules in
    let (filters, rules) = List.partition Rule.is_filter rules_or_filters in 
    let modul = 
      {
       name = ast_module.Ast.module_id;
       local_labels = locals; 
       bad_labels = List.map Label.from_string ast_module.Ast.bad_labels;
       rules = rules; 
       filters = filters;
       confluent = ast_module.Ast.confluent;
       loc = ast_module.Ast.mod_loc;
     } in
    check modul; modul
end

module Sequence = struct
  type t = {
      name: string;
      def: string list;
      loc: Loc.t;
    }

  let check module_list t =
    List.iter
      (fun module_name -> 
        if not (List.exists (fun modul -> modul.Modul.name = module_name) module_list)
        then Error.build ~loc:t.loc "sequence \"%s\", refers to the unknown module \"%s\"."
            t.name module_name
      ) t.def

  let build module_list ast_sequence = 
    let sequence = 
      {
       name = ast_sequence.Ast.seq_name;
       def = ast_sequence.Ast.seq_mod;
       loc = ast_sequence.Ast.seq_loc;
     } in
    check module_list sequence; sequence
end

module Grs = struct
  type sequence = string * string list (* (name of the seq, list of modules) *)
        
  type t = {
      labels: Label.t list;    (* the list of global edge labels *)
      modules: Modul.t list;          (* the ordered list of modules used from rewriting *)
      sequences: Sequence.t list;
    }
        
  let sequence_names t = List.map (fun s -> s.Sequence.name) t.sequences

  let empty = {labels=[]; modules=[]; sequences=[];}

  let check t =
    (* check for duplicate modules *)
    let rec loop already_defined = function
      | [] -> ()
      | m::_ when List.mem m.Modul.name already_defined -> 
          Error.build ~loc:m.Modul.loc "Module '%s' is defined twice" m.Modul.name
      | m::tail -> loop (m.Modul.name :: already_defined) tail in
    loop [] t.modules;

    (* check for duplicate sequences *)
    let rec loop already_defined = function
      | [] -> ()
      | s::_ when List.mem s.Sequence.name already_defined -> 
          Error.build ~loc:s.Sequence.loc "Sequence '%s' is defined twice" s.Sequence.name
      | s::tail -> loop (s.Sequence.name :: already_defined) tail in
    loop [] t.sequences

  let build ast_grs =
    Label.init ast_grs.Ast.labels;
    Domain.init ast_grs.Ast.domain;
    let modules = List.map Modul.build ast_grs.Ast.modules in
    let grs = {
      labels = List.map (fun (l,_) -> Label.from_string l) ast_grs.Ast.labels;
      modules = modules;
      sequences = List.map (Sequence.build modules) ast_grs.Ast.sequences;
    } in
    check grs; grs

  let modules_of_sequence grs sequence =
    let module_names =
      try 
        let seq = List.find (fun s -> s.Sequence.name = sequence) grs.sequences in
        seq.Sequence.def
      with Not_found -> [sequence] in (* a module name can be used as a singleton sequence *)

    List.map 
      (fun name -> 
        try List.find (fun m -> m.Modul.name=name) grs.modules 
        with Not_found -> Log.fcritical "No sequence or module named '%s'" name
      )
      module_names

  let rewrite grs sequence instance = 
    let modules_to_apply = modules_of_sequence grs sequence in
    
    let rec loop instance = function
      | [] -> (* no more modules to apply *) 
          {Rewrite_history.instance = instance; module_name = ""; good_nf = []; bad_nf = []; }
      | next::tail -> 
          let (good_set, bad_set) = 
            Rule.normalize
              ~confluent: next.Modul.confluent
              next.Modul.rules 
              next.Modul.filters
              (Instance.clear instance) in
          let good_list = Instance_set.elements good_set 
          and bad_list = Instance_set.elements bad_set in
          {
           Rewrite_history.instance = instance; 
           module_name = next.Modul.name;
           good_nf = List.map (fun i -> loop i tail) good_list;
           bad_nf = bad_list;
         } in
    loop instance modules_to_apply
      
  let build_rew_display grs sequence instance =
    let modules_to_apply = modules_of_sequence grs sequence in

    let rec loop instance = function
      | [] -> Grew_types.Leaf instance.Instance.graph
      | next :: tail ->
          let (good_set, bad_set) =
            Rule.normalize
              ~confluent: next.Modul.confluent
              next.Modul.rules
              next.Modul.filters
              (Instance.clear instance) in
          let inst_list = Instance_set.elements good_set
              (* and bad_list = Instance_set.elements bad_set *) in

          match inst_list with
          | [{Instance.big_step = None}] ->
              Grew_types.Local_normal_form (instance.Instance.graph, next.Modul.name, loop instance tail)
          | _ -> Grew_types.Node
                (
                 instance.Instance.graph,
                 next.Modul.name,
                 List.map
                   (fun inst ->
                     match inst.Instance.big_step with
                     | None -> Error.bug "Cannot have no big_steps and more than one reducts at the same time"
                     | Some bs -> (bs, loop inst tail)
                   ) inst_list
                )
    in loop instance modules_to_apply

end  

module Gr_stat = struct

  (** the type [gr] stores the stats for the rewriting of one gr file *)
  type t = 
    | Stat of (int StringMap.t * int option)  (* map: rule_name |-> max occ, number of solution *) 
    | Error of string

  let add_one_module modul_opt rules stat =
    match modul_opt with
    | Some modul ->
        List.fold_left
          (fun acc rule ->
            let key = sprintf "%s.%s" modul rule in
            let old = try StringMap.find key acc with Not_found -> 0 in
            StringMap.add key (old+1) acc
          ) stat rules
    | None when rules = [] -> stat
    | None -> Log.fcritical "Unconsistent rewrite history"
          
  let max_stat stat1 stat2 =
    StringMap.fold
      (fun key value acc ->
        let old = try StringMap.find key acc with Not_found -> 0 in
        StringMap.add key (max old value) acc
      ) stat1 stat2
      
  let from_rew_history rew_history =
    let rec loop prev_module rh =
      let sub_stat = 
        match List.map (loop (Some rh.Rewrite_history.module_name)) rh.Rewrite_history.good_nf with 
        | [] -> StringMap.empty
        | h::t -> List.fold_left max_stat h t in
      add_one_module prev_module rh.Rewrite_history.instance.Instance.rules sub_stat
    in Stat (loop None rew_history, Some (Rewrite_history.num_sol rew_history))

  let save stat_file t =
    let out_ch = open_out stat_file in
    (match t with
    | Error msg -> fprintf out_ch "ERROR\n%s" msg 
    | Stat (map, num) ->
        (match num with None -> () | Some n -> fprintf out_ch "NUM_SOL:%d\n%!" n);
        StringMap.iter (fun rule_name occ -> fprintf out_ch "%s:%d\n%!" rule_name occ) map);
    close_out out_ch

  let load stat_file = 
    let sol = ref None in
    try
      let lines = File.read stat_file in
      match lines with
      | "ERROR" :: msg_lines -> Error (List_.to_string (fun x->x) "\n" msg_lines)
      | _ -> 
          let map =
            List.fold_left 
              (fun acc line ->
                match Str.split (Str.regexp ":") line with
                | ["NUM_SOL"; num] -> sol := Some (int_of_string num); acc 
                | [modu_rule; num] -> StringMap.add modu_rule (int_of_string num) acc
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
      modules: Modul.t list;                             (* ordered list of modules in the sequence *)
      map: (int * StringSet.t) StringMap.t StringMap.t;  (* map: see above *)
      amb: StringSet.t IntMap.t;                         (* key: nb of sols |-> set: sentence concerned *)
      error: (string * string) list;                     (* (file, msg) *)
      num: int;                                          (* an integer id relative to the corpus *)
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
                  StringMap.add (Rule.get_name rule) (0,StringSet.empty) acc2
                ) StringMap.empty modul.Modul.rules in
            StringMap.add modul.Modul.name rule_map acc
          else acc
        ) StringMap.empty grs.Grs.modules in
    { modules=modules; map = map; amb = IntMap.empty; error = []; num = 0 }
      
  let add modul rule file num map = 
    let old_rule_map = StringMap.find modul map in
    let (old_num, old_file_set) = StringMap.find rule old_rule_map in
    StringMap.add 
      modul 
      (StringMap.add
         rule
         (old_num + num, StringSet.add file old_file_set)
         old_rule_map
      ) map
        
  let add_gr_stat base_name gr_stat t = 
    match gr_stat with
    | Gr_stat.Error msg -> { t with error = (base_name, msg) :: t.error; num = t.num+1 }
    | Gr_stat.Stat (map, sol_opt) -> 
        let new_map = 
          StringMap.fold
            (fun modul_rule num_occ acc ->
              match Str.split (Str.regexp "\\.") modul_rule with
              | [modul; rule] -> add modul rule base_name num_occ acc
              | _ -> Log.fcritical "illegal modul_rule spec \"%s\"" modul_rule 
            ) map t.map in
        let new_amb = 
          match sol_opt with 
          | None -> t.amb
          | Some sol -> 
              let old = try IntMap.find sol t.amb with Not_found -> StringSet.empty in
              IntMap.add sol (StringSet.add base_name old) t.amb in
        { t with map = new_map; num = t.num+1; amb=new_amb; }

  let save_html ~title ~grs_file ~html ~output_dir t =

    let ratio nb = (float nb) /. (float t.num) *. 100. in

    let out_ch = open_out (Filename.concat output_dir "index.html") in
    
    let css = "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">" in
    
    ignore(Sys.command("cp "^(Filename.concat DATA_DIR "style.css")^" "^(Filename.concat output_dir "style.css")));
    
    fprintf out_ch "<head>\n%s\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" /></head>\n" css title;
    fprintf out_ch "<h1>%s</h1>\n" title;
    fprintf out_ch "<b>Grs file</b>:<a href =\"%s\">%s</a>\n<br/>\n" (Filename.basename grs_file) (Filename.basename grs_file);
    fprintf out_ch "<b>%d Sentences</b><br/>\n<br/>\n" t.num;

    fprintf out_ch "<center><table cellpadding=3 cellspacing=0 width=95%%>\n";
    List.iter
      (fun modul ->
        let modul = modul.Modul.name in
        let rules = StringMap.find modul t.map in
        fprintf out_ch "<tr><td colspan=\"5\" style=\"padding: 0px;\"><h6>Module %s</h6></td>\n" modul;
        fprintf out_ch "<tr><th class=\"first\">Rule</th><th>#occ</th><th>#files</th><th>Ratio</th><th>Files</th></tr>\n";
        let (tot_occ, full_sent) = 
          StringMap.fold
            (fun _ (occ_num, file_set) (acc_occ, acc_sent) -> (acc_occ + occ_num, StringSet.union acc_sent file_set))
            rules (0,StringSet.empty) in
        let tot_sent = StringSet.cardinal full_sent in
        fprintf out_ch "<tr>\n";
        fprintf out_ch "<td class=\"first_total\">Total for module</td>\n";
        fprintf out_ch "<td class=\"total\">%d</td>" tot_occ;
        fprintf out_ch "<td class=\"total\">%d</td>" tot_sent;
        fprintf out_ch "<td class=\"total\">%.2f%%</td>" (ratio tot_sent);
        fprintf out_ch "<td class=\"total\">&nbsp;</td>\n";
        fprintf out_ch "</tr>\n";

        StringMap.iter
          (fun rule (occ_num, file_set) ->
            let file_list = StringSet.elements file_set in

            let tmp = ref "" in
            let counter = ref 0 in
            let rec compute list = match list with
            | [] -> ()
            | h::[] ->
                if (!counter = 10) then (
                  tmp := sprintf "%s<div id=\"%s_%s\" style=\"display:none;\">\n" !tmp modul rule
                 );
                incr counter;
                if html 
                then tmp := sprintf "%s\n    <a href=\"%s.html\">%s</a> &nbsp; &nbsp;" !tmp h h
                else tmp := sprintf "%s\n    %s &nbsp; &nbsp;" !tmp h
            | h::t ->
                if (not (List.mem h t)) then ( (*avoid doublons*)
                  if (!counter = 10) then (
                    tmp := sprintf "%s<div id=\"%s_%s\" style=\"display:none;\">\n" !tmp modul rule
                   );
                  incr counter;
                  if html 
                  then tmp := sprintf "%s\n    <a href=\"%s.html\">%s</a>&nbsp;&nbsp;" !tmp h h 
                  else tmp := sprintf "%s\n    %s&nbsp;&nbsp;" !tmp h
                 );
                compute t
            in compute file_list;

            if file_list = [] then tmp := "&nbsp;";

            let file_num = List.length file_list in

            fprintf out_ch "<tr>\n";
            fprintf out_ch "<td class=\"first_stats\"  valign=top>%s</td>\n" rule;
            fprintf out_ch "<td class=\"stats\"  valign=top>%d</td>\n" occ_num;
            fprintf out_ch "<td class=\"stats\"  valign=top>%d</td>\n" file_num;
            fprintf out_ch "<td class=\"stats\"  valign=top>%.2f%%</td>\n" (ratio file_num);
            
            fprintf out_ch "<td class=\"stats\">%s" !tmp;
            if (!counter > 10)
            then (
              fprintf out_ch "</div><a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s_%s').style.display == 'none') { %s } else { %s }\"><b><p id=\"p_%s_%s\">+ Show all +</p></b></a>\n"
                modul rule
                (sprintf "document.getElementById('%s_%s').style.display = 'block'; document.getElementById('p_%s_%s').innerHTML = '- Show first ten -';" modul rule modul rule)
                (sprintf "document.getElementById('%s_%s').style.display = 'none';; document.getElementById('p_%s_%s').innerHTML = '+ Show all +';" modul rule modul rule)
                modul rule;
             );
            fprintf out_ch "</td></tr>\n";
          ) rules;
      ) t.modules;


    (* add a subtlabe for sentence ambiguity *)
    if not (IntMap.is_empty t.amb)
    then
      begin
        fprintf out_ch "<tr><td colspan=5><h6>Rewriting ambiguity</h6></td>\n";
        fprintf out_ch "<tr><th class=\"first\" >Number of normal forms</th><th colspan=2 width=20>#files</th><th >Ratio</th><th>Files</th></tr>\n";
        
        IntMap.iter
          (fun num set -> 
            let num_files = StringSet.cardinal set in
            fprintf out_ch "<tr>\n";
            fprintf out_ch "<td class=\"first_stats\">%d</td>\n" num;
            fprintf out_ch "<td class=\"stats\" colspan=2>%d</td>\n" num_files;
            fprintf out_ch "<td class=\"stats\">%.2f%%</td>\n" (ratio num_files);
            fprintf out_ch "<td class=\"stats\">";
            
            StringSet.iter
              (fun (file) ->
                if html 
                then fprintf out_ch "<a href=\"%s.html\">%s</a><br/>" file file
                else fprintf out_ch "%s<br/>" file
              ) set;
                
            fprintf out_ch "</td>\n";
            fprintf out_ch "</tr>") t.amb
      end;
    
    fprintf out_ch "</table></center>\n";


    (* add a subtlabe for sentence that produces an error *)
    (match List.length t.error with
    | 0 -> ()
    | nb_errors ->
        fprintf out_ch "<tr><td colspan=5><h6>ERRORS</h6></td>\n";
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
                if html 
                then fprintf out_ch "<a href=\"%s.html\">%s</a>: %s<br/>" file file err
                else fprintf out_ch "%s: %s<br/>" file err
              ) (List.rev l);
            
            fprintf out_ch "</td>\n";
            fprintf out_ch "</tr>");

    fprintf out_ch "</table></center>\n";


    close_out out_ch;
    ()
end (* module Stat *)
