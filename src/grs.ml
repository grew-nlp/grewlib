open Printf
open Log

open Utils
open Rule
open Command
open Grew_edge 
open Graph


module Rewrite_history = struct

  type t = {
      instance: Instance.t;
      module_name: string; 
      good_nf: t list; 
      bad_nf: Instance.t list;
    }

  let rec get_nfs t =
    match t.good_nf with 
    | [] -> [([], t.instance)]
    | l -> 
        List.flatten
          (List_.mapi 
             (fun i t' ->
               List.map 
                 (fun (path,x) -> (i::path,x)) 
                 (get_nfs t') 
             ) l
          )

  let rec rules t = 
    let local = (t.module_name, t.instance.Instance.rules) in
    match t.good_nf with 
    | [] -> [local]
    | l -> local :: (List.flatten (List.map rules l))


IFDEF DEP2PICT THEN
  (* warning: path are returned in reverse order *)
  let save_all_dep ?main_feat ?(init_graph=true) base_name t = 
    let nfs = ref [] in
    let rec loop first (rev_path, rev_rules) t =
      let file = 
        match List.rev rev_path with
        | [] -> base_name 
        | l -> sprintf "%s_%s" base_name (List_.to_string string_of_int "_" l) in

      begin
        match (first, init_graph) with
        | (true, true)
          -> Instance.save_dep_png ?main_feat file t.instance
        | _ when t.good_nf = []  (* t is a leaf of the tree history *)
          -> Instance.save_dep_png ?main_feat file t.instance
        | _ -> ()
      end;
      
      match t.good_nf with
      | [] -> nfs := (rev_path,List.rev rev_rules,file) :: !nfs
      | l ->
          List_.iteri 
            (fun i t' ->
              loop false (i::rev_path,(t.module_name, t'.instance.Instance.rules)::rev_rules) t'
            ) l in
    loop true ([],[]) t;
    List.rev !nfs
      
  let save_html ?main_feat ?(init_graph=true) ?header prefix t =
    (* remove files from previous runs *)
    let _ = Unix.system (sprintf "rm -f %s*.html" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.dep" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.png" prefix) in
    
    let nf_files = save_all_dep ?main_feat ~init_graph prefix t in
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
      (fun i (_,rules_list,file_name) -> 
        fprintf html_ch "<h6>Solution %d</h6>\n" (i+1);

        let local_name = Filename.basename file_name in
        
        (* the png file *)
        fprintf html_ch "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>\n" local_name;

        (* the modules list *)
        fprintf html_ch "<b>Modules applied</b>: %d<br/>\n" (List.length rules_list);
        
        let id = sprintf "id_%d" (i+1) in
        
        fprintf html_ch "<a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s').style.display == 'none') { document.getElementById('%s').style.display = 'block'; document.getElementById('p_%s').innerHTML = 'Hide'; } else { document.getElementById('%s').style.display = 'none';; document.getElementById('p_%s').innerHTML = 'Show'; }\"><b><p id=\"p_%s\">Show</p></b></a>\n" id id id id id id;

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
      confluent: bool;
    }


  let build ?domain ast_module =
    let locals = Array.of_list ast_module.Ast.local_labels in
    Array.sort compare locals;
    {
     name = ast_module.Ast.module_id;
     local_labels = locals; 
     bad_labels = List.map Label.from_string ast_module.Ast.bad_labels;
     rules = List.map (Rule.build ?domain ~locals) ast_module.Ast.rules;
     confluent = ast_module.Ast.confluent;
   }
end

module Grs = struct
  type sequence = string * string list (* (name of the seq, list of modules) *)
        
  type t = {
      labels: Label.t list;    (* the list of global edge labels *)
      modules: Modul.t list;          (* the ordered list of modules used from rewriting *)
      sequences: sequence list;
    }
        
  let sequences t = t.sequences
      
  let empty = {labels=[]; modules=[]; sequences=[];}

  let build ast_grs =
    Label.init ast_grs.Ast.labels; 
    {
     labels = List.map (fun (l,_) -> Label.from_string l) ast_grs.Ast.labels;
     modules = List.map (Modul.build ~domain:ast_grs.Ast.domain) ast_grs.Ast.modules;
     sequences = List.map (fun s -> (s.Ast.seq_name, s.Ast.seq_mod)) ast_grs.Ast.sequences;
   }

  let modules_of_sequence grs sequence =
    let module_names = 
      try List.assoc sequence grs.sequences 
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
              (fun x -> true)  (* FIXME *)
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
              (fun x -> true)  (* FIXME: filtering in module outputs *)
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
  type t = int StringMap.t 

  let add_one_module modul_opt rules stat =
    match modul_opt with
    | Some modul ->
(try
        List.fold_left
          (fun acc rule ->
            let key = sprintf "%s.%s" modul rule in
            let old = try StringMap.find key acc with Not_found -> 0 in
            StringMap.add key (old+1) acc
          ) stat rules
with Not_found -> 
  Log.fcritical "Gr_stat.add modul='%s' rules='%s'" 
    modul 
    (List.fold_left (fun acc r -> acc^"#"^r) "" rules)
)
    | None when rules = [] -> stat
    | None -> Log.fcritical "Unconsistent rewrite history"

  let max_stat stat1 stat2 =
    StringMap.fold
      (fun key value acc ->
        let old = try StringMap.find key acc with Not_found -> 0 in
        StringMap.add key (max old value) acc
      ) stat1 stat2
      
  (* let rec from_rew_history t = *)
  (*   let sub_stat =  *)
  (*     match List.map from_rew_history t.Rewrite_history.good_nf with  *)
  (*     | [] -> StringMap.empty *)
  (*     | h::t -> List.fold_left max_stat h t in *)
  (*   add_one_module t.Rewrite_history.module_name t.Rewrite_history.instance.Instance.rules sub_stat *)


  let from_rew_history rew_history =
    let rec loop prev_module rh =
      let sub_stat = 
        match List.map (loop (Some rh.Rewrite_history.module_name)) rh.Rewrite_history.good_nf with 
        | [] -> StringMap.empty
        | h::t -> List.fold_left max_stat h t in
      add_one_module prev_module rh.Rewrite_history.instance.Instance.rules sub_stat
    in loop None rew_history


  let save stat_file t = 
    let out_ch = open_out stat_file in
    StringMap.iter (fun rule_name occ -> fprintf out_ch "%s:%d\n%!" rule_name occ) t;
    close_out out_ch

  let load stat_file =
    List.fold_left 
      (fun acc line ->
        match Str.split (Str.regexp ":") line with
        | [modu_rule; num] -> StringMap.add modu_rule (int_of_string num) acc
        | _ -> Log.fcritical "invalid stat line: %s" line
      ) StringMap.empty (File.read stat_file)
end (* module Gr_stat *)

module Corpus_stat = struct
  (** the [t] type stores stats for a corpus of gr_files *)
  (* 
     first key: [m] module name
     second key: [r] rule name
     value: [occ_nul, file_list] the totat number of rule applications and the set of gr files concerned *)
  type t = {
      map: (int * StringSet.t) StringMap.t StringMap.t;
      num: int;
    }

  let empty grs =
    let map = List.fold_left 
        (fun acc modul ->
          let rule_map = 
            List.fold_left
              (fun acc2 rule ->
              StringMap.add (Rule.get_name rule) (0,StringSet.empty) acc2
              ) StringMap.empty modul.Modul.rules in
          StringMap.add modul.Modul.name rule_map acc
        ) StringMap.empty grs.Grs.modules in
    { map = map; num = 0 }
        
  let add modul rule file num map = 
try
    let old_rule_map = StringMap.find modul map in
    let (old_num, old_file_set) = StringMap.find rule old_rule_map in
    StringMap.add 
      modul 
      (StringMap.add
         rule
         (old_num + num, StringSet.add file old_file_set)
         old_rule_map
      ) map
with Not_found -> Log.fcritical "Corpus_stat.add modul='%s' rule='%s' file='%s'" modul rule file
   
  let add_gr_stat base_name gr_stat t = 
    let new_map = 
      StringMap.fold
        (fun modul_rule num_occ acc ->
          match Str.split (Str.regexp "\\.") modul_rule with
          | [modul; rule] -> add modul rule base_name num_occ acc
          | _ -> Log.fcritical "illegal modul_rule spec \"%s\"" modul_rule 
        ) gr_stat t.map in
    { map = new_map; num = t.num+1 }
      



  let save_html ~title ~grs_file ~html ~output_dir t =

    let ratio nb = (float nb) /. (float t.num) *. 100. in


    let out_ch = open_out (Filename.concat output_dir "index.html") in
    
    let css = "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">" in
    
    ignore(Sys.command("cp "^(Filename.concat DATA_DIR "style.css")^" "^(Filename.concat output_dir "style.css")));
    
    fprintf out_ch "<head>\n%s\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" /></head>\n" css title;
    fprintf out_ch "<h1>%s</h1>\n" title;
    fprintf out_ch "<b>Grs file</b>:<a href =\"%s\">%s</a>\n<br/>\n" grs_file (Filename.basename grs_file);
    fprintf out_ch "<b>%d Sentences</b><br/>\n<br/>\n" t.num;

    fprintf out_ch "<center><table cellpadding=10 cellspacing=0 width=90%%>\n";
    StringMap.iter
      (fun modul rules ->
        fprintf out_ch "<tr><td colspan=\"5\" style=\"padding: 0px;\"><h6>Module %s</h6></td>\n" modul;
        fprintf out_ch "<tr><th class=\"first\" width=10>Rule</th><th width=10>#occ</th><th width=10>#files</th><th width=10>Ratio</th><th width=10>Files</th></tr>\n";
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
                then tmp := sprintf "%s<a href=\"%s.html\">%s</a>" !tmp h h
                else tmp := sprintf "%s%s" !tmp h
            | h::t ->
                if (not (List.mem h t)) then ( (*avoid doublons*)
                  if (!counter = 10) then (
                    tmp := sprintf "%s<div id=\"%s_%s\" style=\"display:none;\">\n" !tmp modul rule
                   );
                  incr counter;
                  if html 
                  then tmp := sprintf "%s<a href=\"%s.html\">%s</a>" !tmp h h
                  else tmp := sprintf "%s%s" !tmp h
                 );
                compute t
            in compute (List.rev file_list);

            if file_list = [] then tmp := "&nbsp;";

            let file_num = List.length file_list in

            fprintf out_ch "<tr>\n";
            fprintf out_ch "<td class=\"first_stats\" width=10 valign=top>%s</td>\n" rule;
            fprintf out_ch "<td class=\"stats\" width=10 valign=top>%d</td>\n" occ_num;
            fprintf out_ch "<td class=\"stats\" width=10 valign=top>%d</td>\n" file_num;
            fprintf out_ch "<td class=\"stats\" width=10 valign=top>%.2f%%</td>\n" (ratio file_num);
            
            fprintf out_ch "<td class=\"stats\">%s" !tmp;
            if (!counter > 10)
            then (
              fprintf out_ch "</div><a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s_%s').style.display == 'none') { %s } else { %s }\"><b><p id=\"p_%s_%s\">+ Show more +</p></b></a>\n"
                modul rule
                (sprintf "document.getElementById('%s_%s').style.display = 'block'; document.getElementById('p_%s_%s').innerHTML = '- Show less -';" modul rule modul rule)
                (sprintf "document.getElementById('%s_%s').style.display = 'none';; document.getElementById('p_%s_%s').innerHTML = '+ Show more +';" modul rule modul rule)
                modul rule;
             );
            fprintf out_ch "</td></tr>\n";
          ) rules;
      ) t.map;

(* FIXME error in index.html *)
    (* (\* add a subtalbe for sentence that produces an error *\) *)
    (* let nb_errors = List.length !errors in *)
    (* fprintf out_ch "<tr><td colspan=5><h6>ERRORS</h6></td>\n"; *)
    (* fprintf out_ch "<tr><th class=\"first\" width=10>Rule</th><th colspan=2 width=20>#files</th><th width=10>Ratio</th><th>Files</th></tr>\n"; *)

    (* fprintf out_ch "<tr>\n"; *)
    (* fprintf out_ch "<td class=\"first_stats\">Errors</td>\n"; *)
    (* fprintf out_ch "<td class=\"stats\" colspan=2>%d</td>\n" nb_errors; *)
    (* fprintf out_ch "<td class=\"stats\">%.2f%%</td>\n" (ratio nb_errors); *)
    (* fprintf out_ch "<td class=\"stats\">"; *)
    (* List.iter *)
    (*   (fun err -> *)
    (*     fprintf out_ch "<a href=\"%s.html\">%s</a><br/>" (Filename.chop_extension err) (Filename.chop_extension err) *)
    (*   ) (List.rev !errors); *)
    (* fprintf out_ch "</td>\n"; *)
    (* fprintf out_ch "</tr>"; *)

    fprintf out_ch "</table></center>\n";

    close_out out_ch;
    ()


end (* module Stat *)
