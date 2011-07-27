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
    
    let stats = ref [] in
    
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
        
        let id =sprintf "id_%d" (i+1) in
        
        fprintf html_ch "<a style=\"cursor:pointer;\" onClick=\"if (document.getElementById('%s').style.display == 'none') { document.getElementById('%s').style.display = 'block'; document.getElementById('p_%s').innerHTML = 'Hide'; } else { document.getElementById('%s').style.display = 'none';; document.getElementById('p_%s').innerHTML = 'Show'; }\"><b><p id=\"p_%s\">Show</p></b></a>\n" id id id id id id;

        fprintf html_ch " <div id=\"%s\" style=\"display:none;\">\n" id;

        List.iter 
          (fun (mod_name,rules) -> 
            fprintf html_ch "<p><b><font color=\"red\">%s: </font></b><font color=\"green\">%s</font></p>\n" 
              mod_name
              (List_.to_string (fun x -> x) ", " rules);
            stats := (mod_name,rules)::(!stats)
          )
          rules_list;
        fprintf html_ch " </div>\n"

        
      ) nf_files;
    Html.leave html_ch;
    close_out html_ch;


    List.rev !stats
      
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

  let rewrite grs sequence instance = 
    let module_names_to_apply = 
      try List.assoc sequence grs.sequences 
      with Not_found -> [sequence] in
    
    let modules_to_apply = 
      List.map 
        (fun name -> 
          try List.find (fun m -> m.Modul.name=name) grs.modules 
          with Not_found -> Log.fcritical "No sequence or module named '%s'" name
        )
        module_names_to_apply in
    
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
         }
    in loop instance modules_to_apply

  let build_rew_display grs sequence instance = 
    let module_names_to_apply = 
      try List.assoc sequence grs.sequences 
      with Not_found -> [sequence] in
    
    let modules_to_apply = 
      List.map 
        (fun name -> 
          try List.find (fun m -> m.Modul.name=name) grs.modules 
          with Not_found -> Log.fcritical "No sequence or module named '%s'" name
        )
        module_names_to_apply in
    
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

