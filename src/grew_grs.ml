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
    | { good_nf = []; bad_nf = [] } -> 1
    | { good_nf = [] } -> 0 (* dead branch *)
    | { good_nf = l} -> List.fold_left (fun acc t -> acc + (num_sol t)) 0 l


  (** [save_nfs ?main_feat base_name t] does two things:
      - write PNG files of normal forms
      - returns a list of couples (rules, file)
  *)
  let save_nfs ?main_feat ~dot base_name t =
    let rec loop file_name rules t =
      match (t.good_nf, t.bad_nf) with
        | [],[] when dot -> Instance.save_dot_png ?main_feat file_name t.instance; [rules, file_name]
        | [],[] -> Instance.save_dep_png ?main_feat file_name t.instance; [rules, file_name]
        | [],_ -> []
        | l, _ ->
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


  let error_html ?main_feat ?(dot=false) ?(init_graph=true) ?header prefix msg inst_opt =
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

  let save_gr base t =
    let rec loop file_name t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_gr t.instance) (file_name^".gr")
        | l, _ -> List_.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_html ?main_feat ?(dot=false) ?(init_graph=true) ?(out_gr=false)  ?header ~graph_file prefix t =
    (* remove files from previous runs *)
    let _ = Unix.system (sprintf "rm -f %s*.html" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.dep" prefix) in
    let _ = Unix.system (sprintf "rm -f %s*.png" prefix) in

    (if init_graph then Instance.save_dep_png ?main_feat prefix t.instance);

    let nf_files = save_nfs ?main_feat ~dot prefix t in

    let l = List.length nf_files in

    let local = Filename.basename prefix in

    (* All normal forms view *)
    let html_ch = open_out (sprintf "%s.html" prefix) in

    let title = sprintf "Sentence: %s --- %d Normal form%s" local l (if l>1 then "s" else "") in
    let () = Html.enter html_ch ~title ?header prefix in

    fprintf html_ch "<b>Input file</b>: <a href=\"%s\">%s</a><br/>\n"
      graph_file (Filename.basename graph_file);

    fprintf html_ch "<b>Input sentence</b>: <font color=\"green\"><i>%s</i></font></p><br/>\n"
      (G_graph.to_sentence ?main_feat t.instance.Instance.graph);

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

        if out_gr
        then fprintf html_ch "<p><a href=\"%s.gr\">gr file</a>\n" local_name;

        (* the png file *)
        fprintf html_ch "<div width=100%% style=\"overflow-x:auto\"><IMG SRC=\"%s.png\"></div>\n" local_name;

        (* the modules list *)
        fprintf html_ch "<b>Modules applied</b>: %d<br/>\n" (List.length rules_list);

        let id = sprintf "id_%d" (i+1) in

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
end




module Modul = struct
  type t = {
    name: string;
    local_labels: (string * string option) array;
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

  type t = {
    labels: Label.t list;    (* the list of global edge labels *)
    modules: Modul.t list;          (* the ordered list of modules used from rewriting *)
    sequences: Sequence.t list;
  }

  let get_modules t = t.modules

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
            next.Modul.name
            ~confluent: next.Modul.confluent
            next.Modul.rules
            next.Modul.filters
            (Instance.flatten instance) in
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
            next.Modul.name
            ~confluent: next.Modul.confluent
            next.Modul.rules
            next.Modul.filters
            (Instance.flatten instance) in
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

  let rule_iter fct grs =
    List.iter
      (fun modul ->
        List.iter (fun rule -> fct modul.Modul.name rule) modul.Modul.rules
      ) grs.modules

  let filter_iter fct grs =
    List.iter
      (fun modul ->
        List.iter (fun filter -> fct modul.Modul.name filter) modul.Modul.filters
      ) grs.modules
end
