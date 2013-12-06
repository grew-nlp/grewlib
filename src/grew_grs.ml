open Printf
open Log

open Grew_fs
open Libgrew_utils
open Grew_ast
open Grew_edge
open Grew_command
open Grew_graph
open Grew_rule
open Grew_parser

(* ==================================================================================================== *)
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

  let save_nfs ?filter ?main_feat ~dot base_name t =
    let rec loop file_name rules t =
      match (t.good_nf, t.bad_nf) with
        | [],[] when dot -> Instance.save_dot_png ?filter ?main_feat file_name t.instance; [rules, file_name]
        | [],[] -> ignore (Instance.save_dep_png ?filter ?main_feat file_name t.instance); [rules, file_name]
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

  let save_gr base t =
    let rec loop file_name t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_gr t.instance) (file_name^".gr")
        | l, _ -> List_.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  let save_conll base t =
    let rec loop file_name t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_conll t.instance) (file_name^".conll")
        | l, _ -> List_.iteri (fun i son -> loop (sprintf "%s_%d" file_name i) son) l
    in loop base t

  (* suppose that all modules are confluent and produced exacly one normal form *)
  let save_det_gr base t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] -> File.write (Instance.to_gr t.instance) (base^".gr")
        | [one], [] -> loop one
        | _ -> Error.run "Not a single rewriting"
    in loop t

  let save_annot out_dir base_name t =
    List_.mapi
      (fun i alts ->
        match alts.good_nf with
      | [alt_1; alt_2] ->
        let a = sprintf "%s_%d_A" base_name i in
        let b = sprintf "%s_%d_B" base_name i in
        let hpa = Instance.save_dep_svg (Filename.concat out_dir a) alt_1.instance in
        let hpb = Instance.save_dep_svg (Filename.concat out_dir b) alt_2.instance in
        let (afn,afv,apos) = G_graph.get_annot_info alt_1.instance.Instance.graph
        and (bfn,bfv,bpos) = G_graph.get_annot_info alt_2.instance.Instance.graph in
        (base_name,i,(afn,afv,apos),(bfn,bfv,bpos),(hpa,hpb))
      | _ -> Error.run "Not two alternatives in an annotation rewriting in %s" base_name
      ) t.good_nf

  let save_det_conll ?header base t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | ([],[]) ->
          let output =
            match header with
              | Some h -> sprintf "%% %s\n%s" h (Instance.to_conll t.instance)
              | None -> Instance.to_conll t.instance in
          File.write output (base^".conll")
        | ([one], []) -> loop one
        | _ -> Error.run "Not a single rewriting"
    in loop t

  let det_dep_string t =
    let rec loop t =
      match (t.good_nf, t.bad_nf) with
        | [],[] ->
          let graph = t.instance.Instance.graph in
          Some (G_graph.to_dep graph)
        | [one], [] -> loop one
        | _ -> None
    in loop t

  let conll_dep_string ?(keep_empty_rh=false) t =
    if (not keep_empty_rh) && is_empty t
    then None
    else
      let rec loop t =
        match (t.good_nf, t.bad_nf) with
          | [],[] ->
            let graph = t.instance.Instance.graph in
            Some (G_graph.to_conll graph)
          | [one], [] -> loop one
          | _ -> None
      in loop t
end (* module Rewrite_history *)

(* ==================================================================================================== *)
module Modul = struct
  type t = {
    name: string;
    local_labels: (string * string list) array;
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
end (* module Modul *)

(* ==================================================================================================== *)
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
        then Error.build ~loc:t.loc "sequence \"%s\" refers to the unknown module \"%s\"."
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
end (* module Sequence *)

(* ==================================================================================================== *)
module Grs = struct

  type t = {
    labels: Label.t list;        (* the list of global edge labels *)
    modules: Modul.t list;       (* the ordered list of modules used from rewriting *)
    sequences: Sequence.t list;
    filename: string;
    ast: Ast.grs;
  }

  let get_modules t = t.modules
  let get_ast t = t.ast
  let get_filename t = t.filename

  let sequence_names t = List.map (fun s -> s.Sequence.name) t.sequences

  let empty = {labels=[]; modules=[]; sequences=[]; ast=Ast.empty_grs; filename=""; }

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

  let build filename =
    let ast = Grew_parser.grs_of_file filename in
    Label.init ast.Ast.labels;
    Domain.init ast.Ast.domain;
    let modules = List.map Modul.build ast.Ast.modules in
    let grs = {
      labels = List.map (fun (l,_) -> Label.from_string l) ast.Ast.labels;
      sequences = List.map (Sequence.build modules) ast.Ast.sequences;
      modules; ast; filename;
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
    Timeout.start ();
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
end (* module Grs *)
