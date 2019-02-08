(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Log
open Conll


(* ==================================================================================================== *)
(** {2 Location} *)
(* ==================================================================================================== *)
module Loc = struct
  type t = Grew_base.Loc.t
  let to_string = Grew_base.Loc.to_string
end

(* ==================================================================================================== *)
(** {2 Exceptions} *)
(* ==================================================================================================== *)
module Libgrew = struct
  let get_version () = VERSION

  let set_debug_mode flag = Grew_base.Global.debug := flag

  let set_safe_commands flag = Grew_base.Global.safe_commands := flag

  let set_track_rules flag = Grew_base.Global.track_rules := flag

  exception Error of string
  exception Bug of string

  let handle ?(name="") ?(file="No file defined") fct () =
    try fct () with
      (* Raise again already caught exceptions *)
      | Error msg -> raise (Error msg)
      | Bug msg -> raise (Bug msg)

      (* Catch new exceptions *)
      | Grew_base.Error.Parse (msg, Some loc) -> raise (Error (sprintf "%s %s" (Grew_base.Loc.to_string loc) msg))
      | Grew_base.Error.Parse (msg, None) -> raise (Error (sprintf "%s" msg))
      | Grew_base.Error.Build (msg, Some loc) -> raise (Error (sprintf "%s %s" (Grew_base.Loc.to_string loc) msg))
      | Grew_base.Error.Build (msg, None) -> raise (Error (sprintf "%s" msg))
      | Grew_base.Error.Run (msg, Some loc) -> raise (Error (sprintf "%s %s" (Grew_base.Loc.to_string loc) msg))
      | Grew_base.Error.Run (msg, None) -> raise (Error (sprintf "%s" msg))
      | Conll_types.Error msg -> raise (Error (sprintf "Conll error: %s" (Yojson.Basic.to_string msg)))

      | Grew_base.Error.Bug (msg, Some loc) -> raise (Bug (sprintf "%s %s" (Grew_base.Loc.to_string loc) msg))
      | Grew_base.Error.Bug (msg, None) -> raise (Bug (sprintf "%s" msg))
      | Grew_base.Timeout.Stop bound -> raise (Error (sprintf "Timeout (running time execeeds %g seconds)" bound))
      | exc -> raise (Bug (sprintf "[Libgrew.%s] UNCAUGHT EXCEPTION: %s" name (Printexc.to_string exc)))
end

(* ==================================================================================================== *)
(** {2 Domain} *)
(* ==================================================================================================== *)
module Domain = struct
  type t = Grew_domain.Domain.t

  let load filename =
    Libgrew.handle ~name:"Domain.load"
      (fun () ->
      let ast = Grew_loader.Loader.domain filename in
      Grew_grs.Grs.domain_build ast
      ) ()

  let feature_names domain =
    Libgrew.handle ~name:"Domain.feature_names"
      (fun () -> Grew_domain.Domain.feature_names domain)
      ()

  let dump domain =
    Libgrew.handle ~name:"Domain.dump"
      (fun () -> Grew_domain.Domain.dump domain)
      ()

end

(* ==================================================================================================== *)
(** {2 Patterns} *)
(* ==================================================================================================== *)
module Pattern = struct
  type t = Grew_rule.Rule.pattern

  let load ?domain file =
  Libgrew.handle ~name:"Pattern.load" (fun () -> Grew_rule.Rule.build_pattern ?domain (Grew_loader.Loader.pattern file)) ()

  let parse ?domain desc =
  Libgrew.handle ~name:"Pattern.load" (fun () -> Grew_rule.Rule.build_pattern ?domain (Grew_loader.Parser.pattern desc)) ()

  let pid_name_list pattern =
  Libgrew.handle ~name:"Pattern.pid_list"
    (fun () -> List.map (fun x -> x) (Grew_rule.Rule.pid_name_list pattern)
    ) ()
end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching = struct
  type t = Grew_rule.Rule.matching

  let to_json pattern graph t = Grew_rule.Rule.to_python pattern graph t

  let nodes pattern graph matching =
    Libgrew.handle ~name:"Matching.nodes" (fun () ->
      Grew_rule.Rule.node_matching pattern graph matching
    ) ()

end

(* ==================================================================================================== *)
(** {2 Deco} *)
(* ==================================================================================================== *)
module Deco = struct
  type t = Grew_graph.G_deco.t
  let build pattern matching = Grew_rule.Rule.match_deco pattern matching
end

(* ==================================================================================================== *)
(** {2 Graph} *)
(* ==================================================================================================== *)
module Graph = struct
  type t = Grew_graph.G_graph.t

  let load_gr ?domain file =
    if not (Sys.file_exists file)
    then raise (Libgrew.Error ("File_not_found: " ^ file))
    else
      Libgrew.handle ~name:"Graph.load_gr" ~file
        (fun () ->
          let gr_ast = Grew_loader.Loader.gr file in
          Grew_graph.G_graph.build ?domain gr_ast
        ) ()

  let load_conll ?domain file =
    Libgrew.handle ~name:"Graph.load_conll" ~file
      (fun () ->
        Grew_graph.G_graph.of_conll ?domain (Conll.load file)
      ) ()

  let load_brown ?domain file =
    Libgrew.handle ~name:"Graph.load_brown"
      (fun () ->
        let brown = Grew_base.File.load file in
        Grew_graph.G_graph.of_brown ?domain brown
      ) ()

  let load_pst ?domain file =
    if not (Sys.file_exists file)
    then raise (Libgrew.Error ("File_not_found: " ^ file))
    else
      Libgrew.handle ~name:"load_pst" ~file
        (fun () ->
          let const_ast = Grew_loader.Loader.phrase_structure_tree file in
          Grew_graph.G_graph.of_pst ?domain const_ast
        ) ()

  let load ?domain file =
    Libgrew.handle ~name:"Graph.load_graph" ~file
      (fun () ->
        match Grew_base.File.get_suffix file with
        | Some ".gr" -> load_gr ?domain file
        | Some ".conll" -> load_conll ?domain file
        | Some ".br" | Some ".melt" -> load_brown ?domain file
        | Some ".cst" -> load_pst ?domain file
        | _ ->
            Log.fwarning "Unknown file format for input graph '%s', try to guess..." file;
            let rec loop = function
            | [] -> Log.fcritical "[Libgrew.load_graph] Cannot guess input file format of file '%s'. Use .gr or .conll file extension" file
            | load_fct :: tail -> try load_fct ?domain file with _ -> loop tail in
            loop [load_gr; load_conll; load_brown; load_pst]
      ) ()

  let of_gr ?domain ?(grewpy=false) gr_string =
    Libgrew.handle ~name:"Graph.of_gr" (fun () -> Grew_graph.G_graph.build ?domain ~grewpy (Grew_loader.Parser.gr gr_string)) ()

  let of_conll ?domain conll =
    Libgrew.handle ~name:"Graph.of_conll" (fun () -> Grew_graph.G_graph.of_conll ?domain conll) ()

  let of_pst ?domain pst_string =
    Libgrew.handle ~name:"of_pst"
      (fun () ->
        let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
        (Grew_graph.G_graph.of_pst ?domain pst_ast)
      ) ()

  let sentence_of_pst ?domain pst_string =
    Libgrew.handle ~name:"of_pst"
      (fun () ->
        let pst_ast = Grew_loader.Parser.phrase_structure_tree pst_string in
        let word_list = Grew_ast.Ast.word_list pst_ast in
        Sentence.fr_clean_spaces (String.concat " " word_list)
      ) ()

  let of_json json =
    Libgrew.handle ~name:"Graph.of_json" (fun () -> Grew_graph.G_graph.of_json json) ()

  let of_brown ?domain ?sentid brown =
    Libgrew.handle ~name:"Graph.of_brown" (fun () -> Grew_graph.G_graph.of_brown ?domain ?sentid brown) ()

  let to_dot ?main_feat ?(deco=Grew_graph.G_deco.empty) ?get_url graph =
    Libgrew.handle ~name:"Graph.to_dot" (fun () -> Grew_graph.G_graph.to_dot ?main_feat ?get_url graph ~deco) ()

  let to_dep ?filter ?main_feat ?(deco=Grew_graph.G_deco.empty) graph =
    Libgrew.handle ~name:"Graph.to_dep" (fun () -> Grew_graph.G_graph.to_dep ?filter ?main_feat ~deco graph) ()

  let to_gr graph =
    Libgrew.handle ~name:"Graph.to_gr" (fun () -> Grew_graph.G_graph.to_gr graph) ()

  let to_json graph =
    Libgrew.handle ~name:"Graph.to_json" (fun () -> Grew_graph.G_graph.to_json graph) ()

  let to_conll graph =
    Libgrew.handle ~name:"Graph.to_conll" (fun () -> Grew_graph.G_graph.to_conll graph) ()

  let to_conll_string ?cupt graph =
    Libgrew.handle ~name:"Graph.to_conll_string" (fun () -> Grew_graph.G_graph.to_conll_string ?cupt graph) ()

  let to_sentence ?only_pivot ?main_feat ?deco gr =
    Libgrew.handle ~name:"Graph.to_sentence"
      (fun () ->
        Grew_graph.G_graph.to_sentence ?only_pivot ?main_feat ?deco gr
      ) ()

  let save_conll filename graph =
    Libgrew.handle ~name:"Graph.save_conll" (fun () ->
      let out_ch = open_out filename in
      fprintf out_ch "%s" (Grew_graph.G_graph.to_conll_string graph);
      close_out out_ch
    ) ()

  let search_pattern ?domain pattern graph =
    Libgrew.handle ~name:"Graph.search_pattern" (fun () ->
      Grew_rule.Rule.match_in_graph ?domain pattern graph
    ) ()
end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs = struct
  type t = Grew_grs.Grs.t

  let load file =
    Libgrew.handle ~name:"Grs.load" ~file
      (fun () ->
        Grew_grs.Grs.load file
      ) ()

  let dump grs =
    Libgrew.handle ~name:"Grs.dump"
      (fun () ->
        Grew_grs.Grs.dump grs
      ) ()

  let domain grs =
    Libgrew.handle ~name:"Grs.domain"
      (fun () ->
        Grew_grs.Grs.domain grs
      ) ()

  let to_json grs =
    Libgrew.handle ~name:"Grs.to_json"
      (fun () ->
        Grew_grs.Grs.to_json grs
      ) ()

  let get_strat_list grs =
    Libgrew.handle ~name:"Grs.get_strat_list"
      (fun () ->
        Grew_grs.Grs.get_strat_list grs
        ) ()
end

(* ==================================================================================================== *)
(** {2 Rewrite} *)
(* ==================================================================================================== *)
module Rewrite = struct
  type display = Libgrew_types.rew_display

  let size = Libgrew_types.rew_display_size

  let set_max_rules bound = Grew_rule.Rule.set_max_rules bound

  let display ~gr ~grs ~strat =
    Libgrew.handle ~name:"Rewrite.display" (fun () -> Grew_grs.Grs.wrd_rewrite grs strat gr) ()

  let set_timeout t = Grew_base.Timeout.timeout := t

  let simple_rewrite ~gr ~grs ~strat =
    Libgrew.handle ~name:"Rewrite.simple_rewrite" (fun () -> Grew_grs.Grs.gwh_simple_rewrite grs strat gr) ()

  let at_least_one ~grs ~strat =
    Libgrew.handle ~name:"Rewrite.at_least_one" (fun () -> Grew_grs.Grs.at_least_one grs strat) ()
  let at_most_one ~grs ~strat =
    Libgrew.handle ~name:"Rewrite.at_most_one" (fun () -> Grew_grs.Grs.at_most_one grs strat) ()
end
