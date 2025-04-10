(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_utils
open Grew_ast

(* ------------------------------------------------------------------------------------------*)
(** general function to handle parse errors *)
let parse_handle fct_name fct lexbuf =
  try fct lexbuf with
  | Grew_lexer.Error msg ->        Error.parse ~loc:(Global.get_loc ()) "%s, Lexing error: %s" fct_name msg
  | Grew_parser.Error ->           Error.parse ~loc:(Global.get_loc ()) "%s, Parsing error: %s" fct_name (Lexing.lexeme lexbuf)
  | Error.Build (msg, None) ->     Error.parse ~loc:(Global.get_loc ()) "%s, Syntax error: %s" fct_name msg
  | Error.Build (msg, Some loc) -> Error.parse ~loc "%s, Syntax error: %s" fct_name msg
  | Failure msg ->                 Error.parse ~loc:(Global.get_loc ()) "%s, Failure: %s" fct_name msg
  | err ->                         Error.bug ~loc:(Global.get_loc ()) "%s, Unexpected error: %s" fct_name (Printexc.to_string err)

(* ================================================================================ *)
module Loader = struct
  (* ------------------------------------------------------------------------------------------*)

  let rec check_duplicate_id id = function
    | [] -> None
    | Ast.Rule r :: _ when r.Ast.rule_id = id -> Some r.Ast.rule_loc
    | Ast.Package (loc, name, _) :: _ when name = id -> Some loc
    | Ast.Strategy (loc, name, _) :: _ when name = id -> Some loc
    | _::tail -> check_duplicate_id id tail

  let rec check_grs = function
    | [] -> ()
    | Ast.Rule {Ast.rule_loc = loc; rule_id = name; _} :: tail
    | Ast.Strategy (loc, name, _) :: tail ->
      begin
        match check_duplicate_id name tail with
        | None -> ()
        | Some loc2 -> Error.build "Identifier \"%s\" is used twice in the same package (%s and %s)"
                         name (Loc.to_string loc) (Loc.to_string loc2)
      end;
      check_grs tail
    | Ast.Package (loc, name, sub) :: tail ->
      begin
        match check_duplicate_id name tail with
        | None -> ()
        | Some loc2 -> Error.build "Identifier \"%s\" is used twice in the same package (%s and %s)"
                         name (Loc.to_string loc) (Loc.to_string loc2)
      end;
      check_grs sub;
      check_grs tail
    | _ :: tail -> check_grs tail

  let real_dir file =
    match (Unix.lstat file).Unix.st_kind with
    | Unix.S_LNK -> Filename.dirname (Unix.readlink file)
    | _ -> Filename.dirname file

  let loc_grs file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let grs = parse_handle "loc_grs" (Grew_parser.grs Grew_lexer.global) lexbuf in
      close_in in_ch;
      Global.update_grs_timestamp file;
      grs
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.grs] %s" msg

  let rec unfold_grs dir top path new_ast_grs =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Ast.Import filename ->
          let real_file = Filename.concat dir filename in
          let pack_name =
            match CCString.chop_suffix ~suf:".grs" filename with
            | Some x -> x
            | None -> Error.build "Imported file must have the \".grs\" file extension" in
          let sub = loc_grs real_file in
          let unfolded_sub = unfold_grs (real_dir real_file) false (path ^ pack_name ^ ".") sub in
          Ast.Package (Loc.file filename, pack_name, unfolded_sub) :: acc
        | Ast.Include filename ->
          let real_file = Filename.concat dir filename in
          let sub = loc_grs real_file in
          let unfolded_sub = unfold_grs (real_dir real_file) top path sub in
           unfolded_sub @ acc
        | Ast.Features _ when not top -> Error.build "Non top features declaration"
        | Ast.Labels _ when not top -> Error.build "Non top labels declaration"
        | Ast.Package (loc, name, decls) ->
          Ast.Package (loc, name, unfold_grs dir top (path ^ name ^ ".") decls) :: acc
        | Ast.Rule ast_rule ->
          Ast.Rule {ast_rule with Ast.rule_dir = Some dir; Ast.rule_path = path} :: acc
        | x -> x :: acc
      ) [] new_ast_grs

  let grs file =
    Global.reset_grs_timestamp ();
    let final_grs = unfold_grs (real_dir file) true "" (loc_grs file) in
    check_grs final_grs;
    final_grs

  (* ================================================================================ *)

  (* ------------------------------------------------------------------------------------------*)
  let basic file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let basic = parse_handle "[Grew_loader.Loader.basic]" (Grew_parser.basic Grew_lexer.global) lexbuf in
      close_in in_ch;
      basic
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.basic] %s" msg


  (* ------------------------------------------------------------------------------------------*)
  let request file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let request = parse_handle "[Grew_loader.Loader.request]" (Grew_parser.isolated_request Grew_lexer.global) lexbuf in
      close_in in_ch;
      request
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.request] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let phrase_structure_tree file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let graph = parse_handle "[Grew_loader.Loader.phrase_structure_tree]" (Grew_parser.phrase_structure_tree Grew_lexer.const) lexbuf in
      close_in in_ch;
      graph
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.phrase_structure_tree] %s" msg

end (* module Loader *)


module Parser = struct
  (* ------------------------------------------------------------------------------------------*)
  let basic basic_string =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string basic_string in
      let basic = parse_handle "[Grew_loader.Parser.basic]" (Grew_parser.basic Grew_lexer.global) lexbuf in
      basic
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.basic] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let grs grs_string =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string grs_string in
      let grs = parse_handle "[Grew_loader.Parser.grs]" (Grew_parser.grs Grew_lexer.global) lexbuf in
      grs
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.grs] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let phrase_structure_tree s =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string s in
      let graph = parse_handle "[Grew_loader.Parser.phrase_structure_tree]" (Grew_parser.phrase_structure_tree Grew_lexer.const) lexbuf in
      graph
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.phrase_structure_tree] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let request desc =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string desc in
      let request = parse_handle "[Grew_loader.Parser.request]" (Grew_parser.isolated_request Grew_lexer.global) lexbuf in
      request
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.request] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let strategy desc =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string desc in
      let strategy = parse_handle "[Grew_loader.Parser.strategy]" (Grew_parser.strat_alone Grew_lexer.global) lexbuf in
      strategy
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.strategy] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let key s =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string s in
      let k = parse_handle "[Grew_loader.Parser.key]" (Grew_parser.key Grew_lexer.key) lexbuf in
      k
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.key] %s" msg
end (* module Parser *)
