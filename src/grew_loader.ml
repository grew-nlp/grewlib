(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_ast

(* ------------------------------------------------------------------------------------------*)
(** general function to handle parse errors *)
let parse_handle fct lexbuf =
  try fct lexbuf with
    | Grew_lexer.Error msg -> Error.parse ~loc:(Global.get_loc ()) "Lexing error: %s" msg
    | Grew_parser.Error -> Error.parse ~loc:(Global.get_loc ()) "Syntax error: %s" (Lexing.lexeme lexbuf)
    | Error.Build (msg, None) -> Error.parse ~loc:(Global.get_loc ()) "Syntax error: %s" msg
    | Error.Build (msg, Some loc) -> Error.parse ~loc "Syntax error: %s" msg
    | Failure msg -> Error.parse ~loc:(Global.get_loc ()) "Failure: %s" msg
    | err -> Error.bug ~loc:(Global.get_loc ()) "Unexpected error: %s" (Printexc.to_string err)

module Loader = struct


  (* ------------------------------------------------------------------------------------------*)
  let parse_file_to_grs_wi file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let grs = parse_handle (Grew_parser.grs_wi Grew_lexer.global) lexbuf in
      close_in in_ch;
      grs
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.parse_file_to_grs_wi] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let parse_file_to_module_list file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let module_list = parse_handle (Grew_parser.included Grew_lexer.global) lexbuf in
      close_in in_ch;
      module_list
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.parse_file_to_module_list] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let domain file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let gr = parse_handle (Grew_parser.domain Grew_lexer.global) lexbuf in
      close_in in_ch;
      gr
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.domain] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  (**
     [parse_string file] where [file] is a file following the grew syntax
     @param file the file to parse
     @return a syntactic tree of the parsed file
  *)
  let grs main_file =
    let real_dir =
      match (Unix.lstat main_file).Unix.st_kind with
      | Unix.S_LNK -> Filename.dirname (Unix.readlink main_file)
      | _ -> Filename.dirname main_file in

    let unlink file = Filename.concat real_dir (Filename.basename file) in

    let grs_wi = parse_file_to_grs_wi (unlink main_file) in
    let domain = match grs_wi.Ast.domain_wi with
      | None -> None
      | Some (Ast.Dom d) -> Some d
      | Some (Ast.Dom_file file) -> Some (domain (unlink file)) in
    let rec flatten_modules current_file = function
      | [] -> []
      | Ast.Modul m :: tail ->
        {m with Ast.mod_dir = Filename.dirname current_file}
        :: (flatten_modules current_file tail)
      | Ast.Includ (inc_file,loc) :: tail ->
        let sub_file =
          if Filename.is_relative inc_file
          then Filename.concat (Filename.dirname current_file) inc_file
          else inc_file in
        (flatten_modules sub_file (parse_file_to_module_list sub_file))
        @ (flatten_modules current_file tail) in
    {
      Ast.domain = domain;
      Ast.modules = flatten_modules main_file grs_wi.Ast.modules_wi;
      Ast.strategies = grs_wi.Ast.strategies_wi;
    }

  let rec check_duplicate_id id = function
    | [] -> None
    | New_ast.Rule r :: _ when r.Ast.rule_id = id -> Some r.Ast.rule_loc
    | New_ast.Package (loc, name, _) :: _ when name = id -> Some loc
    | New_ast.Strategy (loc, name, _) :: _ when name = id -> Some loc
    | _ -> None

  let rec check_grs = function
    | [] -> ()
    | New_ast.Rule r :: tail ->
      begin
        match check_duplicate_id r.Ast.rule_id tail with
        | None -> ()
        | Some loc -> Error.build "Identifier \"%s\" is used twice in the same package (%s and %s)"
          r.Ast.rule_id (Loc.to_string r.Ast.rule_loc) (Loc.to_string loc)
      end;
      check_grs tail
    | New_ast.Strategy (loc, name, _) :: tail
    | New_ast.Package (loc, name, _) :: tail ->
      begin
        match check_duplicate_id name tail with
        | None -> ()
        | Some loc2 -> Error.build "Identifier \"%s\" is used twice in the same package (%s and %s)"
          name (Loc.to_string loc) (Loc.to_string loc2)
      end;
      check_grs tail
    | _ :: tail -> check_grs tail

  let real_dir file =
    match (Unix.lstat file).Unix.st_kind with
    | Unix.S_LNK -> Filename.dirname (Unix.readlink file)
    | _ -> Filename.dirname file

  let unlink dir file = Filename.concat dir (Filename.basename file)

  let loc_new_grs file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let grs = parse_handle (Grew_parser.new_grs Grew_lexer.global) lexbuf in
      close_in in_ch;
      grs
  with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.grs] %s" msg

  let rec unfold_new_grs dir top new_ast_grs = List.fold_left
    (fun acc decl -> match decl with
      | New_ast.Import filename ->
        let real_file = unlink dir filename in
        let pack_name = match CCString.chop_suffix ~suf:".grs" filename with
          | Some x -> x
          | None -> Error.build "Imported file must have the \".grs\" file extension" in
        let sub = loc_new_grs filename in
        let unfolded_sub = unfold_new_grs (real_dir real_file) false sub in
          New_ast.Package (Loc.file filename, pack_name, unfolded_sub) :: acc
      | New_ast.Include filename ->
        let real_file = unlink dir filename in
        let sub = loc_new_grs real_file in
        let unfolded_sub = unfold_new_grs (real_dir real_file) top sub in
          unfolded_sub @ acc
      | New_ast.Features _ when not top -> Error.build "Non top features declaration"
      | New_ast.Labels _ when not top -> Error.build "Non top labels declaration"
      | x -> x :: acc
    ) [] new_ast_grs

  let new_grs file =
    let final_grs = unfold_new_grs (real_dir file) true (loc_new_grs file) in
    check_grs final_grs;
    final_grs

  (* ------------------------------------------------------------------------------------------*)
  let gr file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let gr = parse_handle (Grew_parser.gr Grew_lexer.global) lexbuf in
      close_in in_ch;
      gr
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.gr] %s" msg


  (* ------------------------------------------------------------------------------------------*)
  let pattern file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let pattern = parse_handle (Grew_parser.pattern Grew_lexer.global) lexbuf in
      close_in in_ch;
      pattern
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.pattern] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let phrase_structure_tree file =
    try
      Global.new_file file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let graph = parse_handle (Grew_parser.phrase_structure_tree Grew_lexer.const) lexbuf in
      close_in in_ch;
      graph
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.phrase_structure_tree] %s" msg

end (* module Loader *)


module Parser = struct
  (* ------------------------------------------------------------------------------------------*)
  let gr gr_string =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string gr_string in
      let gr = parse_handle (Grew_parser.gr Grew_lexer.global) lexbuf in
      gr
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.gr] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let phrase_structure_tree s =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string s in
      let graph = parse_handle (Grew_parser.phrase_structure_tree Grew_lexer.const) lexbuf in
      graph
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.phrase_structure_tree] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let pattern desc =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string desc in
      let pattern = parse_handle (Grew_parser.pattern Grew_lexer.global) lexbuf in
      pattern
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.pattern] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let strat_def desc =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string desc in
      let strategy = parse_handle (Grew_parser.strat_def Grew_lexer.global) lexbuf in
      strategy
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.strategy] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let strategy desc =
    try
      Global.new_string ();
      let lexbuf = Lexing.from_string desc in
      let strategy = parse_handle (Grew_parser.strat_desc Grew_lexer.global) lexbuf in
      strategy
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.strategy] %s" msg


end
