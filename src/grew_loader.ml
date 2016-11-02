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
let parse_handle file fct lexbuf =
  let get_loc () = Loc.file_line file !Global.current_line in
  try fct lexbuf with
    | Grew_lexer.Error msg -> Error.parse ~loc:(get_loc ()) "Lexing error: %s" msg
    | Grew_parser.Error -> Error.parse ~loc:(get_loc ()) "Syntax error: %s" (Lexing.lexeme lexbuf)
    | Error.Build (msg, None) -> Error.parse ~loc:(get_loc ()) "Syntax error: %s" msg
    | Error.Build (msg, Some loc) -> Error.parse ~loc "Syntax error: %s" msg
    | Failure msg -> Error.parse ~loc:(get_loc ()) "Failure: %s" msg
    | err -> Error.bug ~loc:(get_loc ()) "Unexpected error: %s" (Printexc.to_string err)

module Loader = struct


  (* ------------------------------------------------------------------------------------------*)
  let parse_file_to_grs_wi file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let grs = parse_handle file (Grew_parser.grs_wi Grew_lexer.global) lexbuf in
      close_in in_ch;
      grs
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.parse_file_to_grs_wi] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let parse_file_to_module_list file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let module_list = parse_handle file (Grew_parser.included Grew_lexer.global) lexbuf in
      close_in in_ch;
      module_list
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.parse_file_to_module_list] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let domain file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let gr = parse_handle file (Grew_parser.domain Grew_lexer.global) lexbuf in
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
    let grs_wi = parse_file_to_grs_wi main_file in
    let domain = match grs_wi.Ast.domain_wi with
      | None -> None
      | Some (Ast.Dom d) -> Some d
      | Some (Ast.Dom_file file) -> Some (domain file) in
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

  (* ------------------------------------------------------------------------------------------*)
  let gr file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let gr = parse_handle file (Grew_parser.gr Grew_lexer.global) lexbuf in
      close_in in_ch;
      gr
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.gr] %s" msg


  (* ------------------------------------------------------------------------------------------*)
  let pattern file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let pattern = parse_handle file (Grew_parser.pattern Grew_lexer.global) lexbuf in
      close_in in_ch;
      pattern
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.pattern] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let phrase_structure_tree file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let graph = parse_handle file (Grew_parser.phrase_structure_tree Grew_lexer.const) lexbuf in
      close_in in_ch;
      graph
    with Sys_error msg -> Error.parse ~loc:(Loc.file file) "[Grew_loader.Loader.phrase_structure_tree] %s" msg

end (* module Loader *)


module Parser = struct
  (* ------------------------------------------------------------------------------------------*)
  let gr gr_string =
    try
      Global.init "Not a file";
      let lexbuf = Lexing.from_string gr_string in
      let gr = parse_handle "Not a file" (Grew_parser.gr Grew_lexer.global) lexbuf in
      gr
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.gr] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let phrase_structure_tree s =
    try
      Global.init "Not a file";
      let lexbuf = Lexing.from_string s in
      let graph = parse_handle "Not a file" (Grew_parser.phrase_structure_tree Grew_lexer.const) lexbuf in
      graph
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.phrase_structure_tree] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let pattern desc =
    try
      Global.init "Not a file";
      let lexbuf = Lexing.from_string desc in
      let pattern = parse_handle "Not a file" (Grew_parser.pattern Grew_lexer.global) lexbuf in
      pattern
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.pattern] %s" msg

  (* ------------------------------------------------------------------------------------------*)
  let strategy desc =
    try
      Global.init "Not a file";
      let lexbuf = Lexing.from_string desc in
      let strategy = parse_handle "Not a file" (Grew_parser.strategy Grew_lexer.global) lexbuf in
      strategy
    with Sys_error msg -> Error.parse "[Grew_loader.Parser.strategy] %s" msg


end
