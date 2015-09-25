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

module Loader = struct

  (* message and location *)
  exception Error of (string * Loc.t option)

  (* ------------------------------------------------------------------------------------------*)
  (** general function to handle parse errors *)
  let parse_handle file fct lexbuf =
    try fct lexbuf with
      | Grew_lexer.Error msg ->
        let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
        raise (Error ("Lexing error:"^msg, Some (Loc.file_line file cp)))
      | Grew_parser.Error ->
        let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
        raise (Error ("Syntax error:"^(Lexing.lexeme lexbuf), Some (Loc.file_line file cp)))
      | Failure msg ->
        let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
        raise (Error ("Failure:"^msg, Some (Loc.file_line file cp)))
      | Error.Build (msg,_) ->
        let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
        raise (Error ("Syntax error:"^msg, Some (Loc.file_line file cp)))
      | err ->
        let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
        raise (Error ("Unexpected error:"^(Printexc.to_string err), Some (Loc.file_line file cp)))

  (* ------------------------------------------------------------------------------------------*)
  let parse_file_to_grs_with_includes file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let grs = parse_handle file (Grew_parser.grs_with_include Grew_lexer.global) lexbuf in
      close_in in_ch;
      grs
    with Sys_error msg -> raise (Error (msg, None))

  (* ------------------------------------------------------------------------------------------*)
  let parse_file_to_module_list loc file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let module_list = parse_handle file (Grew_parser.included Grew_lexer.global) lexbuf in
      close_in in_ch;
      module_list
    with Sys_error msg-> raise (Error (msg, None))

  (* ------------------------------------------------------------------------------------------*)
  (**
     [parse_string file] where [file] is a file following the grew syntax
     @param file the file to parse
     @return a syntactic tree of the parsed file
  *)
  let grs main_file =
    let grs_with_includes = parse_file_to_grs_with_includes main_file in
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
        (flatten_modules sub_file (parse_file_to_module_list loc sub_file))
        @ (flatten_modules current_file tail) in
    {
      Ast.domain = grs_with_includes.Ast.domain_wi;
      Ast.labels = grs_with_includes.Ast.labels_wi;
      Ast.modules = flatten_modules main_file grs_with_includes.Ast.modules_wi;
      Ast.sequences = grs_with_includes.Ast.sequences_wi;
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
    with Sys_error msg-> raise (Error (msg, None))

  (* ------------------------------------------------------------------------------------------*)
  let pattern file =
    try
      Global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let gr = parse_handle file (Grew_parser.pattern Grew_lexer.global) lexbuf in
      close_in in_ch;
      gr
    with Sys_error msg-> raise (Error (msg, None))

end (* module Loader *)
