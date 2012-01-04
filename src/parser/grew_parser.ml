open Grew_utils
open Grew_ast

module Grew_parser = struct

  (* message and line number *)
  exception Parse_error of (string * int option)
      
(* ------------------------------------------------------------------------------------------*)
(** general fucntion to handle parse errors *)
let parse_handle fct lexbuf =
  try fct lexbuf with
  | Lexer.Error msg -> 
      let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
      raise (Parse_error ("Lexing error:"^msg, Some cp))
  | Gr_grs_parser.Error -> 
      let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
      raise (Parse_error ("Syntax error:"^(Lexing.lexeme lexbuf), Some cp))
  | Failure msg -> 
      let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
      raise (Parse_error ("Failure:"^msg, Some cp))
  | err -> 
      let cp = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
      raise (Parse_error ("Unexpected error:"^(Printexc.to_string err), Some cp))
        
(* ------------------------------------------------------------------------------------------*)
        (**
	 [parse_string str] where [str] is a string following the grew syntax
	 @param str the string to parse
	 @return a syntactic tree of the parsed file
       *)
  let parse_string_to_grs str = parse_handle (Gr_grs_parser.grs Lexer.global) (Lexing.from_string str)

(* ------------------------------------------------------------------------------------------*)
  let parse_file_to_grs_with_includes file = 
    try
      Parser_global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let grs = parse_handle (Gr_grs_parser.grs_with_include Lexer.global) lexbuf in
      close_in in_ch;
      grs
    with Sys_error msg-> raise (Parse_error (msg, None))

(* ------------------------------------------------------------------------------------------*)
  let parse_file_to_module_list loc file =
    try
      Parser_global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let module_list = parse_handle (Gr_grs_parser.included Lexer.global) lexbuf in
      close_in in_ch;
      module_list
    with Sys_error msg-> raise (Parse_error (msg, None))

(* ------------------------------------------------------------------------------------------*)
  (**
     [parse_string file] where [file] is a file following the grew syntax
     @param file the file to parse
     @return a syntactic tree of the parsed file
   *)
  let grs_of_file main_file =
    let grs_with_includes = parse_file_to_grs_with_includes main_file in
    let rec flatten_modules = function
      | [] -> []
      | Ast.Modul m :: tail -> m :: (flatten_modules tail)
      | Ast.Includ (inc_file,loc) :: tail -> 
          let sub_file = 
            if Filename.is_relative inc_file
            then Filename.concat (Filename.dirname main_file) inc_file
            else inc_file in
          (flatten_modules (parse_file_to_module_list loc sub_file))
          @ (flatten_modules tail) in
    {
     Ast.domain = grs_with_includes.Ast.domain_wi;
     Ast.labels = grs_with_includes.Ast.labels_wi;
     Ast.modules = flatten_modules grs_with_includes.Ast.modules_wi;
     Ast.sequences = grs_with_includes.Ast.sequences_wi;
   }
     
(* ------------------------------------------------------------------------------------------*)
      (**
	 [parse_string str] where [str] is a string following the grew syntax
	 @param str the string to parse
	 @return a syntactic tree of the parsed file
       *)
  let parse_string_to_gr str = parse_handle (Gr_grs_parser.gr Lexer.global) (Lexing.from_string str)


(* ------------------------------------------------------------------------------------------*)
  let gr_of_file file = 
    try
      Parser_global.init file;
      let in_ch = open_in file in
      let lexbuf = Lexing.from_channel in_ch in
      let gr = parse_handle (Gr_grs_parser.gr Lexer.global) lexbuf in
      close_in in_ch;
      gr
    with Sys_error msg-> raise (Parse_error (msg, None))
end
