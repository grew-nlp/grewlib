(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

{
  open Printf
  open Log
  open Libgrew_utils
  open Grew_ast
  open Gr_grs_parser

  exception Error of string

  let escaped = ref false

  (* a general notion of "ident" is needed to cover all usages:
     with or without '#', with several '.' (separator for feature names and usual symbol for labels...) *)
  let parse_complex_ident string =
    printf "--parse_complex_ident-->%s<--\n%!" string;
    match Str.split (Str.regexp "#") string with
      | [x] -> Ast.No_sharp x
      | [x;y] -> Ast.Sharp (x,y)
      | _ -> Error.build "\"%s\" is not a valid ident (more than one '#')" string

  let split_comment com =
    let raw = Str.split (Str.regexp "\n") com in
    List.filter (fun l -> not (Str.string_match (Str.regexp "[ \t]*$") l 0)) raw

  let buff = Buffer.create 32
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
(* an identifier is either a single letter, "_", or its lenght is >=2 and it doesn't end with a '-' *)
let ident = (letter | '_') | (letter | '_') (letter | digit | '_' | '.' | '\'' | '-')* (letter | digit | '_' | '\'')

let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let color = hex hex hex hex hex hex

rule comment target = parse
| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; target lexbuf }
| _  { comment target lexbuf }

and comment_multi_doc target = shortest
| (_* as comment)"--%" {
  let start = ref 0 in
  try while (Str.search_forward (Str.regexp "\n") comment !start != -1) do
    start := Str.match_end ();
    incr Parser_global.current_line;
    Lexing.new_line lexbuf;
  done; assert false
  with Not_found ->
    COMMENT(split_comment comment)
}

and comment_multi target = parse
| "*/" { target lexbuf }
| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; comment_multi target lexbuf }
| _  { comment_multi target lexbuf }

and string_lex target = parse
  | '\\' {
    if !escaped
    then (bprintf buff "\\"; escaped := false; string_lex target lexbuf)
    else (escaped := true; string_lex target lexbuf)
  }
  | '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; bprintf buff "\n"; string_lex target lexbuf }
  | '\"' {
    if !escaped
    then (bprintf buff "\""; escaped := false; string_lex target lexbuf)
    else (STRING(Buffer.contents buff) )
  }
  | _ as c {
    if !escaped then bprintf buff "\\";
    escaped := false;
    bprintf buff "%c" c;
    string_lex target lexbuf
  }

(* a dedicated lexer for lexical parameter: read everything until "#END" *)
and lp_lex target = parse
| '\n'                    { incr Parser_global.current_line; Lexing.new_line lexbuf; bprintf buff "\n"; lp_lex target lexbuf }
| _ as c                  { bprintf buff "%c" c; lp_lex target lexbuf }
| "#END" [' ' '\t']* '\n' { incr Parser_global.current_line; LP (Str.split (Str.regexp "\n") (Buffer.contents buff)) }

and global = parse
| [' ' '\t'] { global lexbuf }

| "%--"      { comment_multi_doc global lexbuf }
| "/*"       { comment_multi global lexbuf }
| '%'        { comment global lexbuf }

| "#BEGIN" [' ' '\t']* '\n' { incr Parser_global.current_line; Buffer.clear buff; lp_lex global lexbuf}

| '\n'       { incr Parser_global.current_line; Lexing.new_line lexbuf; global lexbuf}

| "include"     { INCLUDE }
| "features"    { FEATURES }
| "feature"     { FEATURE }
| "file"        { FILE }
| "labels"      { LABELS }
| "suffixes"    { SUFFIXES }
| "match"       { MATCH }
| "without"     { WITHOUT }
| "commands"    { COMMANDS }

| "add_edge"    { ADD_EDGE }
| "del_edge"    { DEL_EDGE }
| "shift_in"    { SHIFT_IN }
| "shift_out"   { SHIFT_OUT }
| "shift"       { SHIFT }
| "merge"       { MERGE }
| "del_node"    { DEL_NODE }
| "add_node"    { ADD_NODE }
| "del_feat"    { DEL_FEAT }
| "activate"    { ACTIVATE }

| "module"      { MODULE }
| "confluent"   { CONFLUENT }
| "rule"        { RULE }
| "lex_rule"    { LEX_RULE }
| "filter"      { FILTER }
| "sequences"   { SEQUENCES }

| "graph"       { GRAPH }

| digit+ ('.' digit*)? as number  { FLOAT (float_of_string number) }

| '$' ident as pat_var     { DOLLAR_ID pat_var}
| '@' ident as cmd_var     { AROBAS_ID cmd_var }
| "@#" color as col        { COLOR col }

| ident as complex_id                        { COMPLEX_ID (parse_complex_ident complex_id) }
| ident '#' ident as complex_id              { COMPLEX_ID (parse_complex_ident complex_id) }

| '{'   { LACC }
| '}'   { RACC }
| '['   { LBRACKET }
| ']'   { RBRACKET }
| '('   { LPAREN }
| ')'   { RPAREN }
| ':'   { DDOT }
| ';'   { SEMIC }
| ','   { COMA }
| '+'   { PLUS }
| '*'   { STAR }
| '#'   { SHARP }
| '='   { EQUAL }
| "!"   { BANG }
| "<>"  { DISEQUAL }

| "<"        { LT }
| ">"        { GT }
| "<=" | "≤" { LE }
| ">=" | "≥" { GE }

| '|'   { PIPE }
| "->"  { GOTO_NODE }
| "-[^" { LTR_EDGE_LEFT_NEG }
| "-["  { LTR_EDGE_LEFT }
| "]->" { LTR_EDGE_RIGHT }
| "<-[" { RTL_EDGE_LEFT }
| "]-"  { RTL_EDGE_RIGHT }
| "==>" { LONGARROW }

| '"'   { Buffer.clear buff; string_lex global lexbuf }

| eof   { EOF }
| _ as c { raise (Error (sprintf "At line %d: unexpected character '%c'" (lexbuf.Lexing.lex_start_p.Lexing.pos_lnum) c)) }
