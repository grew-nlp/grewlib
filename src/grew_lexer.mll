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
  open Grew_base
  open Grew_ast
  open Grew_parser

  exception Error of string

  let escaped = ref false

  (* a general notion of "ident" is needed to cover all usages:
     with or without '#', with several '.' (separator for feature names and usual symbol for labels...) *)
  let parse_complex_ident string =
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

(* a general_ident is an arbitrary sequence of:
   - letter
   - digit
   - underscore '_'
   - dash '-'
  for basic ident construction and
   - dot '.'
   - colon ':'
   - sharp '#'
   - star '*'
  The first characted cannot be a digit, a sharp or a colon (to avoid confusion).
 *)
let label_ident =
  (letter | '_' | '-' | '.' | '*') (letter | digit | '_' | '\'' | '-' | '.' | ':' | '*')*

let general_ident =
  (letter | '_' ) |
  (letter | '_' | '.' ) (letter | digit | '_' | '\'' | '-' | '.' | '#')* (letter | digit | '_' | '\'' | '.')

let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let color = hex hex hex hex hex hex | hex hex hex


(* ------------------------------------------------------------------------------- *)
(* Rules                                                                           *)
(* ------------------------------------------------------------------------------- *)

rule comment target = parse
| '\n'  { incr Global.current_line; Lexing.new_line lexbuf; target lexbuf }
| eof   { EOF }
| _     { comment target lexbuf }

and comment_multi_doc target = shortest
| (_* as comment)"--%" {
  let start = ref 0 in
  try while (Str.search_forward (Str.regexp "\n") comment !start != -1) do
    start := Str.match_end ();
    incr Global.current_line;
    Lexing.new_line lexbuf;
  done; assert false
  with Not_found ->
    COMMENT(split_comment comment)
}

and comment_multi target = parse
| "*/" { target lexbuf }
| '\n' { incr Global.current_line; Lexing.new_line lexbuf; comment_multi target lexbuf }
| _  { comment_multi target lexbuf }

and string_lex target = parse
  | '\\' {
    if !escaped
    then (bprintf buff "\\"; escaped := false; string_lex target lexbuf)
    else (escaped := true; string_lex target lexbuf)
  }
  | '\n' { incr Global.current_line; Lexing.new_line lexbuf; bprintf buff "\n"; string_lex target lexbuf }
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
| '\n'                    { incr Global.current_line; Lexing.new_line lexbuf; bprintf buff "\n"; lp_lex target lexbuf }
| _ as c                  { bprintf buff "%c" c; lp_lex target lexbuf }
| "#END" [' ' '\t']* '\n' { incr Global.current_line; LEX_PAR (Str.split (Str.regexp "\n") (Buffer.contents buff)) }

(* The lexer must be different when label_ident are parsed. The [global] lexer calls either
   [label_parser] or [standard] depending on the flag [Global.label_flag].
   Difference are:
   - a label_ident may contain ':' (like in D:suj:obj) and ':' is a token elsewhere
   - a label_ident may contain '-' anywhere (like "--" in Tiger) but '-' is fordiden as the first or last character elsewhere
   - the string "*" is lexed as ID by [label_parser] and as STAR by [standard]
*)
and global = parse
| ""   {  if !Global.label_flag
          then label_parser global lexbuf
          else standard global lexbuf
        }


and label_parser target = parse
| [' ' '\t'] { global lexbuf }
| "/*"       { comment_multi global lexbuf }
| '%'        { comment global lexbuf }
| '\n'       { incr Global.current_line; Lexing.new_line lexbuf; global lexbuf}

| '{'   { LACC }
| '}'   { Global.label_flag := false; RACC }
| ','   { COMA }
| '|'   { PIPE }

| '@' general_ident as cmd_var     { AROBAS_ID cmd_var }
| "@#" color as col        { COLOR col }

| label_ident as id { ID id }
| '"'   { Buffer.clear buff; string_lex global lexbuf }

| "]->" { Global.label_flag := false; LTR_EDGE_RIGHT }
| "]-"  { Global.label_flag := false; RTL_EDGE_RIGHT }
| "]=>" { Global.label_flag := false; ARROW_RIGHT }

| _ as c { raise (Error (sprintf "unexpected character '%c'" c)) }

and standard target = parse
| [' ' '\t'] { global lexbuf }

| "%--"      { comment_multi_doc global lexbuf }
| "/*"       { comment_multi global lexbuf }
| '%'        { comment global lexbuf }

| "#BEGIN" [' ' '\t']* '\n' { incr Global.current_line; Buffer.clear buff; lp_lex global lexbuf}

| '\n'       { incr Global.current_line; Lexing.new_line lexbuf; global lexbuf}

| "include"     { INCL }
| "features"    { FEATURES }
| "feature"     { FEATURE }
| "file"        { FILE }
| "labels"      { Global.label_flag := true; LABELS }
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

| '$' general_ident as pat_var     { DOLLAR_ID pat_var}
| '@' general_ident as cmd_var     { AROBAS_ID cmd_var }
| "@#" color as col        { COLOR col }

| '*'   { STAR }
| general_ident as id { ID id }

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
| '#'   { SHARP }
| '='   { EQUAL }
| "!"   { BANG }
| "<>"  { DISEQUAL }

| "<"        { LT }
| ">"        { GT }
| "<=" | "≤" { LE }
| ">=" | "≥" { GE }

| '|'   { PIPE }
| "->"  { EDGE }
| "-[^" { Global.label_flag := true; LTR_EDGE_LEFT_NEG }
| "-["  { Global.label_flag := true; LTR_EDGE_LEFT }
| "]->" { LTR_EDGE_RIGHT }
| "<-[" { Global.label_flag := true; RTL_EDGE_LEFT }
| "]-"  { RTL_EDGE_RIGHT }

| "==>" { ARROW }
| "=["  { Global.label_flag := true; ARROW_LEFT }
| "=[^" { Global.label_flag := true; ARROW_LEFT_NEG }
| "]=>" { ARROW_RIGHT }

| '"'   { Buffer.clear buff; string_lex global lexbuf }

| eof   { EOF }
| _ as c { raise (Error (sprintf "unexpected character '%c'" c)) }
