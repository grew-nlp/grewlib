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
   - star '*'
  The first characted cannot be a digit, or a colon (to avoid confusion).
 *)
let label_ident =
  (letter | '_' | '-' | '.' | '*') (letter | digit | '_' | '\'' | '-' | '.' | ':' | '*')*

let general_ident =
  (letter | '_' ) |
  (letter | '_' | '.' ) (letter | digit | '_' | '\'' | '-' | '.')* (letter | digit | '_' | '\'' | '.')

let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let color = hex hex hex hex hex hex | hex hex hex


(* ------------------------------------------------------------------------------- *)
(* Rules                                                                           *)
(* ------------------------------------------------------------------------------- *)

rule comment target = parse
| '\n'  { Global.new_line (); Lexing.new_line lexbuf; target lexbuf }
| eof   { EOF }
| _     { comment target lexbuf }

and comment_multi_doc target = shortest
| (_* as comment)"--%" {
  let start = ref 0 in
  try while (Str.search_forward (Str.regexp "\n") comment !start != -1) do
    start := Str.match_end ();
    Global.new_line ();
    Lexing.new_line lexbuf;
  done; assert false
  with Not_found ->
    COMMENT(split_comment comment)
}

and comment_multi target = parse
| "*/" { target lexbuf }
| '\n' { Global.new_line (); Lexing.new_line lexbuf; comment_multi target lexbuf }
| _  { comment_multi target lexbuf }

and string_lex re target = parse
  | '\\' {
    if !escaped
    then (bprintf buff "\\"; escaped := false; string_lex re target lexbuf)
    else (escaped := true; string_lex re target lexbuf)
  }
  | '\n' { Global.new_line (); Lexing.new_line lexbuf; bprintf buff "\n"; string_lex re target lexbuf }
  | '\"' {
    if !escaped
    then (bprintf buff "\""; escaped := false; string_lex re target lexbuf)
    else (if re then REGEXP (Buffer.contents buff) else STRING (Buffer.contents buff))
  }
  | _ as c {
    if !escaped then bprintf buff "\\";
    escaped := false;
    bprintf buff "%c" c;
    string_lex re target lexbuf
  }

(* a dedicated lexer for lexical parameter: read everything until "#END" *)
and lp_lex target = parse
| '\n'                    { Global.new_line (); Lexing.new_line lexbuf; bprintf buff "\n"; lp_lex target lexbuf }
| _ as c                  { bprintf buff "%c" c; lp_lex target lexbuf }
| "#END" [' ' '\t']* '\n' { Global.new_line ();
                            Printf.printf "********%s********\n%!" (Buffer.contents buff);
                            LEX_PAR (
                              "TODO",
                              Str.split (Str.regexp "\n") (Buffer.contents buff)
                            )
                          }

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
| '\n'       { Global.new_line (); Lexing.new_line lexbuf; global lexbuf}

| '{'   { LACC }
| '}'   { Global.label_flag := false; RACC }
| ','   { COMA }
| '|'   { PIPE }

| '@' general_ident as cmd_var     { AROBAS_ID cmd_var }
| "@#" color as col        { COLOR col }

| label_ident as id { ID id }
| '"'      { Buffer.clear buff; string_lex false global lexbuf }
| "re\""   { Buffer.clear buff; string_lex true global lexbuf }

| "]->" { Global.label_flag := false; LTR_EDGE_RIGHT }
| "]=>" { Global.label_flag := false; ARROW_RIGHT }

| _ as c { raise (Error (sprintf "unexpected character '%c'" c)) }

and standard target = parse
| [' ' '\t'] { global lexbuf }

| "%--"      { comment_multi_doc global lexbuf }
| "/*"       { comment_multi global lexbuf }
| '%'        { comment global lexbuf }

| "#BEGIN" [' ' '\t']* (label_ident as li) [' ' '\t']* '\n'
             { Printf.printf "%s\n%!" li; Global.new_line (); Buffer.clear buff; lp_lex global lexbuf}

| '\n'       { Global.new_line (); Lexing.new_line lexbuf; global lexbuf}

| "include"       { INCL }
| "import"        { IMPORT }
| "domain"        { DOMAIN }
| "features"      { FEATURES }
| "conll_fields"  { Log.fwarning "\"conll_fields\" is deprecated, ignored"; DUMMY }
| "feature"       { FEATURE }
| "file"          { FILE }
| "labels"        { Global.label_flag := true; LABELS }

| "match"         { Log.fwarning "%s \"match\" is deprecated, please use \"pattern\" instead" (Global.loc_string ()); PATTERN }
| "pattern"       { PATTERN }

| "without"       { WITHOUT }
| "commands"      { COMMANDS }

| "add_edge"      { ADD_EDGE }
| "del_edge"      { DEL_EDGE }
| "shift_in"      { SHIFT_IN }
| "shift_out"     { SHIFT_OUT }
| "shift"         { SHIFT }
| "merge"         { raise (Error "merge commnand was removed since version 0.43") }
| "del_node"      { DEL_NODE }
| "add_node"      { ADD_NODE }
| "del_feat"      { DEL_FEAT }

| "module"        { MODULE }
| "package"       { PACKAGE }
| "confluent"     { Log.fwarning "%s \"confluent\" is deprecated, please use \"deterministic\" instead" (Global.loc_string ()); DETERMINISTIC }
| "deterministic" { DETERMINISTIC }
| "rule"          { RULE }
| "strat"         { STRAT }
| "lex_rule"      { Log.fwarning "%s \"lex_rule\" is deprecated, please use \"rule\" instead" (Global.loc_string ()); RULE }
| "sequences"     { SEQUENCES }

| "Pick"          { PICK }
| "Alt"           { ALT }
| "Seq"           { SEQ }
| "Iter"          { ITER }
| "If"            { IF }
| "Try"           { TRY }
| "Emty"          { EMPTY }
| "Onf"           { ONF }

| "graph"         { GRAPH }

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

| "<<"       { LPREC }
| ">>"       { LSUCC }

| ":<"       { BEFORE }
| ":>"       { AFTER }

| "<=" | "≤" { LE }
| ">=" | "≥" { GE }

| '|'        { PIPE }
| "->"       { EDGE }
| "-[^"      { Global.label_flag := true; LTR_EDGE_LEFT_NEG }
| "-["       { Global.label_flag := true; LTR_EDGE_LEFT }
| "]->"      { LTR_EDGE_RIGHT }

| "==>"      { ARROW }
| "=["       { Global.label_flag := true; ARROW_LEFT }
| "=[^"      { Global.label_flag := true; ARROW_LEFT_NEG }
| "]=>"      { ARROW_RIGHT }

| '"'      { Buffer.clear buff; string_lex false global lexbuf }
| "re\""   { Buffer.clear buff; string_lex true global lexbuf }

| eof      { EOF }

| _ as c   { raise (Error (sprintf "unexpected character '%c'" c)) }

and const = parse
  | [' ' '\t']            { const lexbuf }
  | '\n'                  { Global.new_line (); const lexbuf}
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | [^'(' ')' ' ']+ as id { ID id }
