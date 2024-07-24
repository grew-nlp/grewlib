(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

{
  open Printf
  
  open Grew_utils
  open Grew_parser

  exception Error of string

  let escaped = ref false

  let lexicon_lines = ref []

  let split_comment com =
    let raw = Str.split (Str.regexp "\n") com in
    List.filter (fun l -> not (Str.string_match (Str.regexp "[ \t]*$") l 0)) raw

  let buff = Buffer.create 32

  let (previous_token: token option ref) = ref None
  let push token = previous_token := Some token; token
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let float =  "-"? digit+ ('.' digit*)
let int = "-"? digit+

(* a general_ident is an arbitrary sequence of:
   - letter
   - digit
   - underscore '_'
   - dash '-'
  for basic ident construction and
   - dot '.'
   - colon ':'
   - star '*'
  The first characted cannot be a colon (to avoid confusion).
 *)
let label_ident =
  (letter | digit | '_' | '/' | '-' | '.' | '*') (letter | digit | '_' | '/' | '\'' | '-' | '.' | ':' | '@' | '*' | '$' )*

let general_ident =
  (letter | '_' ) |
  (letter | '_' | '.' ) (letter | digit | '_' | '\'' | '-' | '.' | '$')* (letter | digit | '_' | '\'' | '.' | '$')

let key_ident = (letter | digit | '_' | '\'' | '-' | '.' | '/')+

let newline = '\r' | '\n' | "\r\n"

(* ------------------------------------------------------------------------------- *)
(* Rules                                                                           *)
(* ------------------------------------------------------------------------------- *)

(* ignore everything until newline *)
rule comment target = parse
| newline { Global.new_line (); Lexing.new_line lexbuf; target lexbuf }
| eof     { EOF }
| _       { comment target lexbuf }

(* lexing inside strings "xxx" *)
and string_lex re target = parse
  | '\\' {
    if !escaped
    then (bprintf buff "\\"; escaped := false; string_lex re target lexbuf)
    else (escaped := true; string_lex re target lexbuf)
  }
  | newline { Global.new_line (); Lexing.new_line lexbuf; bprintf buff "\n"; string_lex re target lexbuf }
  | '\"'    {
    if !escaped
    then (bprintf buff "\""; escaped := false; string_lex re target lexbuf)
    else (if re then REGEXP (Grew_ast.Regexp.re (Buffer.contents buff)) else STRING (Buffer.contents buff))
  }
  | _ as c {
    if !escaped then bprintf buff "\\";
    escaped := false;
    bprintf buff "%c" c;
    string_lex re target lexbuf
  }

(* lexing inside PCRE like regexp /xxx/ *)
and pcre_lex target = parse
  | '\\' {
    if !escaped
    then (bprintf buff "\\"; escaped := false; pcre_lex target lexbuf)
    else (escaped := true; pcre_lex target lexbuf)
  }
  | newline { raise (Error "Newline are not allowed inside Pcre style regexp") }
  | "/i"    {
    if !escaped
    then (bprintf buff "\\/i"; escaped := false; pcre_lex target lexbuf)
    else REGEXP (Grew_ast.Regexp.pcri (Buffer.contents buff))
  }
  | "/"    {
    if !escaped
    then (bprintf buff "\\/"; escaped := false; pcre_lex target lexbuf)
    else REGEXP (Grew_ast.Regexp.pcre (Buffer.contents buff))
  }
  | _ as c {
    if !escaped then bprintf buff "\\";
    escaped := false;
    bprintf buff "%c" c;
    pcre_lex target lexbuf
  }

(* a dedicated lexer for local lexicons: read everything until "#END" *)
and lp_lex name target = parse
| newline { (match Global.get_line_opt () with
            | None -> raise (Error "no loc in lexer")
            | Some l -> lexicon_lines := (l, Buffer.contents buff) :: !lexicon_lines
            );
            Global.new_line ();
            Lexing.new_line lexbuf;
            Buffer.clear buff;
            lp_lex name target lexbuf
          }
| _ as c  { bprintf buff "%c" c; lp_lex name target lexbuf }
| "#END"  {
            let lines = List.rev !lexicon_lines in
            LEX_PAR (name, lines)
          }

(* The lexer must be different when label_ident are parsed.
   The [global] lexer calls either [label_parser] or [standard] depending on the flag [Global.label_flag].
   Difference are:
   - a label_ident may contain ':' (like in D:suj:obj) and ':' is a token elsewhere
   - a label_ident may contain '-' anywhere (like "--" in Tiger) but '-' is fordiden as the first or last character elsewhere
*)
and global = parse
| ""   {  if !Global.label_flag
          then label_parser global lexbuf
          else standard global lexbuf
        }

(* lexing inside label declaration, like xxx in  -[xxx]-> *)
and label_parser target = parse
| [' ' '\t'] { global lexbuf }
| '%'        { comment global lexbuf }
| newline    { Global.new_line (); Lexing.new_line lexbuf; global lexbuf}

| '{'   { LACC }
| '}'   { Global.label_flag := false; RACC }
| ','   { COMMA }
| '|'   { PIPE }
| '*'   { STAR }

| '='   { EQUAL }
| "!"   { BANG }
| "<>"  { DISEQUAL }

| label_ident as id { ID id }
| '"'      { Buffer.clear buff; string_lex false global lexbuf }
| "re\""   { Buffer.clear buff; string_lex true global lexbuf }

| '/'     { Buffer.clear buff; pcre_lex global lexbuf }

| "]->" { Global.label_flag := false; LTR_EDGE_RIGHT }
| "]=>" { Global.label_flag := false; ARROW_RIGHT }

| _ as c { raise (Error (sprintf "unexpected character '%c'" c)) }

and standard target = parse
| [' ' '\t'] { global lexbuf }

| '%'        { comment global lexbuf }

| "#BEGIN" [' ' '\t']* (label_ident as li) [' ' '\t']* newline
                  { Global.new_line ();
                    Buffer.clear buff;
                    lexicon_lines := [];
                    lp_lex li global lexbuf
                  }

| newline         { Global.new_line (); Lexing.new_line lexbuf; global lexbuf}

| "include"       { INCL }
| "import"        { IMPORT }
| "from"          { FROM }

| "pattern"       { PATTERN }

| "without"       { WITHOUT }
| "with"          { WITH }
| "commands"      { COMMANDS }
| "global"        { GLOBAL }

| "add_edge"      { ADD_EDGE }
| "del_edge"      { DEL_EDGE }
| "shift_in"      { SHIFT_IN }
| "shift_out"     { SHIFT_OUT }
| "shift"         { SHIFT }
| "del_node"      { DEL_NODE }
| "add_node"      { ADD_NODE }
| "del_feat"      { DEL_FEAT }
| "append_feats"  { APPEND_FEATS }
| "prepend_feats" { PREPEND_FEATS }
| "unorder"       { UNORDER }
| "insert"        { INSERT }

| "package"       { PACKAGE }
| "rule"          { RULE }
| "strat"         { STRAT }

| "Pick"          { PICK }
| "Alt"           { ALT }
| "Seq"           { SEQ }
| "Iter"          { ITER }
| "If"            { IF }
| "Try"           { TRY }
| "Empty"         { EMPTY }
| "Onf"           { ONF }

| float as number { FLOAT (float_of_string number) }
| int as number   { INT (int_of_string number) }

| '$' general_ident      { raise (Error "Syntax of lexicon has changed! Please read grew.fr/lexicons_change for updating instructions") }

| '*'   { STAR }

| '{'   { LACC }
| '}'   { RACC }
| '['   { LBRACKET }
| ']'   { RBRACKET }
| '('   { LPAREN }
| ')'   { RPAREN }
| ':'   { DDOT }
| ';'   { SEMIC }
| ','   { COMMA }
| '+'   { PLUS }
| "!"   { BANG }

| "<"        { LT }
| ">"        { GT }

| "<<"       { LPREC }
| ">>"       { LSUCC }

| "><"       { CROSSING }

| ":<"       { BEFORE }
| ":>"       { AFTER }

| "<=" | "≤" { LE }
| ">=" | "≥" { GE }

| '|'        { PIPE }

(* The symbol '/' should be interpreted differently dependending of the context. *)
(* The push function stores the "previous" token in a global variable *)
(* when '/' is read: *)
(*  - if the previous token is an ID, then it is SLASH, used in f=v/g=w or X.f/g *)
(*  - if the previous token is an STRING, then it is SLASH, used in f="v"/g=w *)
(*  - if the previous token is different (only EQUAL and DISEQUAL in practise), then it is the start of a Pcre regexp *)
(* NOTE: other tokens are not pushed so the previous token is not always accurate but it is when we need it! *)
| general_ident as id { push (ID id) }
| '='                 { push EQUAL }
| "<>"                { push DISEQUAL }
| '/'                 { match !previous_token with 
                        | Some (ID _) | Some (STRING _) -> SLASH 
                        | _ -> Buffer.clear buff; pcre_lex global lexbuf
                      }

| "->"       { EDGE }
| "-*->"     { EDGESTAR }
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
  | newline               { Global.new_line (); const lexbuf}
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | [^'(' ')' ' ']+ as id { ID id }

and key = parse
  | [' ' '\t']       { key lexbuf }
  | newline          { Global.new_line (); Lexing.new_line lexbuf; key lexbuf}
  | '#'              { SHARP }
  | '/'              { SLASH }
  | '='              { EQUAL }
  | '['              { LBRACKET }
  | ']'              { RBRACKET }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | ','              { COMMA }
  | "->"             { EDGE }
  | "<->"            { BIEDGE }
  | "delta"          { DELTA }
  | "length"         { LENGTH }
  | float as number  { FLOAT (float_of_string number) }
  | int as number    { FLOAT (float_of_string number) }
  | key_ident as id  { ID id }
  | eof              { EOF }
  | _ as c           { raise (Error (sprintf "unexpected character '%c'" c)) }
