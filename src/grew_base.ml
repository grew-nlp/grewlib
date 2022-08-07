(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

module String_set = Set.Make (String)

module String_map = Map.Make (String)

module String_opt_map = Map.Make (struct type t = string option let compare = compare end)

module Int_set = Set.Make (struct type t = int let compare = Stdlib.compare end)

module Int_map = Map.Make (struct type t = int let compare = Stdlib.compare end)

let to_uname = function
  | "cat" -> "upos"
  | "pos" -> "xpos"
  | "phon" -> "form"
  | x -> x

let (<<) f g x = f (g x)

type cmp = Eq | Neq
let string_of_cmp = function Eq -> "=" | Neq -> "<>"
let cmp_fct cmp = match cmp with Eq -> (=) | Neq -> (<>)

