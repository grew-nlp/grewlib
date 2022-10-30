(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)


open Grew_types
open Grew_utils
open Grew_ast

(* ================================================================================ *)
module Loader: sig
  val grs: string -> Ast.grs

  val basic: string -> Ast.basic

  val pattern: string -> Ast.pattern

  val phrase_structure_tree: string -> Ast.pst
end (* module Loader *)

(* ================================================================================ *)
module Parser : sig
  val basic: string -> Ast.basic

  val grs: string -> Ast.grs

  val phrase_structure_tree: string -> Ast.pst

  val pattern: string -> Ast.pattern

  val strategy: string -> Ast.strat
end (* module Parser *)
