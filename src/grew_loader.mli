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

module Loader: sig
  val domain: string -> Ast.domain

  val grs: string -> Ast.grs

  val gr: string -> Ast.gr

  val pattern: string -> Ast.pattern

  val phrase_structure_tree: string -> Ast.pst
end

module Parser : sig
  val gr: string -> Ast.gr

  val phrase_structure_tree: string -> Ast.pst

  val pattern: string -> Ast.pattern
end