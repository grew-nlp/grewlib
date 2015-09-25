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

  (* message and location *)
  exception Error of (string * Loc.t option)

  val grs: string -> Ast.grs

  val gr: string -> Ast.gr

  val pattern: string -> Ast.pattern
end
