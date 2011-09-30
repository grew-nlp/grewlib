

(** {2 Grew's core} *)
open Utils
open Graph
open Rule
open Grs

exception Parsing_err of string
exception File_dont_exists of string

(** raised when a Gr/Grs structure fails to build *)
exception Build of string * (string * int) option

(** raised during rewriting when a command is undefined *) 
exception Run of string * (string * int) option

exception Bug of string * (string * int) option

(**/**)
type grs = Grs.t
type gr = Instance.t
type rew_history
(**/**)

val rewrite: gr:gr -> grs:grs -> seq:string -> rew_history

(** display a gr with a grs in a rew_display 
@param gr the grapth to rewrite
@param grs the graph rewriting system
@param seq the name of the sequence to apply
@return a structure {b {i easily}} displayable *)
val display: gr:gr -> grs:grs -> seq:string -> rew_display

val write_stat: string -> rew_history -> unit

val empty_grs : grs

(** get a graph rewriting system from a file 
@return a graph rewriting system
@raise Parsing_err if libgrew can't parse the file
@raise File_dont_exists if the file doesn't exists
*)
val load_grs : ?doc_output_dir:string -> string -> grs

val get_available_seq : grs -> (string * string list) list

val empty_gr : gr

(** get a graph from a file
@raise Parsing_err if libgrew can't parse the file
@raise File_dont_exists if the file doesn't exists
*)
val load_gr : string -> gr

val write_html: 
    ?no_init:bool -> ?main_feat:string -> header: string -> rew_history -> string -> unit

val error_html: 
    ?no_init:bool -> ?main_feat:string -> header: string -> string -> ?init:Instance.t -> string -> unit

val make_index: 
    title: string ->
    grs_file: string -> 
    html: bool -> 
    grs: grs -> 
    output_dir: string -> 
    base_names: string list -> 
      unit

val get_css_file: string
