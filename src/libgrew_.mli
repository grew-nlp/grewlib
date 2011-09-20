(* include Grew_types *)

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
(**/**)

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

(** rewrite a gr with a grs in a rew_display 
@param gr the grapth to rewrite
@param grs the graph rewriting system
@param seq the name of the sequence to apply
@return a structure {b {i easily}} displayable *)
val rewrite : gr:gr -> grs:grs -> seq:string -> rew_display

val rules_stat: string -> Grs.t -> string -> string ->  (string * int) list

IFDEF DEP2PICT THEN


val rewrite_to_html_intern :
	?no_init:bool -> ?main_feat:string -> string -> Grs.t -> string -> string -> string -> int -> string -> string -> (string * string list) list option
	
val rewrite_to_html :
	?main_feat:string -> string -> string -> string -> bool -> string -> Grs.t -> string -> string -> unit 
ENDIF

val get_css_file: string
