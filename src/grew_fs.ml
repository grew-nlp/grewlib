open Printf
open Log

open Grew_utils
open Grew_ast

(* ==================================================================================================== *)
module Domain = struct
  let current = ref None

  let reset () = current := None

  let init ast_domain = current := Some ast_domain

  let check ?loc name values = match !current with
  | None -> ()
  | Some d ->
      let rec loop = function
        | [] -> Error.build ?loc "[GRS] Unknown feature name '%s'" name
        | ((Ast.Open n)::_) when n = name -> ()
        | ((Ast.Closed (n,vs))::_) when n = name -> 
            (match List_.sort_diff values vs with 
            | [] -> ()
            | l -> Error.build ?loc "Unknown feature values '%s' for feature name '%s'" 
	          (List_.to_string (fun x->x) ", " l)
	          name
            )
        | _::t -> loop t in
      loop d
end

(* ==================================================================================================== *)
module G_feature = struct
  type t = string * string

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  let build = function
    | ({Ast.kind=Ast.Equality [atom]; name=name},loc) ->
	Domain.check ~loc name [atom];
	(name, atom)
    | _ -> Error.build "Illegal feature declaration in Graph (must be '=' and atomic)"

  let to_string (feat_name, value) = sprintf "%s=\"%s\"" feat_name value

  let to_dep (feat_name, value) = sprintf "%s=%s" feat_name value
end

(* ==================================================================================================== *)
module P_feature = struct
  (* feature= (feature_name, disjunction of atomic values) *) 

  type v = 
    | Equal of string list  (* with Equal constr, the list is MUST never be empty *)
    | Different of string list
    | Param of int 

  type t = string * v

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  let to_string = function
    | (feat_name, Equal atoms) -> sprintf "%s=%s" feat_name (List_.to_string (fun x->x) "|" atoms)
    | (feat_name, Different []) -> sprintf "%s=*" feat_name
    | (feat_name, Different atoms) -> sprintf "%s<>%s" feat_name (List_.to_string (fun x->x) "|" atoms)
    | (feat_name, Param index) -> sprintf "%s=$%d" feat_name index 

  let build ?pat_vars = function
    | ({Ast.kind=Ast.Equality unsorted_values; name=name}, loc) ->
	let values = List.sort Pervasives.compare unsorted_values in
	Domain.check ~loc name values;
	(name, Equal values)
    | ({Ast.kind=Ast.Disequality unsorted_values; name=name}, loc) ->
	let values = List.sort Pervasives.compare unsorted_values in
	Domain.check ~loc name values;
	(name, Different values)
    | ({Ast.kind=Ast.Param var; name=name}, loc) ->
        match pat_vars with
        | None -> Error.bug ~loc "[P_feature.build] param '%s' in an unparametrized rule" var
        | Some l -> 
            match List_.pos var l with
            | Some index -> (name, Param index)
            | None -> Error.build ~loc "[P_feature.build] Unknown pattern variable '%s'" var
end

(* ==================================================================================================== *)
module G_fs = struct
  (* list are supposed to be striclty ordered wrt compare*)
  type t = G_feature.t list

  let empty = []

  let set_feat ?loc feature_name atom t =
    Domain.check ?loc feature_name [atom];
    let rec loop = function
    | [] -> [(feature_name, atom)]
    | ((fn,_)::_) as t when feature_name < fn -> (feature_name, atom)::t
    | (fn,_)::t when feature_name = fn -> (feature_name, atom)::t
    | (fn,a)::t -> (fn,a) :: (loop t) 
    in loop t

  let del_feat = List_.sort_remove_assoc

  let get_atom = List_.sort_assoc

  let to_gr t = List_.to_string G_feature.to_string ", " t

  let to_string t = List_.to_string G_feature.to_string "\\n" t

  let build ast_fs =
    let unsorted = List.map (fun feat -> G_feature.build feat) ast_fs in
    List.sort G_feature.compare unsorted

  let of_conll line =
    let unsorted = ("phon", line.Conll.phon) :: ("lemma", line.Conll.lemma) :: ("cat", line.Conll.pos2) :: line.Conll.morph in
    List.sort G_feature.compare unsorted

  exception Fail_unif 
  let unif fs1 fs2 = 
    let rec loop = function
      | [], fs | fs, [] -> fs
      | (f1::t1, f2::t2) when G_feature.compare f1 f2 < 0 -> f1 :: loop (t1, f2::t2)
      | (f1::t1, f2::t2) when G_feature.compare f1 f2 > 0 -> f2 :: loop (f1::t1, t2)
                                                                    
      (* all remaining case are fn1 = fn2 *)
      | ((fn, a1)::t1, (_, a2)::t2) when a1=a2 -> (fn,a1) :: (loop (t1, t2))
      | _ -> raise Fail_unif
    in try Some (loop (fs1, fs2)) with Fail_unif -> None

  let get_main ?main_feat t =
    let main_list = match main_feat with
    | None -> []
    | Some string -> Str.split (Str.regexp " *; *") string in

    let rec loop = function
      | [] -> (None, t)
      | feat_name :: tail ->
          match List_.sort_assoc feat_name t with
          | Some atom -> (Some atom, List_.sort_remove_assoc feat_name t)
          | None -> loop tail in
    loop main_list

  let to_dot ?main_feat t =
    match get_main ?main_feat t with
    | (None, _) -> List_.to_string G_feature.to_string "\\n" t
    | (Some atom, sub) -> sprintf "%s|%s" atom (List_.to_string G_feature.to_string "\\n" sub)
          
  let to_dep ?main_feat t =
    let (main_opt, sub) = get_main ?main_feat t in
    sprintf " word=\"%s\"; subword=\"%s\"; " 
      (match main_opt with Some atom -> atom | None -> "")
      (List_.to_string G_feature.to_dep "#" sub)
end
 
(* ==================================================================================================== *)
module P_fs = struct
  (* list are supposed to be striclty ordered wrt compare*)
  type t = P_feature.t list

  let empty = []

  let build ?pat_vars ast_fs =
    let unsorted = List.map (P_feature.build ?pat_vars) ast_fs in
    List.sort P_feature.compare unsorted 

  let to_string t = List_.to_string P_feature.to_string "\\n" t

  let to_dot t = List_.to_string P_feature.to_string "\\n" t

  exception Fail

  let match_ ?param pattern fs =
    let rec loop acc = function
      | [], _ -> acc

      (* Two next cases: each feature_name present in pattern must be in instance: [] means unif failure *)
      | _, [] -> raise Fail
      | ((fn_pat, _)::_, (fn, _)::_) when fn_pat < fn -> raise Fail

      (* a feature_name present only in instance -> Skip it *)
      | ((fn_pat, fv_pat)::t_pat, (fn, _)::t) when fn_pat > fn -> loop acc ((fn_pat, fv_pat)::t_pat, t)

      (* Next cases: fn_pat = fn *)
      | ((_, (P_feature.Equal fv))::t_pat, (_, fa)::t) when List_.sort_mem fa fv -> loop acc (t_pat,t)
      | ((_, (P_feature.Different fv))::t_pat, (_, fa)::t) when not (List_.sort_mem fa fv) -> loop acc (t_pat,t)

      | ((_, (P_feature.Param index))::t_pat, (_, atom)::t) ->
          (match acc with
          | None -> Log.bug "[P_fs.compatible] Illegal parametrized pattern feature"; exit 2
          | Some param ->
              (match Lex_par.filter index atom param with
              | None -> raise Fail
              | Some new_param -> loop (Some new_param) (t_pat,t)
              )
          )

      (* remaining cases: Equal and not list_mem  |  Diff and not list_mem -> fail*)  
      | _ -> raise Fail
    in loop param (pattern,fs)

  let filter fs_p fs_g = 
    let rec loop = function
      | [], fs -> true
      | fs, [] -> false

      | ((fn1,_)::_, (fn2,_)::_) when fn1 < fn2 -> false
      | ((fn1,_)::_ as f1, (fn2,_)::t2) when fn1 > fn2 -> loop (f1, t2)

      (* all remaining case are fn1 = fn2 *)
      | ((_, (P_feature.Equal fv))::t1, (_, atom)::t2) when List_.sort_mem atom fv -> loop (t1, t2)
      | ((_, (P_feature.Different fv))::t1, (_, atom)::t2) when not (List_.sort_mem atom fv) -> loop (t1, t2)
      | _ -> false

    in loop (fs_p, fs_g)
end
