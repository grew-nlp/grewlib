open Printf
open Log

open Grew_utils
open Grew_ast


type value = String of string | Int of int

let string_of_value = function
  | String s -> s
  | Int i -> string_of_int i

(* ==================================================================================================== *)
module Domain = struct
  let current = ref None

  let reset () = current := None

  let init ast_domain = current := Some ast_domain

  let check ?loc name values =
    if name.[0] <> '_'
    then
    match (name.[0], !current) with
      | ('_', _)
      | (_,None) -> ()
      | (_, Some d) ->
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

  let build ?loc name values =
    match (name.[0], !current) with
      | ('_', _)
      | (_, None) -> List.map (fun s -> String s) values (* no check on feat_name starting with '_' *)
      | (_, Some d) ->
        let rec loop = function
          | [] -> Error.build ?loc "[GRS] Unknown feature name '%s'" name
          | ((Ast.Open n)::_) when n = name ->
            List.map (fun s -> String s) values
          | ((Ast.Int n)::_) when n = name ->
            (try List.map (fun s -> Int (int_of_string s)) values
            with Failure _ -> Error.build ?loc "[GRS] The feature '%s' is of type int" name)
          | ((Ast.Closed (n,vs))::_) when n = name ->
            (match List_.sort_diff values vs with
              | [] -> List.map (fun s -> String s) values
              | l -> Error.build ?loc "Unknown feature values '%s' for feature name '%s'"
	        (List_.to_string (fun x->x) ", " l)
	        name
            )
          | _::t -> loop t in
        loop d

  let build_one ?loc name value =
    match build ?loc name [value] with
      | [x] -> x
      | _ -> Error.bug ?loc "[Domain.build_one]"
end

(* ==================================================================================================== *)
module G_feature = struct

  type t = string * value

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  let build (x : Ast.feature) = match x with
    | ({Ast.kind=Ast.Equality [atom]; name=name},loc) ->
	(* Domain.check ~loc name [atom]; *)
	(* (name, atom) *)
      (name, Domain.build_one ~loc name atom)
    | _ -> Error.build "Illegal feature declaration in Graph (must be '=' and atomic)"

  let to_string (feat_name, feat_val) = sprintf "%s=%s" feat_name (string_of_value feat_val)

  let to_gr (feat_name, feat_val) =
    match feat_val with
      | String s -> sprintf "%s=\"%s\"" feat_name s
      | Int i -> sprintf "%s=\"%d\"" feat_name i
      
  let to_dot (feat_name, feat_val) =
    match feat_val with
      | Int i -> sprintf "%s=%d" feat_name i
      | String s ->
        match Str.split (Str.regexp ":C:") s with
          | [] -> Error.bug "[G_feature.to_dot] feature value '%s'" s
          | fv::_ -> sprintf "%s=%s" feat_name fv
end

(* ==================================================================================================== *)
module P_feature = struct
  (* feature= (feature_name, disjunction of atomic values) *) 

  type v = 
    | Equal of value list  (* with Equal constr, the list MUST never be empty *)
    | Different of value list
    | Param of int 

  type t = string * v

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  let unif_value v1 v2 = 
    match (v1, v2) with
    | (Equal l1, Equal l2) -> 
        (match List_.sort_inter l1 l2 with
        | [] -> Error.build "Unification failure" 
        | l -> Equal l)
    | (Different l1, Different l2) -> Different (List_.sort_union l1 l2)
    | _ -> Error.build "cannot unify heterogeneous pattern features"
        
  let to_string ?param_names = function
    | (feat_name, Equal atoms) -> sprintf "%s=%s" feat_name (List_.to_string string_of_value "|" atoms)
    | (feat_name, Different []) -> sprintf "%s=*" feat_name
    | (feat_name, Different atoms) -> sprintf "%s<>%s" feat_name (List_.to_string string_of_value "|" atoms)
    | (feat_name, Param index) -> 
      match param_names with
        | None -> sprintf "%s=$%d" feat_name index 
        | Some (l,_) -> sprintf "%s=%s" feat_name (List.nth l index)

  let build ?pat_vars = function
    | ({Ast.kind=Ast.Equality unsorted_values; name=name}, loc) ->
      let values = Domain.build ~loc name unsorted_values in (name, Equal values)
	(* let values = List.sort Pervasives.compare unsorted_values in *)
	(* Domain.check ~loc name values; *)
	(* (name, Equal values) *)
    | ({Ast.kind=Ast.Disequality unsorted_values; name=name}, loc) ->
      let values = Domain.build ~loc name unsorted_values in (name, Different values)
	(* let values = List.sort Pervasives.compare unsorted_values in *)
	(* Domain.check ~loc name values; *)
	(* (name, Different values) *)
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

  let to_raw t = List.map (fun (name, value) -> (name, string_of_value value)) t

  let empty = []

  let set_feat ?loc feature_name atom t =
    let new_value = Domain.build_one ?loc feature_name atom in
    (* Domain.check ?loc feature_name [atom]; *)
    let rec loop = function
    | [] -> [(feature_name, new_value)]
    | ((fn,_)::_) as t when feature_name < fn -> (feature_name, new_value)::t
    | (fn,_)::t when feature_name = fn -> (feature_name, new_value)::t
    | (fn,a)::t -> (fn,a) :: (loop t) 
    in loop t

  let del_feat = List_.sort_remove_assoc

  let get_atom = List_.sort_assoc

  let get_string_atom feat_name t = 
    match List_.sort_assoc feat_name t with
      | None -> None
      | Some v -> Some (string_of_value v)

  let get_int_feat feat_name t =
    match List_.sort_assoc feat_name t with
      | None -> None
      | Some (Int i) -> Some i
      | Some _ -> Error.build "[Fs.get_int_feat]"

  let to_string t = List_.to_string G_feature.to_string "," t
  let to_gr t = List_.to_string G_feature.to_gr ", " t

  let build ast_fs =
    let unsorted = List.map (fun feat -> G_feature.build feat) ast_fs in
    List.sort G_feature.compare unsorted

  let of_conll line =
    let unsorted =
      ("phon", String line.Conll.phon)
      :: ("lemma", String line.Conll.lemma)
      :: ("cat", String line.Conll.pos1)
      :: ("pos", String line.Conll.pos2)
      :: ("num", Int line.Conll.num)
      :: (List.map (fun (f,v) -> (f, String v)) line.Conll.morph) in
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
    | Some string -> Str.split (Str.regexp "\\( *; *\\)\\|#") string in

    let rec loop = function
      | [] -> (None, t)
      | feat_name :: tail ->
          match List_.sort_assoc feat_name t with
          | Some atom -> (Some atom, List_.sort_remove_assoc feat_name t)
          | None -> loop tail in
    loop main_list

  let to_dot ?main_feat t =
    match get_main ?main_feat t with
    | (None, _) -> List_.to_string G_feature.to_dot "\\n" t
    | (Some atom, sub) ->
      sprintf "{%s|%s}" (string_of_value atom) (List_.to_string G_feature.to_dot "\\n" sub)
          
  let to_word ?main_feat t =
    match get_main ?main_feat t with
      | (None, _) -> "#"
      | (Some atom, _) -> string_of_value atom
        
  let to_dep ?main_feat t =
    let (main_opt, sub) = get_main ?main_feat t in
    sprintf " word=\"%s\"; subword=\"%s\"; " 
      (match main_opt with Some atom -> string_of_value atom | None -> "")
      (List_.to_string G_feature.to_string "#" sub)
end (* module G_fs *)
 
(* ==================================================================================================== *)
module P_fs = struct
  (* list are supposed to be striclty ordered wrt compare*)
  type t = P_feature.t list

  let empty = []

  let build ?pat_vars ast_fs =
    let unsorted = List.map (P_feature.build ?pat_vars) ast_fs in
    List.sort P_feature.compare unsorted 

  let to_string t = List_.to_string P_feature.to_string "\\n" t

  let to_dep param_names t = List_.to_string (P_feature.to_string ~param_names) "#" t

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
              (match Lex_par.filter index (string_of_value atom) param with
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

  let unif fs1 fs2 = 
    let rec loop = function
      | [], fs -> fs
      | fs, [] -> fs

      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 < fn2 -> (fn1,v1) :: (loop (t1,(fn2,v2)::t2))
      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 > fn2 -> (fn2,v2) :: (loop ((fn1,v1)::t1,t2))

      (* all remaining case are fn1 = fn2 *)
      | ((fn1,v1)::t1, (fn2,v2)::t2) (* when fn1 = fn2 *) -> (fn1,P_feature.unif_value v1 v2) :: (loop (t1,t2))
    in loop (fs1, fs2)      
end
