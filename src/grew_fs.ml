open Printf
open Log

open Grew_utils
open Grew_ast

module Feature = struct
  (* feature= (feature_name, disjunction of atomic values). empty list to encode "any value" *) 
  type t = 
    | Equal of string * string list
    | Different of string * string list

  let get_name = function | Equal (n,_) -> n | Different (n,_) -> n

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)
  (* suppose all feat_names to be different and ordered *)

  let rec check ?domain loc name values = match domain with
  | None -> ()
  | Some [] -> Log.fmessage "[GRS] Unknown feature name '%s' %s" name (Loc.to_string loc)
  | Some ((Ast.Open n)::_) when n = name -> ()
  | Some ((Ast.Closed (n,vs))::_) when n = name -> 
      (match List_.sort_diff values vs with 
      | [] -> ()
      | l -> Error.build ~loc "Unknown feature values '%s' for feature name '%s'" 
	    (List_.to_string (fun x->x) ", " l)
	    name
      )
  | Some (_::t) -> check ~domain:t loc name values

  let build ?domain = function
    | ({Ast.kind=Ast.Equality;name=name;values=unsorted_values},loc) ->
	let values = List.sort Pervasives.compare unsorted_values in
	check ?domain loc name values;
	Equal (name, values)
    | ({Ast.kind=Ast.Disequality;name=name;values=unsorted_values},loc) ->
	let values = List.sort Pervasives.compare unsorted_values in
	check ?domain loc name values;
	Different (name, values)
end



module Feature_structure = struct
  (* list are supposed to be striclty ordered wrt compare*)
  type t = Feature.t list
	
  let build ?domain ast_fs =
    let unsorted = List.map (Feature.build ?domain) ast_fs in
    List.sort Feature.compare unsorted 
      
  let empty = []

  let rec get name = function
    | [] -> None
    | Feature.Equal (n,l) :: _ when n=name -> Some l
    | Feature.Equal (n,l) :: t when n<name -> get name t
    | Feature.Equal _ :: _ -> None
    | Feature.Different _ :: _ -> Log.critical "[Feature_structure.get] this fs contains 'Different' constructor"

  let get_atom name t =
    match get name t with
    | Some [one] -> Some one
    | _ -> None

  let string_of_feature = function
    | Feature.Equal (feat_name, atoms) -> 
	sprintf "%s=%s" feat_name
	  (match atoms with
	  | [] -> "*"
	  | h::t -> List.fold_right (fun atom acc -> atom^"|"^acc) t h
	  )
    | Feature.Different (feat_name, atoms) -> 
	sprintf "%s<>%s" feat_name
	  (match atoms with
	  | [] -> "EMPTY"
	  | h::t -> List.fold_right (fun atom acc -> atom^"|"^acc) t h
	  )


  let to_string t = List_.to_string string_of_feature "\\n" t

  let gr_of_feature = function
    | Feature.Equal (feat_name, [one]) -> sprintf "%s=\"%s\"" feat_name one
    | _ -> Log.critical "[Feature_structure.gr_of_feature] all feature in gr must be atomic value"

  let to_gr t = List_.to_string gr_of_feature ", " t

  let to_dep ?main_feat t = 
    let main = match main_feat with None -> "label" | Some mf -> mf in

    let wordform = 
      try 
	match 
	  (List.find 
	     (function Feature.Equal (f, _) | Feature.Different (f, _) when f=main -> true | _ -> false)
	     t
	  ) with
	| Feature.Equal (_,[ph]) | Feature.Different (_,[ph]) -> 
            Str.global_replace (Str.regexp_string  "//PV//") ";" 
              (Str.global_replace (Str.regexp_string  "//AND//") "&amp;" ph)
	| _ -> raise Not_found
      with Not_found -> "" in
    let fs = 
      Str.global_replace (Str.regexp_string  "//PV//") ";"
        (Str.global_replace (Str.regexp_string  "//AND//") "&amp;"
	   (List_.to_string string_of_feature "#" 
	      (List.filter 
		 (function Feature.Equal (f, _) | Feature.Different (f, _) when f=main -> false | _ -> true) t)
	   )
        ) in
    match fs with 
    | "" -> sprintf " word=\"%s\"; " wordform
    | s -> sprintf " word=\"%s\"; subword=\"%s\"; " wordform s


  let rec set_feat feature_name atoms = function
    | [] -> [Feature.Equal (feature_name, atoms)]
    | ((Feature.Equal (fn,_))::_) as t when feature_name < fn -> (Feature.Equal (feature_name, atoms))::t
    | (Feature.Equal (fn,_))::t when feature_name = fn -> (Feature.Equal (feature_name, atoms))::t
    | Feature.Equal (fn,ats)::t -> Feature.Equal (fn,ats):: (set_feat feature_name atoms t) 
    | _ -> Log.bug "[Feature_structure.set_feat]: Disequality not allowed in graph features"; exit 2

  let rec del_feat feature_name = function
    | [] -> []
    | ((Feature.Equal (fn,_))::_) as t when feature_name < fn -> t
    | (Feature.Equal (fn,_))::t when feature_name = fn -> t
    | Feature.Equal (fn,ats)::t -> Feature.Equal (fn,ats):: (del_feat feature_name t) 
    | _ -> Log.bug "[Feature_structure.set_feat]: Disequality not allowed in graph features"; exit 2

  (* WARNING: different from prev implem: does not fail when pattern contains a feature_name or in instance *)
  let compatible pattern fs =
    let rec loop = function
      | [], _ -> true

      (* Three next cases: each feature_name present in pattern must be in instance *)
      | _, [] -> false
      | ((Feature.Equal (fn_pat, fv_pat))::t_pat, (Feature.Equal (fn, fv))::t) when fn_pat < fn -> false
      | ((Feature.Different (fn_pat, fv_pat))::t_pat, (Feature.Equal (fn, fv))::t) when fn_pat < fn -> false

      | ((Feature.Equal (fn_pat, fv_pat))::t_pat, (Feature.Equal (fn, fv))::t) 
	when fn_pat > fn ->
	  loop ((Feature.Equal (fn_pat, fv_pat))::t_pat, t)

      | ((Feature.Different (fn_pat, fv_pat))::t_pat, (Feature.Equal (fn, fv))::t) 
	when fn_pat > fn ->
	  loop ((Feature.Different (fn_pat, fv_pat))::t_pat, t)

      | ((Feature.Equal (fn_pat, fv_pat))::t_pat, (Feature.Equal (fn, fv))::t) 
	  (* when fn_pat = fn *) -> 
 	    (match fv_pat, fv with 
	    | [],_ | _, [] -> loop (t_pat,t)
	    | l_pat,l -> not (List_.sort_disjoint l_pat l) && loop (t_pat,t)
	    )

      | ((Feature.Different (fn_pat, fv_pat))::t_pat, (Feature.Equal (fn, fv))::t) 
	  (* when fn_pat = fn*) ->
 	  (match fv_pat, fv with 
	  | [],_ | _, [] -> loop (t_pat,t)
	  | l_pat,l -> (List_.sort_disjoint l_pat l) && loop (t_pat,t)
	  )
    | _ -> Log.bug "[Feature_structure.set_feat]: Disequality not allowed in graph features"; exit 2

    in loop (pattern,fs)

  exception Fail_unif 
  exception Bug_unif of string 
  let unif fs1 fs2 = 
    let rec loop = function
      | [], fs 
      | fs, [] -> fs

      | (f1::t1, f2::t2) when Feature.compare f1 f2 < 0 -> f1 :: loop (t1, f2::t2)
      | (f1::t1, f2::t2) when Feature.compare f1 f2 > 0 -> f2 :: loop (f1::t1, t2)

      (* all remaining case are fn1 = fn2 *)
      | ((Feature.Equal (fn, fv1))::t1, (Feature.Equal (_, fv2))::t2) -> 
 	  (match List_.sort_inter fv1 fv2 with
	  | [] -> raise Fail_unif
	  | fv -> (Feature.Equal (fn, fv)) :: (loop (t1, t2)))

      | ((Feature.Different (fn, fv1))::t1, (Feature.Equal (_, fv2))::t2) -> 
 	  (match List_.sort_diff fv2 fv1 with
	  | [] -> raise Fail_unif
	  | fv -> (Feature.Equal (fn, fv)) :: (loop (t1, t2)))

      | ((Feature.Equal (fn, fv1))::t1, (Feature.Different (_, fv2))::t2) -> 
 	  (match List_.sort_diff fv1 fv2 with
	  | [] -> raise Fail_unif
	  | fv -> (Feature.Equal (fn, fv)) :: (loop (t1, t2)))

       | _ -> raise (Bug_unif "two value declared \"Feature.Different\", cannot reply without the domain !")
    in try Some (loop (fs1, fs2)) with Fail_unif -> None




  let unifiable fs1 fs2 = 
    let rec loop = function
      | [], fs 
      | fs, [] -> true

      | (f1::t1, f2::t2) when Feature.compare f1 f2 < 0 -> loop (t1, f2::t2)
      | (f1::t1, f2::t2) when Feature.compare f1 f2 > 0 -> loop (f1::t1, t2)

      (* all remaining case are fn1 = fn2 *)
      | ((Feature.Equal (fn, fv1))::t1, (Feature.Equal (_, fv2))::t2) -> 
 	  (match List_.sort_inter fv1 fv2 with
	  | [] -> false
	  | _ -> loop (t1, t2))

      | ((Feature.Different (fn, fv1))::t1, (Feature.Equal (_, fv2))::t2) -> 
 	  (match List_.sort_diff fv2 fv1 with
	  | [] -> false
	  | _ -> loop (t1, t2))

      | ((Feature.Equal (fn, fv1))::t1, (Feature.Different (_, fv2))::t2) -> 
 	  (match List_.sort_diff fv1 fv2 with
	  | [] -> false
	  | _ -> loop (t1, t2))
	    
      | _ -> raise (Bug_unif "two value declared \"Different\", cannot reply without the domain !")
    in loop (fs1, fs2)



  let filter fs_p fs_g = 
    let rec loop = function
      | [], fs -> true
      | fs, [] -> false

      | (f1::t1, f2::t2) when Feature.compare f1 f2 < 0 -> false
      | (f1::t1, f2::t2) when Feature.compare f1 f2 > 0 -> loop (f1::t1, t2)

      (* all remaining case are fn1 = fn2 *)
      | ((Feature.Equal (fn, fv1))::t1, (Feature.Equal (_, fv2))::t2) -> 
 	  (match List_.sort_inter fv1 fv2 with
	  | [] -> false
	  | _ -> loop (t1, t2))

      | ((Feature.Different (fn, fv1))::t1, (Feature.Equal (_, fv2))::t2) -> 
 	  (match List_.sort_diff fv2 fv1 with
	  | [] -> false
	  | _ -> loop (t1, t2))

      | ((Feature.Equal (fn, fv1))::t1, (Feature.Different (_, fv2))::t2) -> 
 	  (match List_.sort_diff fv1 fv2 with
	  | [] -> false
	  | _ -> loop (t1, t2))
	    
      | _ -> raise (Bug_unif "two value declared \"Different\", cannot reply without the domain !")
    in loop (fs_p, fs_g)


end
