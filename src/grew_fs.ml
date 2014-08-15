(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Log

open Grew_base
open Grew_types
open Grew_ast

(* ==================================================================================================== *)
module G_feature = struct

  type t = string * value

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  let build (x : Ast.feature) = match x with
    | ({Ast.kind=Ast.Equality [atom]; name=name},loc) ->
      (name, Domain.build_one ~loc name atom)
    | _ -> Error.build "Illegal feature declaration in Graph (must be '=' and atomic)"

  let to_string (feat_name, feat_val) = sprintf "%s=%s" feat_name (string_of_value feat_val)

  let to_gr (feat_name, feat_val) = sprintf "%s=\"%s\"" feat_name (string_of_value feat_val)

  let to_dot (feat_name, feat_val) =
    let string_val = string_of_value feat_val in
    match Str.split (Str.regexp ":C:") string_val with
      | [] -> Error.bug "[G_feature.to_dot] feature value '%s'" string_val
      | fv::_ -> sprintf "%s=%s" feat_name fv

  let buff_dot buff (feat_name, feat_val) =
    let string_val = string_of_value feat_val in
    match Str.split (Str.regexp ":C:") string_val with
      | [] -> Error.bug "[G_feature.to_dot] feature value '%s'" string_val
      | fv::_ -> bprintf buff "<TR><TD ALIGN=\"right\">%s</TD><TD>=</TD><TD ALIGN=\"left\">%s</TD></TR>\n" feat_name fv
end

(* ==================================================================================================== *)
module P_feature = struct
  (* feature= (feature_name, disjunction of atomic values) *)

  type v =
    | Equal of value list  (* with Equal constr, the list MUST never be empty *)
    | Different of value list
    | Param of int
    | Absent

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
    | (feat_name, Absent) -> sprintf "!%s" feat_name
    | (feat_name, Param index) ->
      match param_names with
        | None -> sprintf "%s=$%d" feat_name index
        | Some (l,_) -> sprintf "%s=%s" feat_name (List.nth l index)

  let build ?pat_vars = function
    | ({Ast.kind=Ast.Equality unsorted_values; name=name}, loc) ->
      let values = Domain.build ~loc name unsorted_values in (name, Equal values)
    | ({Ast.kind=Ast.Disequality unsorted_values; name=name}, loc) ->
      let values = Domain.build ~loc name unsorted_values in (name, Different values)
    | ({Ast.kind=Ast.Absent; name=name}, loc) -> (name, Absent)
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
    let rec loop = function
    | [] -> [(feature_name, new_value)]
    | ((fn,_)::_) as t when feature_name < fn -> (feature_name, new_value)::t
    | (fn,_)::t when feature_name = fn -> (feature_name, new_value)::t
    | (fn,a)::t -> (fn,a) :: (loop t)
    in loop t

  let del_feat = List_.sort_remove_assoc

  let get_atom = List_.sort_assoc

  let get_annot_info fs =
    match List.filter (fun (fn,_) -> String.length fn > 1 && String.sub fn 0 2 = "__") fs with
      | [] -> None
      | [(fn,_)] -> Some (String.sub fn 2 ((String.length fn) - 2))
      | _ -> Error.build "[Fs.get_annot_info] More than one annot feature in the same feature structure"

  let get_string_atom feat_name t =
    match List_.sort_assoc feat_name t with
      | None -> None
      | Some v -> Some (conll_string_of_value v)

  let get_float_feat feat_name t =
    match List_.sort_assoc feat_name t with
      | None -> None
      | Some (Float i) -> Some i
      | Some _ -> Error.build "[Fs.get_float_feat]"

  let to_string t = List_.to_string G_feature.to_string "," t
  let to_gr t = List_.to_string G_feature.to_gr ", " t

  let build ast_fs =
    let unsorted = List.map (fun feat -> G_feature.build feat) ast_fs in
    List.sort G_feature.compare unsorted

  let of_conll ?loc line =
    let unsorted_without_pos =
      ("phon", Domain.build_one ?loc "phon" line.Conll.phon)
      :: ("lemma", Domain.build_one ?loc "lemma" line.Conll.lemma)
      :: ("cat", Domain.build_one ?loc "cat" line.Conll.pos1)
      :: (List.map (fun (f,v) -> (f, Domain.build_one ?loc f v)) line.Conll.morph) in
    let unsorted = match line.Conll.pos2 with
      | "" | "_" -> unsorted_without_pos
      | s -> ("pos", Domain.build_one "pos" s) :: unsorted_without_pos in
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
    | None -> ["phon"]
    | Some string -> Str.split (Str.regexp "\\( *; *\\)\\|#") string in
    let rec loop = function
      | [] -> (None, t)
      | feat_name :: tail ->
          match List_.sort_assoc feat_name t with
          | Some atom -> (Some (feat_name, atom), List_.sort_remove_assoc feat_name t)
          | None -> loop tail in
    loop main_list

  let to_dot ?(decorated_feat=("",[])) ?main_feat t =
    let buff = Buffer.create 32 in
    let () = match (fst decorated_feat) with
      | "" -> ()
      | pid -> bprintf buff "<TR><TD COLSPAN=\"3\" BGCOLOR=\"yellow\"><B>[%s]</B></TD></TR>\n" pid in

    let next =
      match get_main ?main_feat t with
      | (None, sub) -> sub
      | (Some (feat_name,atom), sub) ->
        if List.mem feat_name (snd decorated_feat)
        then bprintf buff "<TR><TD COLSPAN=\"3\" BGCOLOR=\"yellow\"><B>%s</B></TD></TR>\n" (string_of_value atom)
        else bprintf buff "<TR><TD COLSPAN=\"3\"><B>%s</B></TD></TR>\n" (string_of_value atom);
        sub in
    List.iter
      (fun g_feat ->
        G_feature.buff_dot buff g_feat
      ) next;

    match Buffer.contents buff with
      | "" -> ""
      | s -> sprintf "<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">\n%s\n</TABLE>\n" s




  let to_word ?main_feat t =
    match get_main ?main_feat t with
      | (None, _) -> "#"
      | (Some (_,atom), _) -> string_of_value atom

  let to_dep ?(decorated_feat=("",[])) ?position ?main_feat ?filter t =
    let (main_opt, sub) = get_main ?main_feat t in
    let (pid_name, feat_list) = decorated_feat in

    let main = match main_opt with
      | None -> []
      | Some (feat_name, atom) ->
        [ if List.mem feat_name (snd decorated_feat)
          then sprintf "%s:B:yellow" (string_of_value atom)
          else string_of_value atom] in

    let word_list = match pid_name with
      | "" -> main
      | _ -> (sprintf "[%s]:B:yellow" pid_name)::main in

    let word = match word_list with
      | [] -> "_"
      | l ->  String.concat "#" l in

    let last = match position with
      | Some f when f > 0. -> [G_feature.to_string ("position", Float f)]
      | _ -> [] in

    let lines = List.fold_left
      (fun acc (feat_name, atom) ->
        if List.mem feat_name (snd decorated_feat)
        then (sprintf "%s:B:yellow" (G_feature.to_string (feat_name, atom))) :: acc
        else
          match filter with
            | Some filt_list when not (List.mem feat_name filt_list) -> acc
            | _ -> (G_feature.to_string (feat_name, atom)) :: acc
      ) last sub in
    let subword = String.concat "#" (List.rev lines) in

    sprintf " word=\"%s\"; subword=\"%s\"" word subword

  let to_conll ?exclude t =
    let reduced_t = match exclude with
      | None -> t
      | Some list -> List.filter (fun (fn,_) -> not (List.mem fn list || fn.[0]='_')) t in
    match reduced_t with
      | [] -> "_"
      | _ -> String.concat "|"
        (List.map
           (function (fn, String "true") -> fn | (fn, fv) -> fn^"="^(string_of_value fv))
           reduced_t
        )
end (* module G_fs *)

(* ==================================================================================================== *)
module P_fs = struct
  (* list are supposed to be striclty ordered wrt compare*)
  type t = P_feature.t list

  let empty = []

  let check_position ?param position t =
    try
      match List.assoc "position" t with
        | P_feature.Equal pos_list -> List.mem (Float position) pos_list
        | P_feature.Different pos_list -> not (List.mem (Float position) pos_list)
        | P_feature.Absent -> false
        | P_feature.Param index ->
          match param with
            | Some p -> float_of_string (Lex_par.get_param_value index p) = position
            | None -> Log.bug "[P_fs.check_position] Illegal parametrized pattern feature"; exit 2
    with Not_found -> true

  let build ?pat_vars ast_fs =
    let unsorted = List.map (P_feature.build ?pat_vars) ast_fs in
    List.sort P_feature.compare unsorted

  let feat_list t = List.map P_feature.get_name t

  let to_string t = List_.to_string P_feature.to_string "\\n" t

  let to_dep ?filter param_names t =
    let reduced = match filter with
      | None -> t
      | Some l -> List.filter (fun (fn,_) -> List.mem fn l) t in
    List_.to_string (P_feature.to_string ~param_names) "#" reduced

  let to_dot t = List_.to_string P_feature.to_string "\\n" t

  exception Fail

  let match_ ?param pattern fs =
    let pattern_wo_pos =
      try List.remove_assoc "position" pattern
      with Not_found -> pattern in
    let rec loop acc = function
      | [], _ -> acc

      (* a feature_name present only in instance -> Skip it *)
      | ((fn_pat, fv_pat)::t_pat, (fn, _)::t) when fn_pat > fn -> loop acc ((fn_pat, fv_pat)::t_pat, t)

      (* Three next cases: pattern requires for the absence of a feature. case 1&2: OK, go on, 3: fail *)
      | ((fn_pat, P_feature.Absent)::t_pat, []) -> loop acc (t_pat, [])
      | ((fn_pat, P_feature.Absent)::t_pat, (fn, fa)::t) when fn_pat < fn -> loop acc (t_pat, (fn, fa)::t)
      | ((fn_pat, P_feature.Absent)::t_pat, (fn, fa)::t) when fn_pat = fn -> raise Fail

      (* Two next cases: each feature_name present in pattern must be in instance: [] means unif failure *)
      | _, [] -> raise Fail
      | ((fn_pat, _)::_, (fn, _)::_) when fn_pat < fn -> raise Fail

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
    in loop param (pattern_wo_pos,fs)

  let filter fs_p fs_g =
    let rec loop = function
      | [], fs -> true

      | ((fn1,_)::_ as f1, (fn2,_)::t2) when fn1 > fn2 -> loop (f1, t2)

      | ((fn1,P_feature.Absent)::t1, []) -> loop (t1,[])
      | ((fn1,P_feature.Absent)::t1, ((fn2,_)::_ as f2)) when fn1 < fn2 -> loop (t1,f2)
      | ((fn1,P_feature.Absent)::t1, (fn2,_)::_) when fn1 = fn2 -> false

      | fs, [] -> false

      | ((fn1,_)::_, (fn2,_)::_) when fn1 < fn2 -> false

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
