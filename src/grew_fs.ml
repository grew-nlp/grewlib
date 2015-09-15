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

(* ================================================================================ *)
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
end (* module G_feature *)

(* ================================================================================ *)
module P_feature = struct
  (* feature= (feature_name, disjunction of atomic values) *)

  type cst =
    | Absent
    | Equal of value list     (* with Equal constr, the list MUST never be empty *)
    | Different of value list

  (* NB: in the current version, |in_param| ≤ 1 *)
  type v = {
    cst: cst;
    in_param: int list;  (* the list of parameters to which the value must belong *)
  }

  type t = string * v
  let dump (feature_name, {cst; in_param}) =
    printf "[P_feature.dump]\n";
    printf "%s%s\n"
      feature_name
      (match cst with
      | Different [] -> "=*"
      | Different l -> "≠" ^ (String.concat "|" (List.map string_of_value l))
      | Equal l -> "=" ^ (String.concat "|" (List.map string_of_value l))
      | Absent -> " must be Absent!");
    printf "in_param=[%s]\n" (String.concat "," (List.map string_of_int in_param));
    printf "%!"

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  exception Fail_unif

  let unif_value v1 v2 = match (v1, v2) with
    | ({cst=Absent;in_param=[]},{cst=Absent;in_param=[]}) -> v1
    | ({cst=Absent;in_param=[]},_)
    | (_,{cst=Absent;in_param=[]}) -> raise Fail_unif

    | ({cst=cst1; in_param=in1}, {cst=cst2; in_param=in2}) ->
      let cst =  match (cst1, cst2) with
        | (Equal l1, Equal l2) ->
            (match List_.sort_inter l1 l2 with
            | [] -> raise Fail_unif
            | l -> Equal l)
        | (Equal l1, Different l2)
        | (Different l2, Equal l1) ->
            (match List_.sort_diff l1 l2 with
            | [] -> raise Fail_unif
            | l -> Equal l)
        | (Different l1, Different l2) -> Different (List_.sort_union l1 l2)
        | _ -> Error.bug "[P_feature.unif_value] inconsistent match case" in
      let (in_) = match (in1,in2) with
        | (_,[]) -> (in1)
        | ([],_) -> (in2)
        | _ -> Error.build "more than one parameter constraint for the same feature in not yet implemented" in
      {cst; in_param=in_}

  let to_string ?param_names t =
    let param_string index = match param_names with
      | None -> sprintf "$%d" index
      | Some (l,_) -> sprintf "%s" (List.nth l index) in

    match t with
    | (feat_name, {cst=Absent ;in_param=[]}) -> sprintf "!%s" feat_name
    | (feat_name, {cst=Equal atoms;in_param=[]}) -> sprintf "%s=%s" feat_name (List_.to_string string_of_value "|" atoms)
    | (feat_name, {cst=Different [];in_param=[]}) -> sprintf "%s=*" feat_name
    | (feat_name, {cst=Different atoms;in_param=[]}) -> sprintf "%s≠%s" feat_name (List_.to_string string_of_value "|" atoms)

    | (feat_name, {cst=Equal atoms;in_param=[one_in]}) -> sprintf "%s=%s=$%s" feat_name (List_.to_string string_of_value "|" atoms) (param_string one_in)
    | (feat_name, {cst=Different [];in_param=[one_in]}) -> sprintf "%s=$%s" feat_name (param_string one_in)
    | (feat_name, {cst=Different atoms;in_param=[one_in]}) -> sprintf "%s≠%s^%s=%s" feat_name (List_.to_string string_of_value "|" atoms) feat_name (param_string one_in)

    | _ -> Error.bug "[P_feature.to_string] multiple parameters are not handled"

  let build ?pat_vars = function
    | ({Ast.kind=Ast.Absent; name=name}, loc) -> 
      Domain.check_feature_name ~loc name;
      (name, {cst=Absent;in_param=[];})
    | ({Ast.kind=Ast.Equality unsorted_values; name=name}, loc) ->
      let values = Domain.build ~loc name unsorted_values in (name, {cst=Equal values;in_param=[];})
    | ({Ast.kind=Ast.Disequality unsorted_values; name=name}, loc) ->
      let values = Domain.build ~loc name unsorted_values in (name, {cst=Different values;in_param=[];})
    | ({Ast.kind=Ast.Equal_param var; name=name}, loc) ->
        begin
          match pat_vars with
          | None -> Error.bug ~loc "[P_feature.build] param '%s' in an unparametrized rule" var
          | Some l ->
              match List_.index var l with
              | Some index -> (name, {cst=Different []; in_param = [index]})
              | None -> Error.build ~loc "[P_feature.build] Unknown pattern variable '%s'" var
        end
end (* module P_feature *)

(* ================================================================================ *)
module G_fs = struct
  (* list are supposed to be strictly ordered wrt compare *)
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

    let last = match (filter, position) with
      | (Some l, Some f) when List.mem "position" l && f > 0. -> [G_feature.to_string ("position", Float f)]
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

(* ================================================================================ *)
module P_fs = struct
  (* list are supposed to be striclty ordered wrt compare *)
  type t = P_feature.t list

  let empty = []

  let check_position ?param position t =
    try
      match List.assoc "position" t with
        | {P_feature.cst=P_feature.Equal pos_list; in_param=[]} -> List.mem (Float position) pos_list
        | {P_feature.cst=P_feature.Different pos_list; in_param=[]} -> not (List.mem (Float position) pos_list)
        | {P_feature.cst=P_feature.Absent} -> false
        | _ -> Error.bug "Position can't be parametrized"
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

  let match_ ?param p_fs g_fs =
    let p_fs_wo_pos =
      try List.remove_assoc "position" p_fs
      with Not_found -> p_fs in
    let rec loop acc = function
      | [], _ -> acc

      (* a feature_name present only in instance -> Skip it *)
      | ((fn_pat, fv_pat)::t_pat, (fn, _)::t) when fn_pat > fn -> loop acc ((fn_pat, fv_pat)::t_pat, t)

      (* Two next cases: p_fs requires for the absence of a feature -> OK *)
      | ((fn_pat, {P_feature.cst=P_feature.Absent})::t_pat, []) -> loop acc (t_pat, [])
      | ((fn_pat, {P_feature.cst=P_feature.Absent})::t_pat, (fn, fa)::t) when fn_pat < fn -> loop acc (t_pat, (fn, fa)::t)

      (* Two next cases: each feature_name present in p_fs must be in instance: [] means unif failure *)
      | _, [] -> raise Fail
      | ((fn_pat, _)::_, (fn, _)::_) when fn_pat < fn -> raise Fail

      (* Next cases: fn_pat = fn *)
      | ((_, {P_feature.cst=cst; P_feature.in_param=in_param})::t_pat, (_, atom)::t) ->

        (* check for the constraint part and fail if needed *)
        let () = match cst with
        | P_feature.Absent -> raise Fail
        | P_feature.Equal fv when not (List_.sort_mem atom fv) -> raise Fail
        | P_feature.Different fv when List_.sort_mem atom fv -> raise Fail
        | _ -> () in

        (* if constraint part don't fail, look for lexical parameters *)
        match (acc, in_param) with
          | (_,[]) -> loop acc (t_pat,t)
          | (None,_) -> Log.bug "[P_fs.match_] Parametrized constraint in a non-parametrized rule"; exit 2
          | (Some param, [index]) ->
            (match Lex_par.select index (string_of_value atom) param with
              | None -> raise Fail
              | Some new_param -> loop (Some new_param) (t_pat,t)
            )
          | _ -> Error.bug "[P_fs.match_] several different parameters contraints for the same feature is not implemented" in
    loop param (p_fs_wo_pos,g_fs)

  exception Fail_unif
  let unif fs1 fs2 =
    let rec loop = function
      | [], fs -> fs
      | fs, [] -> fs

      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 < fn2 -> (fn1,v1) :: (loop (t1,(fn2,v2)::t2))
      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 > fn2 -> (fn2,v2) :: (loop ((fn1,v1)::t1,t2))

      (* all remaining case are fn1 = fn2 *)
      | ((fn1,v1)::t1, (fn2,v2)::t2) (* when fn1 = fn2 *) ->
        try (fn1,P_feature.unif_value v1 v2) :: (loop (t1,t2))
        with
        | P_feature.Fail_unif -> raise Fail_unif
        | Error.Build (msg,_) -> Error.build "Feature '%s', %s" fn1 msg
    in loop (fs1, fs2)
end (* module P_fs *)
