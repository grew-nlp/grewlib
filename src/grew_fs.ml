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
open Conll

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain

  let decode_feat_name s = Str.global_replace (Str.regexp "__\\([0-9a-z]+\\)$") "[\\1]" s

(* ================================================================================ *)
module Feature_value = struct
  let build_disj ?loc ?domain name unsorted_values =
    Domain.build_disj ?loc ?domain name unsorted_values

  let build_value ?loc ?domain name value =
    match build_disj ?loc ?domain name [value] with
      | [x] -> x
      | _ -> Error.bug ?loc "[Feature_value.build_value]"
end (* module Feature_value *)

(* ================================================================================ *)
module G_feature = struct

  type t = string * value

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  (* another order used for printing purpose only *)
  let print_order = ["phon"; "form"; "cat"; "upos"; "lemma"; "pos"; "xpos"]
  let print_cmp (name1,_) (name2,_) =
    match (List_.index name1 print_order, List_.index name2 print_order) with
    | (Some i, Some j) -> Pervasives.compare i j
    | (Some i, None) -> -1
    | (None, Some j) -> 1
    | (None, None) -> Pervasives.compare name1 name2

  let build ?domain = function
    | ({Ast.kind=Ast.Equality [atom]; name=name},loc) ->
      (name, Feature_value.build_value ~loc ?domain name atom)
    | (uf,loc) -> Error.build ~loc "in graph nodes, features must follow the shape \"name = value\" (error on feature: \"%s\")" (Ast.u_feature_to_string uf)

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
      | fv::_ -> bprintf buff "<TR><TD ALIGN=\"right\">%s</TD><TD>=</TD><TD ALIGN=\"left\">%s</TD></TR>\n" (decode_feat_name feat_name) fv
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

  let to_json ?domain (feature_name, {cst}) =
    `Assoc [
      ("feature_name", `String feature_name);
      ( match cst with
        | Absent -> ("absent", `Null)
        | Equal val_list -> ("equal", `List (List.map (fun x -> `String (string_of_value x)) val_list))
        | Different val_list -> ("different", `List (List.map (fun x -> `String (string_of_value x)) val_list))
      )
    ]

  let get_name = fst

  let compare feat1 feat2 = Pervasives.compare (get_name feat1) (get_name feat2)

  exception Fail_unif

  (** raise [P_feature.Fail_unif] *)
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

  let build ?domain ?pat_vars = function
    | ({Ast.kind=Ast.Absent; name=name}, loc) ->
      Domain.check_feature_name ~loc ?domain name;
      (name, {cst=Absent;in_param=[];})
    | ({Ast.kind=Ast.Equality unsorted_values; name=name}, loc) ->
      let values = Feature_value.build_disj ~loc ?domain name unsorted_values in (name, {cst=Equal values;in_param=[];})
    | ({Ast.kind=Ast.Disequality unsorted_values; name=name}, loc) ->
      let values = Feature_value.build_disj ~loc ?domain name unsorted_values in (name, {cst=Different values;in_param=[];})
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

  (* ---------------------------------------------------------------------- *)
  let empty = []

  (* ---------------------------------------------------------------------- *)
  let set_feat ?loc ?domain feature_name atom t =
    let new_value = Feature_value.build_value ?loc ?domain feature_name atom in
    let rec loop = function
    | [] -> [(feature_name, new_value)]
    | ((fn,_)::_) as t when feature_name < fn -> (feature_name, new_value)::t
    | (fn,_)::t when feature_name = fn -> (feature_name, new_value)::t
    | (fn,a)::t -> (fn,a) :: (loop t)
    in loop t

  (* ---------------------------------------------------------------------- *)
  let del_feat = List_.sort_remove_assoc_opt

  (* ---------------------------------------------------------------------- *)
  let get_atom = List_.sort_assoc

  (* ---------------------------------------------------------------------- *)
  let get_string_atom feat_name t =
    match List_.sort_assoc feat_name t with
      | None -> None
      | Some v -> Some (conll_string_of_value v)

  (* ---------------------------------------------------------------------- *)
  let get_float_feat feat_name t =
    match List_.sort_assoc feat_name t with
      | None -> None
      | Some (Float i) -> Some i
      | Some (String s) -> Error.build "[Fs.get_float_feat] feat_name=%s, value=%s" feat_name s

  (* ---------------------------------------------------------------------- *)
  let to_string t = List_.to_string G_feature.to_string "," t

  (* ---------------------------------------------------------------------- *)
  let to_gr t = List_.to_string G_feature.to_gr ", " t

  (* ---------------------------------------------------------------------- *)
  let build ?domain ast_fs =
    let unsorted = List.map (fun feat -> G_feature.build ?domain feat) ast_fs in
    List.sort G_feature.compare unsorted

  (* ---------------------------------------------------------------------- *)
  let of_conll ?loc ?domain line =
    let (c2, c3, c4, c5) = Domain.conll_fields domain in
    let raw_list0 =
      (c2, Feature_value.build_value ?loc ?domain c2 line.Conll.form)
      :: (c4, Feature_value.build_value ?loc ?domain c4 line.Conll.upos)
      :: (List.map (fun (f,v) -> (f, Feature_value.build_value ?loc ?domain f v)) line.Conll.feats) in
    let raw_list1 = match line.Conll.xpos with
      | "" | "_" -> raw_list0
      | s -> (c5, Feature_value.build_value ?loc ?domain c5 s) :: raw_list0 in
    let raw_list2 = match line.Conll.lemma with
      | "" | "_" -> raw_list1
      | s -> (c3, Feature_value.build_value ?loc ?domain c3 s) :: raw_list1 in
    List.sort G_feature.compare raw_list2


  (* ---------------------------------------------------------------------- *)
  let pst_leaf ?loc ?domain phon = [("phon", Feature_value.build_value ?loc ?domain "phon" phon)]
  let pst_node ?loc ?domain cat = [("cat", Feature_value.build_value ?loc ?domain "cat" cat)]

  (* ---------------------------------------------------------------------- *)
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

  (* ---------------------------------------------------------------------- *)
  let get_main ?main_feat t =
    let default_list = ["phon"; "form"; "label"; "cat"; "upos"] in
    let main_list = match main_feat with
    | None -> default_list
    | Some string -> (Str.split (Str.regexp "\\( *; *\\)\\|#") string) @ default_list in
    let rec loop = function
      | [] -> (None, t)
      | feat_name :: tail ->
          match List_.sort_assoc feat_name t with
          | Some atom -> (Some (feat_name, atom), List_.sort_remove_assoc feat_name t)
          | None -> loop tail in
    loop main_list

  (* ---------------------------------------------------------------------- *)
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
    let next = List.sort G_feature.print_cmp next in
    List.iter
      (fun g_feat ->
        G_feature.buff_dot buff g_feat
      ) next;

    match Buffer.contents buff with
      | "" -> ""
      | s -> sprintf "<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">\n%s\n</TABLE>\n" s

  (* ---------------------------------------------------------------------- *)
  let to_word ?main_feat t =
    match get_main ?main_feat t with
      | (None, _) -> "#"
      | (Some (_,atom), _) -> string_of_value atom

  (* ---------------------------------------------------------------------- *)
  let escape_sharp s =
    Str.global_replace (Str.regexp "#") "__SHARP__" s

  (* ---------------------------------------------------------------------- *)
  let to_dep ?(decorated_feat=("",[])) ?position ?main_feat ?filter t =
    let (pid_name, feat_list) = decorated_feat in

    let (main_opt, sub) = get_main ?main_feat t in
    let sub = List.sort G_feature.print_cmp sub in

    let main = match main_opt with
      | None -> []
      | Some (feat_name, atom) ->
        let esc_atom = escape_sharp (string_of_value atom) in
        [ if List.mem feat_name (snd decorated_feat)
          then sprintf "%s:B:yellow" esc_atom
          else esc_atom] in

    let word_list = match pid_name with
      | "" -> main
      | _ -> (sprintf "[%s]:B:yellow" pid_name)::main in

    let word = match word_list with
      | [] -> "_"
      | l ->  String.concat "#" l in

    let last = match (!Global.debug, position) with
      | (true, Some f) -> [(G_feature.to_string ("position", Float f))^":B:lightblue"]
      | _ -> [] in

    let lines = List.fold_left
      (fun acc (feat_name, atom) ->
        let esc_atom = escape_sharp (G_feature.to_string (decode_feat_name feat_name, atom)) in
        if List.mem feat_name (snd decorated_feat)
        then (sprintf "%s:B:yellow" esc_atom) :: acc
        else
          match filter with
            | Some test when not (test feat_name) -> acc
            | _ -> esc_atom :: acc
      ) last sub in
    let subword = String.concat "#" (List.rev lines) in

    sprintf " word=\"%s\"; subword=\"%s\"" word subword

  (* ---------------------------------------------------------------------- *)
  let to_conll_string ?exclude t =
    let reduced_t = match exclude with
      | None -> t
      | Some list -> List.filter (fun (fn,_) -> not (List.mem fn list)) t in
    let ud_ordering = (* In UD CoNLL-U format, features are sorted wrt lowercase form *)
      List.sort
        (fun feat1 feat2 -> Pervasives.compare (String.lowercase_ascii (G_feature.get_name feat1)) (String.lowercase_ascii (G_feature.get_name feat2)))
        reduced_t in
    match reduced_t with
      | [] -> "_"
      | _ -> String.concat "|"
        (List.map
          (function
            | (fn, String "true") -> fn
            | (fn, fv) -> (decode_feat_name fn)^"="^(string_of_value fv))
          ud_ordering
        )

  (* ---------------------------------------------------------------------- *)
  let to_conll ?exclude t =
    let reduced_t = match exclude with
      | None -> t
      | Some list -> List.filter (fun (fn,_) -> not (List.mem fn list)) t in
    let ud_ordering = (* In UD CoNLL-U format, features are sorted wrt lowercase form *)
      List.sort
        (fun feat1 feat2 -> Pervasives.compare (String.lowercase_ascii (G_feature.get_name feat1)) (String.lowercase_ascii (G_feature.get_name feat2)))
        reduced_t in
    List.map (fun (fn, fv) -> (fn, string_of_value fv)) ud_ordering
end (* module G_fs *)

(* ================================================================================ *)
module P_fs = struct
  (* list are supposed to be striclty ordered wrt compare *)
  type t = P_feature.t list

  let empty = []

  let to_json ?domain t = `List (List.map (P_feature.to_json ?domain) t)

  let check_position ?param position t =
    try
      match (List.assoc "position" t, position) with
      | ({P_feature.cst=P_feature.Equal pos_list; in_param=[]}, Some p) -> List.mem (Float p) pos_list
      | ({P_feature.cst=P_feature.Equal pos_list; in_param=[]}, None) -> false
      | ({P_feature.cst=P_feature.Different pos_list; in_param=[]}, Some p) -> not (List.mem (Float p) pos_list)
      | ({P_feature.cst=P_feature.Different pos_list; in_param=[]}, None) -> false
      | ({P_feature.cst=P_feature.Absent}, Some _) -> false
      | ({P_feature.cst=P_feature.Absent}, None) -> true
      | _ -> Error.bug "Position can't be parametrized"
    with Not_found -> true

  let build ?domain ?pat_vars ast_fs =
    let unsorted = List.map (P_feature.build ?domain ?pat_vars) ast_fs in
    List.sort P_feature.compare unsorted

  let feat_list t = List.map P_feature.get_name t

  let to_string t = List_.to_string P_feature.to_string "\\n" t

  let to_dep ?filter param_names t =
    let reduced = match filter with
      | None -> t
      | Some test -> List.filter (fun (fn,_) -> test fn) t in
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
