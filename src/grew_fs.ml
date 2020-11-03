(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
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

  type t = feature_name * feature_value

  let get_name = fst

  let compare feat1 feat2 = Stdlib.compare (get_name feat1) (get_name feat2)

  (* another order used for printing purpose only *)
  let print_order = ["phon"; "form"; "cat"; "upos"; "lemma"; "pos"; "xpos"]
  let print_cmp (name1,_) (name2,_) =
    match (List_.index_opt name1 print_order, List_.index_opt name2 print_order) with
    | (Some i, Some j) -> Stdlib.compare i j
    | (Some i, None) -> -1
    | (None, Some j) -> 1
    | (None, None) -> Stdlib.compare name1 name2

  let build ?domain = function
    | ({Ast.kind=Ast.Equality [atom]; name=name},loc) ->
      (name, Feature_value.build_value ~loc ?domain name atom)
    | (uf,loc) -> Error.build ~loc "in graph nodes, features must follow the shape \"name = value\" (error on feature: \"%s\")" (Ast.u_feature_to_string uf)

  let to_string (feat_name, feat_val) = sprintf "%s=%s" feat_name (string_of_value feat_val)

  let to_gr (feat_name, feat_val) = sprintf "%s=\"%s\"" feat_name (string_of_value feat_val)

  let to_json (feat_name, feat_val) = (feat_name, `String (conll_string_of_value feat_val))

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

  type p_feature_value =
    | Absent
    | Equal of feature_value list     (* with Equal constr, the list MUST never be empty *)
    | Different of feature_value list
    | Equal_lex of string * string
    | Different_lex of string * string
    | Else of (feature_value * feature_name * feature_value)

  type t = feature_name * p_feature_value

  let get_name = fst

  let compare feat1 feat2 = Stdlib.compare (get_name feat1) (get_name feat2)

  let dump (feature_name, p_feature_value) =
    printf "[P_feature.dump]\n";
    printf "%s%s\n"
      feature_name
      (match p_feature_value with
       | Different [] -> "=*"
       | Different l -> "≠" ^ (String.concat "|" (List.map string_of_value l))
       | Equal l -> "=" ^ (String.concat "|" (List.map string_of_value l))
       | Equal_lex (lex,fn) -> sprintf "= %s.%s" lex fn
       | Different_lex (lex,fn) -> sprintf "≠ %s.%s" lex fn
       | Absent -> " must be Absent!"
       | Else (fv1,fn2,fv2) -> sprintf " = %s/%s = %s" (string_of_value fv1) fn2 (string_of_value fv2));

    printf "%!"

  let to_json_python ?domain (feature_name, p_feature_value) =
    `Assoc [
      ("feature_name", `String feature_name);
      ( match p_feature_value with
        | Absent -> ("absent", `Null)
        | Equal val_list -> ("equal", `List (List.map (fun x -> `String (string_of_value x)) val_list))
        | Different val_list -> ("different", `List (List.map (fun x -> `String (string_of_value x)) val_list))
        | Equal_lex (lex,fn) -> ("equal_lex", `String (sprintf "%s.%s" lex fn))
        | Different_lex (lex,fn) -> ("different_lex", `String (sprintf "%s.%s" lex fn))
        | Else (fv1,fn2,fv2) -> ("else", `List [`String (string_of_value fv1); `String fn2; `String (string_of_value fv2)]);
      )
    ]


  exception Fail_unif

  (** raise [P_feature.Fail_unif] *)
  let unif_value v1 v2 = match (v1, v2) with
    | (Absent, Absent) -> v1
    | (Absent, _)
    | (_, Absent) -> raise Fail_unif
    | (Equal l1, Equal l2) ->
      begin
        match List_.sort_inter l1 l2 with
        | [] -> raise Fail_unif
        | l -> Equal l
      end
    | (Equal l1, Different l2)
    | (Different l2, Equal l1) ->
      begin
        match List_.sort_diff l1 l2 with
        | [] -> raise Fail_unif
        | l -> Equal l
      end
    | (Different l1, Different l2) -> Different (List_.sort_union l1 l2)
    | _ -> Error.bug "[P_feature.unif_value] inconsistent match case"

  let to_string = function
    | (feat_name, Absent) -> sprintf "!%s" feat_name
    | (feat_name, Equal atoms) -> sprintf "%s=%s" feat_name (List_.to_string string_of_value "|" atoms)
    | (feat_name, Different []) -> sprintf "%s=*" feat_name
    | (feat_name, Different atoms) -> sprintf "%s≠%s" feat_name (List_.to_string string_of_value "|" atoms)
    | (feat_name, Equal_lex (lex,fn)) -> sprintf "%s=%s.%s" feat_name lex fn
    | (feat_name, Different_lex (lex,fn)) -> sprintf "%s<>%s.%s" feat_name lex fn
    | (feat_name, Else (fv1,fn2,fv2)) -> sprintf "%s=%s/%s=%s" feat_name (string_of_value fv1) fn2 (string_of_value fv2)

  let build ?domain lexicons = function
    | ({Ast.kind=Ast.Absent; name=name}, loc) ->
      Domain.check_feature_name ~loc ?domain name;
      (name, Absent)
    | ({Ast.kind=Ast.Equality unsorted_values; name=name}, loc) ->
      let values = Feature_value.build_disj ~loc ?domain name unsorted_values in (name, Equal values)
    | ({Ast.kind=Ast.Disequality unsorted_values; name=name}, loc) ->
      let values = Feature_value.build_disj ~loc ?domain name unsorted_values in (name, Different values)
    | ({Ast.kind=Ast.Equal_lex (lex,fn); name=name}, loc) ->
      Lexicons.check ~loc lex fn lexicons;
      (name, Equal_lex (lex,fn) )
    | ({Ast.kind=Ast.Disequal_lex (lex,fn); name=name}, loc) ->
      Lexicons.check ~loc lex fn lexicons;
      (name, Different_lex (lex,fn) )
    | ({Ast.kind=Ast.Else (fv1,fn2,fv2); name=name}, loc) ->
      let v1 = match Feature_value.build_disj ~loc ?domain name [fv1] with [one] -> one | _ -> failwith "BUG Else" in
      let v2 = match Feature_value.build_disj ~loc ?domain name [fv2] with [one] -> one | _ -> failwith "BUG Else" in
      (name, Else (v1,fn2,v2))
end (* module P_feature *)

(* ================================================================================ *)
module G_fs = struct
  (* list are supposed to be strictly ordered wrt compare *)
  type t = G_feature.t list

  (* ---------------------------------------------------------------------- *)
  let empty = []

  (* ---------------------------------------------------------------------- *)
  let get_features t = List.fold_left (fun acc (feat_name,_) -> String_set.add feat_name acc) String_set.empty t

  (* ---------------------------------------------------------------------- *)
  let set_value ?loc ?domain feature_name value t =
    let rec loop = function
      | [] -> [(feature_name, value)]
      | ((fn,_)::_) as t when feature_name < fn -> (feature_name, value)::t
      | (fn,_)::t when feature_name = fn -> (feature_name, value)::t
      | (fn,a)::t -> (fn,a) :: (loop t)
    in loop t

  (* ---------------------------------------------------------------------- *)
  let set_atom ?loc ?domain feature_name atom t =
    let value = Feature_value.build_value ?loc ?domain feature_name atom in
    set_value ?loc ?domain feature_name value t

  (* ---------------------------------------------------------------------- *)
  let del_feat_opt = List_.sort_remove_assoc_opt

  (* ---------------------------------------------------------------------- *)
  let get_value_opt = List_.sort_assoc_opt

  (* ---------------------------------------------------------------------- *)
  let to_string t = List_.to_string G_feature.to_string "," t

  (* ---------------------------------------------------------------------- *)
  let to_gr t = List_.to_string G_feature.to_gr ", " t

  (* ---------------------------------------------------------------------- *)
  let to_json t = List.map G_feature.to_json t

  (* ---------------------------------------------------------------------- *)
  let of_ast ?domain ast_fs =
    let unsorted = List.map (fun feat -> G_feature.build ?domain feat) ast_fs in
    List.sort G_feature.compare unsorted

  (* ---------------------------------------------------------------------- *)
  let of_items items =
    let unsorted = List.map (fun (f,v) -> (f, typed_vos f v)) items in
    List.sort G_feature.compare unsorted

  (* ---------------------------------------------------------------------- *)
  let pst_leaf ?loc ?domain form = [("form", Feature_value.build_value ?loc ?domain "form" form)]
  let pst_node ?loc ?domain upos = [("upos", Feature_value.build_value ?loc ?domain "upos" upos)]

  (* ---------------------------------------------------------------------- *)
  let concat_values ?loc separator v1 v2 =
    match (v1, v2) with
    | (String v1, String v2) -> String (v1 ^ separator ^ v2)
    | _ -> Error.run "Cannot concat numerical values"

  (* ---------------------------------------------------------------------- *)
  let append_feats_opt ?loc src tar separator regexp =
    match List.filter
            (fun (feature_name,_) ->
               match feature_name with
               | "form" | "lemma" | "upos" | "xpos" -> false
               | _ -> String_.re_match (Str.regexp regexp) feature_name
            ) src with
    | [] -> None
    | sub_src ->
      let (new_tar, updated_feats) = List.fold_left
          (fun (acc_tar, acc_updated_feats) (feat, value) ->
             match List_.sort_assoc_opt feat tar with
             | None -> (set_value ?loc feat value acc_tar, (feat, value)::acc_updated_feats)
             | Some v ->
               let new_value = concat_values ?loc separator v value in
               (set_value ?loc feat new_value acc_tar, (feat, new_value)::acc_updated_feats)
          ) (tar,[]) sub_src in
      Some (new_tar, updated_feats)

  (* ---------------------------------------------------------------------- *)
  let get_main ?main_feat t =
    let default_list = ["form"] in
    let main_list = match main_feat with
      | None -> default_list
      | Some string -> (Str.split (Str.regexp "\\( *; *\\)\\|#") string) @ default_list in
    let rec loop = function
      | [] -> (None, t)
      | feat_name :: tail ->
        match List_.sort_assoc_opt feat_name t with
        | Some atom -> (Some (feat_name, atom), List_.sort_remove_assoc feat_name t)
        | None -> loop tail in
    loop main_list

  (* ---------------------------------------------------------------------- *)
  let to_dot ?(decorated_feat=("",[])) ?main_feat t =
    let (pid_name, highlighted_feat_list) = decorated_feat in

    let is_highlithed feat_name =
      (List.mem_assoc feat_name highlighted_feat_list) ||
      (List.exists (function
           | (f, Some g) when g = feat_name && (not (List.mem_assoc f t)) && (List.mem_assoc g t) -> true
           | _ -> false
         ) highlighted_feat_list
      ) in

    let buff = Buffer.create 32 in
    let () = match pid_name with
      | "" -> ()
      | pid -> bprintf buff "<TR><TD COLSPAN=\"3\" BGCOLOR=\"#00FF00\"><B>[%s]</B></TD></TR>\n" pid in

    let next =
      match get_main ?main_feat t with
      | (None, sub) -> sub
      | (Some (feat_name,atom), sub) ->
        if is_highlithed feat_name
        then bprintf buff "<TR><TD COLSPAN=\"3\" BGCOLOR=\"#00FF00\"><B>%s</B></TD></TR>\n" (string_of_value atom)
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
  let to_word_opt (t:t) =
    match List_.sort_assoc_opt "_UD_empty" t with
    | Some v when string_of_value v = "Yes" -> None
    | _ ->
      match List_.sort_assoc_opt "phon" t with
      | Some s -> Some (string_of_value s)
      | None ->
        match List_.sort_assoc_opt "form" t with
        | Some s -> Some (string_of_value s)
        | None -> None

  (* ---------------------------------------------------------------------- *)
  let escape_sharp s =
    Str.global_replace (Str.regexp "#") "__SHARP__" s

  (* ---------------------------------------------------------------------- *)
  let to_dep ?(decorated_feat=("",[])) ?(tail=[]) ?main_feat ?filter t =
    let (pid_name, highlighted_feat_list) = decorated_feat in

    let is_highlithed feat_name =
      (List.mem_assoc feat_name highlighted_feat_list) ||
      (List.exists (function
           | (f, Some g) when g = feat_name && (not (List.mem_assoc f t)) && (List.mem_assoc g t) -> true
           | _ -> false
         ) highlighted_feat_list
      ) in

    let (main_opt, sub) = get_main ?main_feat t in
    let sub = List.sort G_feature.print_cmp sub in

    let color = match (get_value_opt "parseme" t, get_value_opt "frsemcor" t) with
      | (Some (String "NE"), None) -> ":C:#ff760b"
      | (Some (String "MWE"), None) -> ":C:#1d7df2"
      | (None, Some _) -> ":C:#12CD56"
      | _ -> "" in

    let main = match main_opt with
      | None -> []
      | Some (feat_name, atom) ->
        let esc_atom = escape_sharp (string_of_value atom) in
        let text = esc_atom ^ color in
        [ if is_highlithed feat_name
          then sprintf "%s:B:#8bf56e" text
          else text] in

    (* add the pattern identifier *)
    let word_list = match pid_name with
      | "" -> main
      | _ -> (sprintf "[%s]:B:#8bf56e" pid_name)::main in

    let word = match word_list with
      | [] -> ""
      | l ->  String.concat "#" l in

    let lines = List.fold_left
        (fun acc (feat_name, atom) ->
           let esc_atom = escape_sharp (G_feature.to_string (decode_feat_name feat_name, atom)) in
           let text = esc_atom ^ color in
           if is_highlithed feat_name
           then (sprintf "%s:B:#8bf56e" text) :: acc
           else
             match filter with
             | Some test when not (test feat_name) -> acc
             | _ -> text :: acc
        ) tail sub in
    let subword = String.concat "#" (List.rev lines) in

    sprintf " word=\"%s\"; subword=\"%s\"" word subword

  (* ---------------------------------------------------------------------- *)
  let to_conll ?exclude t =
    let reduced_t = match exclude with
      | None -> t
      | Some list -> List.filter (fun (fn,_) -> not (List.mem fn list)) t in
    let ud_ordering = (* In UD CoNLL-U format, features are sorted wrt lowercase form *)
      List.sort
        (fun feat1 feat2 -> Stdlib.compare (String.lowercase_ascii (G_feature.get_name feat1)) (String.lowercase_ascii (G_feature.get_name feat2)))
        reduced_t in
    List.map (fun (fn, fv) -> (fn, string_of_value fv)) ud_ordering

  let to_json_python t = `Assoc (List.map (fun (fn, fv) -> (fn, `String (string_of_value fv))) t)

end (* module G_fs *)

(* ================================================================================ *)
module P_fs = struct
  (* list are supposed to be striclty ordered wrt compare *)
  type t = P_feature.t list

  let empty = []

  let to_json_python ?domain t = `List (List.map (P_feature.to_json_python ?domain) t)

  let of_ast ?domain lexicons ast_fs =
    let unsorted = List.map (P_feature.build lexicons ?domain) ast_fs in
    List.sort P_feature.compare unsorted

  let feat_list t =
    List.map (function
        | (fn, P_feature.Else (_,fn2,_)) -> (fn, Some fn2)
        | (fn, _) -> (fn, None)
      ) t

  let to_string t = List_.to_string P_feature.to_string "\\n" t

  let to_dep ?filter t =
    let reduced = match filter with
      | None -> t
      | Some test -> List.filter (fun (fn,_) -> test fn) t in
    List_.to_string (P_feature.to_string) "#" reduced

  let to_dot t = List_.to_string P_feature.to_string "\\n" t

  exception Fail

  let match_ ?(lexicons=[]) p_fs g_fs =
    let p_fs_wo_pos =
      try List.remove_assoc "position" p_fs
      with Not_found -> p_fs in
    let rec loop acc = function
      | [], _ -> acc

      (* a feature_name present only in graph -> Skip it *)
      | ((fn_pat, fv_pat)::t_pat, (fn, _)::t) when fn_pat > fn -> loop acc ((fn_pat, fv_pat)::t_pat, t)

      (* Two next cases: p_fs requires for the absence of a feature -> OK *)
      | ((fn_pat, P_feature.Absent)::t_pat, []) -> loop acc (t_pat, [])
      | ((fn_pat, P_feature.Absent)::t_pat, (fn, fa)::t) when fn_pat < fn -> loop acc (t_pat, (fn, fa)::t)

      (* look for the second part of an Else construction*)
      | ((_, P_feature.Else (_,fn2,fv2))::t_pat,[]) ->        begin
          try if (List.assoc fn2 g_fs) <> fv2 then raise Fail
          with Not_found -> raise Fail
        end; loop acc (t_pat, [])

      (* special case of Else when the first request is not satified *)
      | ((fn_pat, P_feature.Else (_,fn2,fv2))::t_pat, (((fn, _)::_) as t)) when fn_pat < fn ->
        begin
          try if (List.assoc fn2 g_fs) <> fv2 then raise Fail
          with Not_found -> raise Fail
        end; loop acc (t_pat, t)

      (* p_fs is not empty and  does not begin with and Absent request, g_fs empty ==> Fail *)
      | _::_, [] -> raise Fail

      (* the next p_fs request cannot be satisfied ==> Fail *)
      | ((fn_pat, _)::_, (fn, _)::_) when fn_pat < fn -> raise Fail

      (* Next cases: fn_pat = fn *)
      | ((_, P_feature.Absent)::_, (_, atom)::t) -> raise Fail
      | ((_, P_feature.Equal fv)::_, (_, atom)::t) when not (List_.sort_mem atom fv) -> raise Fail
      | ((_, P_feature.Else (fv,_,_))::_, (_, atom)::t) when atom <> fv -> raise Fail
      | ((_, P_feature.Different fv)::_, (_, atom)::t) when List_.sort_mem atom fv -> raise Fail

      | ((_, P_feature.Equal_lex (lex_id,field))::t_pat, (_, atom)::t) ->        begin
          try
            let lexicon = List.assoc lex_id acc in
            match Lexicon.select_opt field (string_of_value atom) lexicon with
            | None -> raise Fail
            | Some new_lexicon ->
              let new_acc = (lex_id, new_lexicon) :: (List.remove_assoc lex_id acc) in
              loop new_acc (t_pat, t)
          with
          | Not_found -> Error.bug "[P_fs.match_] Cannot find lexicon. lex_id=\"%s\"" lex_id
        end

      (* We have exhausted Fail cases, head of g_fs satisties head of p_fs *)
      | (_::p_tail, _::g_tail) -> loop acc (p_tail,g_tail) in
    loop lexicons (p_fs_wo_pos,g_fs)

  exception Fail_unif
  let unif fs1 fs2 =
    let rec loop = function
      | [], fs -> fs
      | fs, [] -> fs

      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 < fn2 -> (fn1,v1) :: (loop (t1,(fn2,v2)::t2))
      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 > fn2 -> (fn2,v2) :: (loop ((fn1,v1)::t1,t2))
      | ((fn1,v1)::t1, (fn2,v2)::t2) (* when fn1 = fn2 *) ->
        try (fn1,P_feature.unif_value v1 v2) :: (loop (t1,t2))
        with
        | P_feature.Fail_unif -> raise Fail_unif
        | Error.Build (msg,_) -> Error.build "Feature '%s', %s" fn1 msg
    in loop (fs1, fs2)
end (* module P_fs *)
