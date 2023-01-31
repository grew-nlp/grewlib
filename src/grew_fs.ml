(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

open Grew_types
open Grew_utils
open Grew_ast

let decode_feat_name s = Str.global_replace (Str.regexp "__\\([0-9a-z]+\\)$") "[\\1]" s

let dot_escape_string s =
  s 
  |> Str.global_replace (Str.regexp_string ">") "&gt;"
  |> Str.global_replace (Str.regexp_string "<") "&lt;"

(* ================================================================================ *)
module G_feature = struct

  type t = string * Feature_value.t

  let get_name = fst

  let compare feat1 feat2 = Stdlib.compare (get_name feat1) (get_name feat2)

  (* another order used for printing purpose only *)
  let print_order = ["phon"; "form"; "cat"; "upos"; "lemma"; "pos"; "xpos"]
  let print_cmp (name1,_) (name2,_) =
    match (List_.index_opt name1 print_order, List_.index_opt name2 print_order) with
    | (Some i, Some j) -> Stdlib.compare i j
    | (Some _, None) -> -1
    | (None, Some _) -> 1
    | (None, None) -> Stdlib.compare name1 name2

  let build = function
    | ({Ast.kind=Ast.Feat_kind_list (Eq,[atom]); name=name},loc) ->
      (name, Feature_value.parse ~loc name atom)
    | (uf,loc) -> Error.build ~loc "in graph nodes, features must follow the shape \"name = value\" (error on feature: \"%s\")" (Ast.u_feature_to_string uf)

  let to_string (feat_name, feat_val) = sprintf "%s=%s" feat_name (Feature_value.to_string feat_val)

  let to_json (feat_name, feat_val) = (feat_name, Feature_value.to_json feat_val)

  let buff_dot buff (feat_name, feat_val) =
    let string_val = Feature_value.to_string feat_val in
    match Str.split (Str.regexp ":C:") string_val with
    | [] -> bprintf buff "<TR><TD ALIGN=\"right\">%s</TD><TD>=</TD><TD ALIGN=\"left\"></TD></TR>\n" (decode_feat_name feat_name)
    | fv::_ -> bprintf buff "<TR><TD ALIGN=\"right\">%s</TD><TD>=</TD><TD ALIGN=\"left\">%s</TD></TR>\n" (decode_feat_name feat_name) (dot_escape_string fv)
end (* module G_feature *)

(* ================================================================================ *)
module P_feature = struct

  type p_feature_value =
    | Pfv_list of Cmp.t * Feature_value.t list (* value (Eq,[]) must not be used *)
    | Pfv_lex of Cmp.t * string * string
    | Pfv_re of Cmp.t * string
    | Absent
    | Else of (Feature_value.t * string * Feature_value.t)

  type t = string * p_feature_value

  let get_name = fst

  let compare feat1 feat2 = Stdlib.compare (get_name feat1) (get_name feat2)

  let _dump (feature_name, p_feature_value) =
    printf "[P_feature.dump]\n";
    printf "%s%s\n"
      feature_name
      (match p_feature_value with
       | Pfv_list (Neq, []) -> "=*"
       | Pfv_list (cmp,l) -> sprintf "%s%s" (Cmp.to_string cmp) (String.concat "|" (List.map Feature_value.to_string l))
       | Pfv_lex (cmp,lex,fn) -> sprintf "%s %s.%s" (Cmp.to_string cmp) lex fn
       | Pfv_re (cmp,re) -> sprintf "%s re\"%s\"" (Cmp.to_string cmp) re
       | Absent -> " must be Absent!"
       | Else (fv1,fn2,fv2) -> sprintf " = %s/%s = %s" (Feature_value.to_string fv1) fn2 (Feature_value.to_string fv2));
    printf "%!"

  exception Fail_unif

  (** raise [P_feature.Fail_unif] *)
  let unif_value v1 v2 = match (v1, v2) with
    | (Absent, Absent) -> v1
    | (Absent, _)
    | (_, Absent) -> raise Fail_unif
    | (Pfv_list (Eq,l1), Pfv_list (Eq,l2)) ->
      begin
        match List_.sort_inter l1 l2 with
        | [] -> raise Fail_unif
        | l -> Pfv_list (Eq,l)
      end
    | (Pfv_list (Eq,l1), Pfv_list (Neq,l2))
    | (Pfv_list (Neq,l2), Pfv_list (Eq,l1)) ->
      begin
        match List_.sort_diff l1 l2 with
        | [] -> raise Fail_unif
        | l -> Pfv_list (Eq,l)
      end
    | (Pfv_list (Neq,l1), Pfv_list (Neq,l2)) -> Pfv_list (Neq,List_.sort_union l1 l2)
    | _ -> Error.bug "[P_feature.unif_value] inconsistent match case" (* HHH : change message error run not handled... *)

  let to_string = function
    | (feat_name, Pfv_list (Neq,[])) -> sprintf "%s=*" feat_name
    | (feat_name, Pfv_list (cmp,atoms)) -> sprintf "%s%s%s" feat_name (Cmp.to_string cmp) (String.concat "|" (List.map Feature_value.to_string atoms))
    | (feat_name, Pfv_lex (cmp,lex,fn)) -> sprintf "%s%s%s.%s" feat_name (Cmp.to_string cmp) lex fn
    | (feat_name, Pfv_re (cmp,re)) -> sprintf "%s%sre\"%s\"" feat_name (Cmp.to_string cmp) re
    | (feat_name, Absent) -> sprintf "!%s" feat_name
    | (feat_name, Else (fv1,fn2,fv2)) -> sprintf "%s=%s/%s=%s" feat_name (Feature_value.to_string fv1) fn2 (Feature_value.to_string fv2)

  let build lexicons = function
    | ({Ast.kind=Ast.Feat_kind_list (cmp,unsorted_values); name}, loc) ->
      let values = unsorted_values
        |> List.sort Stdlib.compare
        |> List.map (Feature_value.parse ~loc name) in
      (name, Pfv_list (cmp,values))

    | ({Ast.kind=Ast.Feat_kind_lex (cmp,lex,fn); name}, loc) ->
      Lexicons.check ~loc lex fn lexicons;
      (name, Pfv_lex (cmp,lex,fn))

    | ({Ast.kind=Ast.Feat_kind_re (cmp,re); name}, _) -> (name, Pfv_re (cmp,re) )

    | ({Ast.kind=Ast.Absent; name}, _) ->
      (name, Absent)

    | ({Ast.kind=Ast.Else (fv1,fn2,fv2); name}, loc) ->
      let v1 = Feature_value.parse ~loc name fv1 in
      let v2 = Feature_value.parse ~loc name fv2 in
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
  let set_value feature_name value t =
    let rec loop = function
      | [] -> [(feature_name, value)]
      | ((fn,_)::_) as t when feature_name < fn -> (feature_name, value)::t
      | (fn,_)::t when feature_name = fn -> (feature_name, value)::t
      | (fn,a)::t -> (fn,a) :: (loop t)
    in loop t

  (* ---------------------------------------------------------------------- *)
  let set_atom ?loc feature_name atom t =
    let value = Feature_value.parse ?loc feature_name atom in
    set_value feature_name value t

  (* ---------------------------------------------------------------------- *)
  let del_feat_opt = List_.sort_remove_assoc_opt

  (* ---------------------------------------------------------------------- *)
  let del_feat = List_.sort_remove_assoc

  (* ---------------------------------------------------------------------- *)
  let get_value_opt = List_.sort_assoc_opt

  (* ---------------------------------------------------------------------- *)
  let to_string t = String.concat "," (List.map G_feature.to_string t)
  (* ---------------------------------------------------------------------- *)
  let to_json = function
    | ["label", Feature_value.String label] -> `String label
    | feat_list -> `Assoc (List.map G_feature.to_json feat_list)

  (* ---------------------------------------------------------------------- *)
  let of_ast ast_fs =
    let unsorted = List.map (fun feat -> G_feature.build feat) ast_fs in
    List.sort G_feature.compare unsorted

  (* ---------------------------------------------------------------------- *)
  let of_items items =
    let unsorted = List.map (fun (f,v) -> (f, Feature_value.parse f v)) items in
    List.sort G_feature.compare unsorted

  (* ---------------------------------------------------------------------- *)
  let pst_leaf ?loc form = [("form", Feature_value.parse ?loc "form" form)]
  let pst_node ?loc upos = [("upos", Feature_value.parse ?loc "upos" upos)]

  (* ---------------------------------------------------------------------- *)
  let concat_values ?loc side separator v1 v2 =
    match (side, v1, v2) with
    | (Ast.Append, Feature_value.String v1, Feature_value.String v2) -> Feature_value.String (v1 ^ separator ^ v2)
    | (Ast.Prepend, Feature_value.String v1, Feature_value.String v2) -> Feature_value.String (v2 ^ separator ^ v1)
    | _ -> Error.run ?loc "Cannot concat numerical values"

  (* ---------------------------------------------------------------------- *)
  let concat_feats_opt ?loc side src tar separator regexp =
    match List.filter
            (fun (feature_name,_) ->
               match feature_name with
               | "form" | "lemma" | "upos" | "xpos" | "wordform" | "textform" -> false
               | _ -> String_.re_match (Str.regexp regexp) feature_name
            ) src with
    | [] -> None
    | sub_src ->
      let (new_tar, updated_feats) = List.fold_left
          (fun (acc_tar, acc_updated_feats) (feat, value) ->
             match List_.sort_assoc_opt feat tar with
             | None -> (set_value feat value acc_tar, (feat, value)::acc_updated_feats)
             | Some v ->
               let new_value = concat_values ?loc side separator v value in
               (set_value feat new_value acc_tar, (feat, new_value)::acc_updated_feats)
          ) (tar,[]) sub_src in
      Some (new_tar, updated_feats)

  (* ---------------------------------------------------------------------- *)
  let get_main ?main_feat t =
    let default_list = ["form"; "lemma"; "gpred"; "label"] in
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
        let s = match Feature_value.to_string atom with "" -> "_" | x -> dot_escape_string x in (* Bug in dot if empty *)

        if is_highlithed feat_name
        then bprintf buff "<TR><TD COLSPAN=\"3\" BGCOLOR=\"#00FF00\"><B>%s</B></TD></TR>\n" s
        else bprintf buff "<TR><TD COLSPAN=\"3\"><B>%s</B></TD></TR>\n" s;
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
    | Some v when Feature_value.to_string v = "Yes" -> None
    | _ ->
      match List_.sort_assoc_opt "phon" t with
      | Some s -> Some (Feature_value.to_string s)
      | None ->
        match List_.sort_assoc_opt "form" t with
        | Some s -> Some (Feature_value.to_string s)
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
      | (Some (Feature_value.String "NE"), None) -> ":C:#ff760b"
      | (Some (Feature_value.String "MWE"), None) -> ":C:#1d7df2"
      | (None, Some _) -> ":C:#12CD56"
      | _ -> "" in

    let main = match main_opt with
      | None -> []
      | Some (feat_name, atom) ->
        let esc_atom = escape_sharp (Feature_value.to_string atom) in
        let text = esc_atom ^ color in
        [ if is_highlithed feat_name
          then sprintf "%s:B:#8bf56e" text
          else text] in

    (* add the request identifier *)
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
        ) [] sub in
    let subword = String.concat "#" (List.rev (tail @ lines)) in

    sprintf " word=\"%s\"; subword=\"%s\"" word subword

end (* module G_fs *)

(* ================================================================================ *)
module P_fs = struct
  (* list are supposed to be striclty ordered wrt compare *)
  type t = P_feature.t list

  let empty = []

  let of_ast lexicons ast_fs =
    let unsorted = List.map (P_feature.build lexicons) ast_fs in
    List.sort P_feature.compare unsorted

  let feat_list t =
    List.map (function
        | (fn, P_feature.Else (_,fn2,_)) -> (fn, Some fn2)
        | (fn, _) -> (fn, None)
      ) t

  let to_string t = t |> List.map P_feature.to_string |> String.concat ","

  let to_dep ?filter t =
    let reduced = match filter with
      | None -> t
      | Some test -> List.filter (fun (fn,_) -> test fn) t in
    reduced |> List.map P_feature.to_string |> String.concat "#"

    let to_dot t = t |> List.map P_feature.to_string |> String.concat "\\n"
  exception Fail

  let match_ ?(lexicons=[]) p_fs g_fs =
    let rec loop acc = function
      | [], _ -> acc

      (* a feature_name present only in graph -> Skip it *)
      | ((fn_pat, fv_pat)::t_pat, (fn, _)::t) when fn_pat > fn -> loop acc ((fn_pat, fv_pat)::t_pat, t)

      (* Two next cases: p_fs requires for the absence of a feature -> OK *)
      | ((_, P_feature.Absent)::t_pat, []) -> loop acc (t_pat, [])
      | ((fn_pat, P_feature.Absent)::t_pat, (fn, fa)::t) when fn_pat < fn -> loop acc (t_pat, (fn, fa)::t)

      (* look for the second part of an Else construction *)
      | ((_, P_feature.Else (_,fn2,fv2))::t_pat,[]) ->
        begin
          try if (List.assoc fn2 g_fs) <> fv2 then raise Fail
          with Not_found -> raise Fail
        end; loop acc (t_pat, [])

      (* special case of Else when the first constraint is not satified *)
      | ((fn_pat, P_feature.Else (_,fn2,fv2))::t_pat, (((fn, _)::_) as t)) when fn_pat < fn ->
        begin
          try if (List.assoc fn2 g_fs) <> fv2 then raise Fail
          with Not_found -> raise Fail
        end; loop acc (t_pat, t)

      (* p_fs is not empty and  does not begin with and Absent constraint, g_fs empty ==> Fail *)
      | _::_, [] -> raise Fail

      (* the next p_fs constraint cannot be satisfied ==> Fail *)
      | ((fn_pat, _)::_, (fn, _)::_) when fn_pat < fn -> raise Fail

      (* Next cases: fn_pat = fn *)
      | ((_, P_feature.Absent)::_, (_, _)::_) -> raise Fail

      | ((_, P_feature.Pfv_list (Eq,fv))::_, (_, atom)::_) when not (List_.sort_mem atom fv) -> raise Fail
      | ((_, P_feature.Pfv_list (Neq,fv))::_, (_, atom)::_) when (List_.sort_mem atom fv) -> raise Fail

      | ((_, P_feature.Pfv_re (Eq,re))::_, (_, atom)::_) when not (String_.re_match (Str.regexp re) (Feature_value.to_string atom)) -> raise Fail
      | ((_, P_feature.Pfv_re (Neq,re))::_, (_, atom)::_) when (String_.re_match (Str.regexp re) (Feature_value.to_string atom)) -> raise Fail

      | ((_, P_feature.Else (fv,_,_))::_, (_, atom)::_) when atom <> fv -> raise Fail

      | ((_, P_feature.Pfv_lex (cmp,lex_id,field))::t_pat, (_, atom)::t) ->
        begin
          try
            let lexicon = List.assoc lex_id acc in
            match Lexicon.filter_opt cmp field (Feature_value.to_string atom) lexicon with
            | None -> raise Fail
            | Some new_lexicon ->
              let new_acc = (lex_id, new_lexicon) :: (List.remove_assoc lex_id acc) in
              loop new_acc (t_pat, t)
          with
          | Not_found -> Error.bug "[P_fs.match_] Cannot find lexicon. lex_id=\"%s\"" lex_id
        end

      (* We have exhausted Fail cases, head of g_fs satisties head of p_fs *)
      | (_::p_tail, _::g_tail) -> loop acc (p_tail,g_tail) in
    loop lexicons (p_fs,g_fs)

  exception Fail_unif
  let unif fs1 fs2 =
    let rec loop = function
      | [], fs -> fs
      | fs, [] -> fs

      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 < fn2 -> (fn1,v1) :: (loop (t1,(fn2,v2)::t2))
      | ((fn1,v1)::t1, (fn2,v2)::t2) when fn1 > fn2 -> (fn2,v2) :: (loop ((fn1,v1)::t1,t2))
      | ((fn1,v1)::t1, (_,v2)::t2) (* when fn1 = fn2 *) ->
        try (fn1,P_feature.unif_value v1 v2) :: (loop (t1,t2))
        with
        | P_feature.Fail_unif -> raise Fail_unif
        | Error.Build (msg,_) -> Error.build "Feature '%s', %s" fn1 msg
    in loop (fs1, fs2)
end (* module P_fs *)
