(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Conll
open Amr

open Grew_types
open Grew_utils
open Grew_loader
open Grew_graph
open Grew_rule

(* ==================================================================================================== *)
module Pst_corpus = struct
  let load_files files =
    Array.of_list
      (CCList.flat_map
        (fun file ->
          let line_list =
            CCIO.(with_in file read_lines_l)
            (* Blanks lines (empty or only with spaces and tabs) and lines starting with '%' are ignored. *)
            |> List.filter (fun line -> not ((Str.string_match (Str.regexp "^[ \t]*$") line 0) || (line.[0] = '%'))) in
          List.mapi
            (fun i line ->
              match Str.split (Str.regexp "\t") line with
              | [pst] -> (sprintf "%s_%05d" file (i+1), pst)
              | [id; pst] -> (id, pst)
              | _ -> failwith "Pst syntax error"
            ) line_list
        ) files
      )
end

(* ==================================================================================================== *)
module Corpus = struct
  type kind =
    | Conll of Conll_columns.t option (* value is None in Corpus_desc and Some c once the corpus is really loaded *)
    | Pst | Amr | Gr | Json | Dmrs | Ucca

  type item = {
    sent_id: string;
    text: string;
    graph: G_graph.t;
  }

  type t = {
    items: item array;
    kind: kind;
  }

  let graph_of_sent_id sent_id corpus =
    match CCArray.find_idx (fun item -> item.sent_id = sent_id) corpus.items with
    | Some (_,item) -> Some item.graph
    | None -> None

  let item_of_graph graph =
    let sent_id =
      match G_graph.get_meta_opt "sent_id" graph with
      | Some s -> s
      | None -> "_" in
    let text =
      match G_graph.get_meta_opt "text" graph with
      | Some s -> s
      | None -> "_" in
    { sent_id; text; graph }

  let merge = function
    | [] -> Error.bug "Empty list in Corpus.merge"
    | [one] -> one
    | h::t ->
      if List.exists (fun t -> t.kind <> h.kind) t
      then Error.run "Cannot merge corpora with incompatible kinds"
      else {h with items = Array.concat (List.map (fun t -> t.items) (h::t)) }

  let of_conllx_corpus conllx_corpus =
    let items =
      Array.map
        (fun (sent_id, conllx) ->
           let text = match List.assoc_opt "text" (Conll.get_meta conllx) with Some t -> t | None -> "__missing text metadata__" in
           let graph = conllx |> Conll.to_json |> G_graph.of_json in
           { sent_id; text; graph }
        ) (Conll_corpus.get_data conllx_corpus) in
    { kind = Conll (Some (Conll_corpus.get_columns conllx_corpus)); items }

  let of_amr_file file =
    try
      let amr_corpus = Amr_corpus.load file in
      let items =
        Array.map
          (fun (sent_id, amr) ->
             let json = Amr.to_json ~unfold:true amr in
             let graph = G_graph.of_json json in
             let text = match G_graph.get_meta_opt "text" graph with Some t -> t | None -> "__missing text metadata__" in
             { sent_id; text; graph }
          ) amr_corpus in
      { kind=Amr; items; }
    with Amr.Error msg -> Error.build "Amr error in file `%s`: %s" file msg

  let fold_left fct init t =
    Array.fold_left
      (fun acc item -> fct acc item.sent_id item.graph)
      init t.items

  let fold_right fct t init =
    Array.fold_right
      (fun item acc -> fct item.sent_id item.graph acc)
      t.items init


  let iteri fct t = Array.iteri (fun i item -> fct i item.sent_id item.graph) t.items



  let size t = Array.length t.items

  let get_graph position t = t.items.(position).graph
  let get_sent_id position t = t.items.(position).sent_id

  let is_conll t =
    match t.kind with
    | Conll _ | Dmrs -> true
    | _ -> false

  let get_text position t = t.items.(position).text

  let update_graph sent_id graph corpus =
    match CCArray.find_idx (fun item -> item.sent_id = sent_id) corpus.items with
    | Some (pos,item) -> corpus.items.(pos) <- {item with graph}
    | None -> Error.run "[update_graph] unknown sent_id"

  let permut_length t =
    let items_with_length =
      Array.mapi
        (fun i item -> (i,G_graph.size item.graph)) t.items in
    let _ = Array.sort
        (fun (_,s1) (_,s2) -> Stdlib.compare s1 s2)
        items_with_length in
    Array.map fst items_with_length

  let from_json ?loc json =
    try
      match json with
      | `List jsons ->
        Array.of_list (
          List.map (fun json -> json |> G_graph.of_json |> item_of_graph) jsons
        )
      | json -> [| json |> G_graph.of_json |> item_of_graph |]
    with
    | Yojson.Json_error msg -> Error.run ?loc "Error in the JSON file format: %s" msg

  let from_stdin ?ext ?log_file ?config () =
    match ext with
    | Some ".json" ->
      let s = CCIO.read_all stdin in
      { kind=Json; items = from_json (Yojson.Basic.from_string s)}
    | Some ".conll" | Some ".conllu" | Some ".cupt" | Some ".orfeo" | Some ".frsemcor"
    | _ -> (* TODO: use Conll by default --> more robust stuff needed *)
      let lines = CCIO.read_lines_l stdin in
      of_conllx_corpus (Conll_corpus.of_lines ?log_file ?config lines)

  let from_string ?ext ?log_file ?config s =
    match ext with
    | Some ".json" -> { kind=Json; items = from_json (Yojson.Basic.from_string s)}
    | Some ".conll" | Some ".conllu" | Some ".cupt" | Some ".orfeo" | Some ".frsemcor"
    | _ -> (* TODO: use Conll by default --> more robust stuff needed *)
      let lines = Str.split (Str.regexp "\n") s in
      of_conllx_corpus (Conll_corpus.of_lines ?log_file ?config lines)

  let of_json_file file =
    try { kind=Json; items = from_json ~loc: (Loc.file file) (Yojson.Basic.from_file file)}
    with Yojson.Json_error msg -> Error.run ~loc:(Loc.file file) "Error in the JSON file format: %s" msg

  let from_file ?ext ?log_file ?config file =
    let extension = match ext with Some e -> e | None -> Filename.extension file in
    match extension with
    | ".conll" | ".conllu" | ".cupt" | ".orfeo" | ".frsemcor" ->
      of_conllx_corpus (Conll_corpus.load ?log_file ?config file)
    | ".amr" | ".txt" ->
      of_amr_file file
    | ".json" ->
      of_json_file file
    | ext -> Error.run "Cannot load file `%s`, unknown extension `%s`" file ext

  let from_dir ?log_file ?config dir =
    let files = Sys.readdir dir in
    let (conll_files, amr_files, txt_files, json_files) =
      Array.fold_right
        (fun file (conll_acc, amr_acc, txt_acc, json_acc) ->
          let full_file = Filename.concat dir file in
           match Filename.extension file with
           | ".conll" | ".conllu" | ".cupt" | ".orfeo" -> (full_file::conll_acc, amr_acc, txt_acc, json_acc)
           | ".amr" -> (conll_acc, full_file::amr_acc, txt_acc, json_acc)
           | ".txt" -> (conll_acc, amr_acc, full_file::txt_acc, json_acc)
           | ".json" -> (conll_acc, amr_acc, txt_acc, full_file::json_acc)
           | _ -> (conll_acc, amr_acc, txt_acc, json_acc)
        ) files ([],[],[], []) in

    (* txt files are interpreted as AMR files only if there is no conll-like files (eg: UD containts txt files in parallel to conllu) *)
    match (conll_files, amr_files, txt_files, json_files) with
    | ([],[],[], []) -> Error.run "The directory `%s` does not contain any graphs" dir
    | (_::_ as conll_files,_,_,_) -> of_conllx_corpus (Conll_corpus.load_list ?log_file ?config conll_files)
    | ([], (_::_ as amr_files), txt_files, _) | ([], amr_files, (_::_ as txt_files), _) -> (amr_files @ txt_files) |> List.map of_amr_file |> merge
    | ([],[],[],json_files) -> json_files |> List.map of_json_file |> merge

  (* val from_assoc_list: (string * G_graph.t) list -> t *)
  let from_assoc_list l =
    {
      items = Array.of_list (List.map (fun (sent_id,graph) -> {sent_id; graph; text=""}) l);
      kind = Gr
    }

  let get_columns_opt corpus =
    match corpus.kind with
    | Conll (Some c) -> Some c
    | _ -> None

  (* ---------------------------------------------------------------------------------------------------- *)
  let search ?(json_label=false) ~config default update request cluster_item_list corpus =
    let depth = List.length cluster_item_list in
    fold_left
    (fun acc sent_id graph ->
      let matchings = Matching.search_request_in_graph ~config request graph in
      List.fold_left
      (fun acc2 matching ->
        let cluster_value_list =
          List.map
          (fun cluster_item ->
            Matching.get_clust_value_opt ~json_label ~config cluster_item request graph matching
          ) cluster_item_list in
          Clustered.update (update sent_id graph matching) cluster_value_list default acc2
      ) acc matchings
    ) (Clustered.empty depth) corpus

  (* ---------------------------------------------------------------------------------------------------- *)
  type status =
    | Ok
    | Timeout of float
    | Over of float

  let bounded_search ?(json_label=false) ~config ?(ordering = None) bound timeout default update request cluster_item_list corpus =
    let depth = List.length cluster_item_list in
    let len = size corpus in
    let permut_fct =
      match ordering with
      | Some "length" -> 
        let perm = permut_length corpus in fun x -> perm.(x)
      | Some "shuffle" -> 
        let mix = Array.init len CCFun.id in
        let () = CCArray.shuffle mix in
        fun x -> mix.(x)
      | _ -> fun x -> x in
    let matching_counter = ref 0 in
    let init_time = Unix.gettimeofday() in
    let status = ref Ok in
    let check graph_counter =
      (match bound with Some b when !matching_counter > b -> status := Over ((float graph_counter) /. (float (Array.length corpus.items))) | _ -> ());
      (match timeout with Some b when Unix.gettimeofday() -. init_time >= b -> status := Timeout ((float graph_counter) /. (float (Array.length corpus.items))) | _ -> ()) in

    let rec loop acc graph_counter =
      if !status <> Ok || graph_counter = len
        then acc
        else
          begin
            let graph_index = permut_fct graph_counter in
            let graph = get_graph graph_index corpus in
            let sent_id = get_sent_id graph_index corpus in
            let matchings = Matching.search_request_in_graph ~config request graph in
            let nb_in_graph = List.length matchings in
            let new_acc =
              CCList.foldi (* TODO: replace by loop or exception to avoid useless steps *)
                (fun acc2 pos_in_graph matching ->
                  incr matching_counter;
                  check graph_counter;
                  if !status <> Ok
                  then acc2
                  else
                    let cluster_value_list =
                      List.map
                        (fun cluster_item ->
                          Matching.get_clust_value_opt ~json_label ~config cluster_item request graph matching
                        ) cluster_item_list in
                      Clustered.update (update graph_index sent_id graph pos_in_graph nb_in_graph matching) cluster_value_list default acc2
                ) acc matchings in
            loop new_acc (graph_counter + 1)
          end in
    loop (Clustered.empty depth) 0
    |> (fun x -> match !status with
    | Ok -> (x, "complete", 1.)
    | Timeout r -> (x, "timeout", r)
    | Over r -> (x, "max_results", r)
    )

  let count_feature_values ?(filter=fun _ -> true) t =
    fold_right
      (fun _ graph acc ->
        G_graph.count_feature_values ~filter ~acc (graph : G_graph.t)
      ) t String_map.empty
end (* module Corpus *)

(* ==================================================================================================== *)
module Corpus_desc = struct
  open Yojson.Basic.Util

  type t = Yojson.Basic.t

  let to_json t = t

  let of_json t = t
  let get_id corpus_desc = corpus_desc |> member "id" |> to_string

  let get_field_opt field corpus_desc = corpus_desc |> member field |> to_string_option

  let get_config corpus_desc =
    try corpus_desc |> member "config" |> to_string_option |> CCOption.map_or ~default:(Conll_config.build "ud") Conll_config.build
    with Type_error _ -> Error.run "[Corpus_desc.get_config] \"config\" field must be a string in %s" (get_id corpus_desc)

  let get_directory corpus_desc =
    try corpus_desc |> member "directory" |> to_string
    with Type_error _ -> Error.run "[Corpus_desc.get_directory] \"directory\" field is mandatory and must be a string in %s" (get_id corpus_desc)

  let ensure_directory dir =
    match File.get_path_status dir with
    | Directory -> ()
    | File -> Error.run "Cannot build directory `%s`, a file with the same name already exists" dir
    | Dont_exist -> Unix.mkdir dir 0o755

  let get_build_directory corpus_desc =
    let dir = get_directory corpus_desc in
    match File.get_path_status dir
    with
      | Directory ->
        let intermediate_dir = Filename.concat (get_directory corpus_desc) "_build_grew" in
        ensure_directory intermediate_dir;
        let build_dir = Filename.concat intermediate_dir (get_id corpus_desc) in
        ensure_directory build_dir;
        build_dir
      | File -> Error.run "corpus `%s`: `%s` is not a directory" (get_id corpus_desc) dir
      | Dont_exist -> Error.run "corpus `%s`: the directory `%s` does not exist" (get_id corpus_desc) dir

  (** default value is [false] *)
  let get_flag flag corpus_desc =
    try corpus_desc |> member flag |> to_bool
    with Type_error _ -> false

  let get_display corpus_desc =
      try Some (corpus_desc |> member "display" |> to_int)
      with Type_error _ -> None

  let get_kind corpus_desc =
    try 
      match (corpus_desc |> member "kind" |> to_string_option, corpus_desc |> member "columns" |> to_string_option) with
      | (None, columns_opt) | (Some "conll", columns_opt) ->
          Corpus.Conll (CCOption.map (Conll_columns.of_list << (CCString.split_on_char ' ')) columns_opt)
      | (Some "pst",_) -> Pst
      | (Some "amr",_) -> Amr
      | (Some "dmrs",_) -> Dmrs
      | (Some "ucca",_) -> Ucca
      | (Some "json",_) -> Json
      | (Some x,_) -> Error.run "[Corpus.load_json] Unknown \"kind\":\"%s\" field in corpus: \"%s\"" x (get_id corpus_desc)
    with Type_error _ -> Error.run "[Corpus.load_json] \"kind\" must be a string in corpus: \"%s\"" (get_id corpus_desc)


  let read_dir directory =
    try
      let file_seq = CCIO.File.read_dir directory in
      let rec loop () =
        match file_seq () with
        | Some file -> file :: loop ()
        | None -> [] in
      loop ()
    with Sys_error _ -> []

  (* replace ${…} with value of the environment variable and add the list of built files in the field "built_files" *)
  let expand_and_check ~env json_file = function
    | `Assoc l ->
      let (id_flag, dir) = (ref None, ref None) in
      let new_l =
        List.map (function
          | ("id", `String id) as i -> id_flag := Some id; i
          | ("directory", `String d) ->
            let ext_d = String_.extend_path ~env d in
            dir := Some ext_d;
            ("directory", `String ext_d)
          | ("grs", `String d) -> ("grs", `String (String_.extend_path ~env d))
          | x -> x
        ) l in
          begin
            match (!id_flag, !dir) with
            | (None, _) -> Error.run "[Corpus_desc] ill-formed JSON file (missing `id` field) in file `%s`" json_file
            | (_, None) -> Error.run "[Corpus_desc] ill-formed JSON file (missing `directory` field) in file `%s`" json_file
            | (Some id, Some dir) ->
              let built_files =
                (Filename.concat dir (Filename.concat "_build_grew" id))
                |> read_dir
                |> List.map (fun x -> `String x) in
              `Assoc (("built_files", `List built_files) :: new_l)
          end
    | _ -> Error.run "[Corpus_desc] ill-formed JSON file (corpus desc is not a JSON object) in file `%s` " json_file

  let load_json ?(env=[]) json_file =
    try
      json_file
      |> Yojson.Basic.from_file
      |> to_list
      |> (List.map (expand_and_check ~env json_file))
    with
    | Yojson.Json_error msg -> Error.run "[Corpus_desc] JSON error `%s` in file `%s`" msg json_file
    | Type_error (msg,_) -> Error.run "[Corpus_desc] ill-formed JSON file `%s` in file `%s" msg json_file


  (* get the list of paths for all file with [extension] in the [directory] *)
  (* raises Error.run if the directory does not exist *)
  let get_full_local_files directory extension =
    try
      let files = Sys.readdir directory in
      Array.fold_right
        (fun file acc ->
          if Filename.extension file = extension
          then (Filename.concat directory file) :: acc
          else acc
        ) files []
    with Sys_error msg -> Error.run "%s" msg

  let get_files corpus_desc =
    let directory = get_directory corpus_desc in
    match member "files" corpus_desc with
    | `Null -> get_full_local_files directory ".conllu"
    | `String s -> get_full_local_files directory s
    | `List l -> List.map (fun f -> Filename.concat directory (to_string f)) l
    | _ -> Error.run "[Corpus_desc] ill-formed JSON file (unexpected `files` field)"

  (* ---------------------------------------------------------------------------------------------------- *)
  let build_corpus corpus_desc =
    let config = get_config corpus_desc in
    let conll_corpus = Conll_corpus.load_list ~config (get_files corpus_desc) in
    let columns = Conll_corpus.get_columns conll_corpus in
    let items =
      CCArray.filter_map (fun (sent_id,conll) ->
        try
          let graph = G_graph.of_json (Conll.to_json conll) in
          Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
          with Error.Build (msg, loc_opt) ->
            Warning.magenta "[build_corpus, sent_id=%s%s] skipped: %s"
              sent_id
              (match loc_opt with None -> "" | Some loc -> "; " ^ (Loc.to_string loc))
              msg;
            None
        ) (Conll_corpus.get_data conll_corpus) in
      { Corpus.items; kind=Conll (Some columns) }

  (* ---------------------------------------------------------------------------------------------------- *)
  let load_corpus_opt corpus_desc =
    let marshal_file = Filename.concat (get_build_directory corpus_desc) "marshal" in
    try
      let in_ch = open_in_bin marshal_file in
      let data = (Marshal.from_channel in_ch : Corpus.t) in
      close_in in_ch;
      Some data
    with Sys_error _ -> None

  (* ---------------------------------------------------------------------------------------------------- *)
  let table_and_desc corpus_desc conll_corpus =
    let config = get_config corpus_desc in
    let corpus_id = get_id corpus_desc in
    let build_dir = get_build_directory corpus_desc in

    (* write table file *)
    let stat = Conll_stat.build ~config ("upos", None) ("ExtPos", Some "upos") conll_corpus in
    let html = Conll_stat.to_html corpus_id ("upos", None) ("ExtPos", Some "upos") stat in
    let out_file = Filename.concat build_dir "table.html" in
    let () = CCIO.with_out out_file (fun oc -> CCIO.write_line oc html) in

    let (nb_trees, nb_tokens) = Conll_corpus.sizes conll_corpus in
    let desc = `Assoc (CCList.filter_map CCFun.id [
      Some ("nb_trees", `Int nb_trees);
      Some ("nb_tokens", `Int nb_tokens);
      (
        if get_flag "dynamic" corpus_desc
        then Some ("update", `Int (int_of_float ((Unix.gettimeofday ()) *. 1000.)))
        else None
      )
      ]) in
    let () = Yojson.Basic.to_file (Filename.concat build_dir "desc.json") desc in
    ()

  (* ---------------------------------------------------------------------------------------------------- *)
  let build_marshal_file corpus_desc =
    let config = get_config corpus_desc in
    let full_files = get_files corpus_desc in

    let build_dir = get_build_directory corpus_desc in
    let marshal_file = Filename.concat build_dir "marshal" in

    (* remove the previous log file (if any) *)
    let _ = try Unix.unlink (Filename.concat build_dir "log") with Unix.Unix_error _ -> () in
    let log_file =
      match get_kind corpus_desc with
      | Conll _ -> Some (Filename.concat build_dir "log")
      | _ -> None in

    try

      let (data : Corpus.t) =
        match get_kind corpus_desc with
        | Conll columns ->
          let conll_corpus = Conll_corpus.load_list ?log_file ~config ?columns full_files in
          let columns = Conll_corpus.get_columns conll_corpus in
          let () = table_and_desc corpus_desc conll_corpus in
          let items = CCArray.filter_map (fun (sent_id,conllx) ->
              try
                let graph = G_graph.of_json (Conll.to_json conllx) in
                Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
              with Error.Build (msg, loc_opt) ->
                Warning.magenta "[build_marshal_file, sent_id=%s%s] skipped: %s"
                  sent_id
                  (match loc_opt with None -> "" | Some loc -> "; " ^ (Loc.to_string loc))
                  msg; None
            ) (Conll_corpus.get_data conll_corpus) in
          {Corpus.items; kind= Conll (Some columns) }

        | Pst ->
          let pst_corpus = Pst_corpus.load_files full_files in
          let items = CCArray.filter_map (fun (sent_id,pst) ->
              try
                let graph = G_graph.of_pst (Parser.phrase_structure_tree pst) in
                Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
              with exc -> Warning.magenta "[id=%s] PST skipped [exception: %s]" sent_id (Printexc.to_string exc); None
            ) pst_corpus in
          {Corpus.items; kind= Pst }

        | Amr ->
          let amr_corpus =
            match full_files with
            | [one] -> Amr_corpus.load one
            | _ -> failwith "AMR multi-files corpus is not handled" in
          let items = CCArray.filter_map (fun (sent_id,amr) ->
              try
                let json = Amr.to_json ~unfold:true amr in
                let graph = G_graph.of_json json in
                let text = match G_graph.get_meta_opt "text" graph with Some t -> t | None -> "__missing text metadata__" in
                Some {Corpus.sent_id; text; graph }
              with exc -> Warning.magenta "[id=%s] AMR skipped [exception: %s]" sent_id (Printexc.to_string exc); None
            ) amr_corpus in
          {Corpus.items; kind= Amr }

        | Json | Dmrs | Ucca as kind ->
          let items = Array.concat (
              List.map (
                fun file ->
                  try Corpus.from_json ~loc: (Loc.file file) (Yojson.Basic.from_file file)
                  with Yojson.Json_error msg -> Error.run ~loc:(Loc.file file) "Error in the JSON file format: %s" msg
              ) full_files
            ) in
            {Corpus.items; kind }

        | Gr -> Error.run "Gr corpora are not supported in file compilation" in
      let _ = Info.green "[%s] %d graphs loaded" (get_id corpus_desc) (Array.length data.items) in
      let out_ch = open_out_bin marshal_file in
      Marshal.to_channel out_ch data [];
      close_out out_ch
    with
    | Conll_error json -> Warning.magenta "[Conll_error] skip corpus `%s`:\n%s" (get_id corpus_desc) (Yojson.Basic.pretty_to_string json)
    | Sys_error msg -> Warning.magenta "[Sys_error] skip corpus `%s`: %s" (get_id corpus_desc) msg
    | Error.Run (msg,_) -> Warning.magenta "[Error] skip corpus `%s`: %s" (get_id corpus_desc) msg
    | exc -> Warning.magenta "[Unexepected error] skip corpus %s\nexception: %s" (get_id corpus_desc) (Printexc.to_string exc)

  (* ---------------------------------------------------------------------------------------------------- *)
  let outdated corpus_desc built_file =
    try
      let full_built_file = Filename.concat (get_build_directory corpus_desc) built_file in
      let built_file_time = (Unix.stat full_built_file).Unix.st_mtime in
      List.exists (fun f -> (Unix.stat f).Unix.st_mtime > built_file_time) (get_files corpus_desc)
    with Unix.Unix_error _ -> true

    (* ---------------------------------------------------------------------------------------------------- *)
  let need_compile corpus_desc = outdated corpus_desc "marshal"

  (* ---------------------------------------------------------------------------------------------------- *)
  let need_validate corpus_desc =
    match get_field_opt "validation" corpus_desc with
    | Some "ud" -> outdated corpus_desc "valid_ud.txt"
    | Some "sud" -> outdated corpus_desc "valid_sud.json"
    (* TODO parseme validation??*)
    | _ -> false

  (* ---------------------------------------------------------------------------------------------------- *)
  let compile ?(force=false) corpus_desc =
    if force || (need_compile corpus_desc) then build_marshal_file corpus_desc

  (* ---------------------------------------------------------------------------------------------------- *)
  let clean corpus_desc =
    let build_dir = get_build_directory corpus_desc in
    let _ = FileUtil.rm ~recurse:true [build_dir] in
    ()

  type item = {
    request: string list; (* JSON does not support multi line strings *)
    description: string;
    level: string;
  }

  type modul = {
    title: string;
    items: item list;
    languages: string list option; (* list of the languages codes restriction, None for all lang *)
  }

  let load_modul json_file =
    let open Yojson.Basic.Util in

    let json =
      try Yojson.Basic.from_file json_file
      with Yojson.Json_error msg -> Error.run "[Corpus_desc.load_modul] file `%s`: %s" json_file msg in

    let parse_one json =
      let request =
        try json |> member "request" |> to_string |> (fun x -> [x])
        with Type_error _ ->
        try json
            |> member "request"
            |> to_list
            |> (List.map to_string)
        with Type_error (_,_) ->
          Error.run "[Corpus_desc.load_modul] file `%s`: cannot read \"request\" field " json_file in
      let description =
        try json |> member "description" |> to_string
        with Type_error _ -> "No description" in
      let level =
        try json |> member "level" |> to_string
        with Type_error _ -> "No level" in

      { request; description; level } in

    let title =
      try json |> member "title" |> to_string
      with Type_error (_,_) ->
        Error.run "[Corpus_desc.load_modul] file `%s`: cannot read \"title\" field " json_file in
        let items = List.map parse_one (json |> member "items" |> to_list) in
    let languages = try Some (json |> member "languages" |> to_list |> List.map to_string) with Type_error _ -> None in
    { title; items; languages }


  (* -------------------------------------------------------------------------------- *)
  let check modul_list out_file corpus_desc =
    let corpus = build_corpus corpus_desc in
    let config = get_config corpus_desc in
    let corpus_id = get_id corpus_desc in

    let modules =
      `List
        (CCList.filter_map
          (fun modul ->
            match (get_field_opt "lang" corpus_desc, modul.languages) with
              | (Some lang, Some lang_list) when not (List.mem lang lang_list) -> None
              | _ ->
                let (out_items : Yojson.Basic.t) =
                  `List
                    (List.map
                      (fun item ->
                        let grew_request =
                          try Request.parse ~config (String.concat "\n" item.request)
                          with Grew_utils.Error.Parse (msg, _) ->
                            Error.run "[Corpus_desc.check] cannot parse request with desc: %s (%s)" item.description msg in
                        let count =
                          Corpus.fold_left (fun acc _ graph ->
                              acc + (List.length (Matching.search_request_in_graph ~config grew_request graph))
                            ) 0 corpus in
                        `Assoc [
                          "count", `Int count;
                          "request", `List (List.map (fun x -> `String x) item.request);
                          "description", `String item.description;
                          "level", `String item.level
                        ]
                      ) modul.items
                    ) in
              Some (`Assoc ["title", `String modul.title; "items", out_items])
          ) modul_list
        ) in

    let json = `Assoc [
        "corpus", `String corpus_id;
        "date", `String (Time.now ());
        "modules", modules
      ] in

    CCIO.with_out out_file (fun out_ch -> fprintf out_ch "%s\n" (Yojson.Basic.pretty_to_string json))


  let validate_sud ~verbose ~env corpus_desc =
    printf "====validate_sud=====\n%!";
    let modules_directory = Env.get env "SUDVALIDATION" in
    let all_files = Sys.readdir modules_directory |> Array.to_list in
    let json_files = List.filter (fun file -> Filename.extension file = ".json") all_files in
    let full_files = List.map (fun file -> Filename.concat modules_directory file) json_files in
    let modules_time = List.fold_left (fun acc file -> max acc (File.last_modif file)) Float.min_float full_files in
    let validator_list = List.map load_modul full_files in

    let corpus_id = get_id corpus_desc in
    let valid_file = Filename.concat (get_build_directory corpus_desc) "valid_sud.json" in
    let valid_time = File.last_modif valid_file in
    let files = get_files corpus_desc in
    let files_time = List.fold_left (fun acc file -> max acc (File.last_modif file)) Float.min_float files in
    if valid_time > files_time && valid_time > modules_time
      then (if verbose then Info.green "%s --> SUD validation is uptodate" corpus_id)
      else
        begin
          Info.green "SUD validation of %s" corpus_id;
          check validator_list valid_file corpus_desc
        end

  let validate_ud ~verbose ~env corpus_desc =
    let validate_script = Filename.concat (Env.get env "UDTOOLS") "validate.py" in
    let corpus_id = get_id corpus_desc in
    let lang_opt = get_field_opt "lang" corpus_desc in
    let valid_file = Filename.concat (get_build_directory corpus_desc) "valid_ud.txt" in
    let valid_time = File.last_modif valid_file in
    let files = get_files corpus_desc in
    let files_time = List.fold_left (fun acc file -> max acc (File.last_modif file)) Float.min_float files in
    if valid_time > files_time
    then (if verbose then Info.green "%s --> UD validation is uptodate" corpus_id)
    else
      let out_ch = open_out valid_file in
      Printf.fprintf out_ch "%s\n" (Time.now ());
      let args =
        match lang_opt with
        | Some l -> sprintf "--lang=%s" l
        | None ->
          Printf.fprintf out_ch "WARNING: no lang defined, validation only up to level 3\n";
          "--lang=unknown --level=3" in
      close_out out_ch;
        List.iter (fun file ->
          Info.green "UD validation of file %s [in corpus %s]" (Filename.basename file) corpus_id;
        let out_ch = open_out_gen [Open_append] 0o644 valid_file in
        Printf.fprintf out_ch "================================ %s ================================\n" (Filename.basename file);
        close_out out_ch;
        let command = sprintf "%s %s --max-err 0 %s 2>>  %s || true" validate_script args file valid_file in
        match Sys.command command with
          | 0 -> ()
          | _ -> Warning.magenta "Error when running UD Python validation script on file %s" (Filename.basename file);
      ) files



  let validate ?(verbose=false) ?(env=[]) corpus_desc =
    try
      match get_field_opt "validation" corpus_desc with
      | None -> ()  (* No validation defined for this corpus *)
      | Some "ud" | Some "UD" -> validate_ud ~verbose ~env corpus_desc
      | Some "sud" | Some "SUD" -> validate_sud ~verbose ~env corpus_desc
      | Some s -> Error.run "Unknown validation `%s`" s
    with Unix.Unix_error (_x,_y,_z) -> Warning.magenta "Skip `%s`, Error %s %s" (get_id corpus_desc) _y _z

  let show corpus_desc =
    Info.green "<><><> %s <><><>" (get_id corpus_desc) ;
    match corpus_desc with
    | `Assoc l ->
      let indent = List.fold_left
        (fun acc (k,_) -> max acc (String.length k)) 0 l in
      List.iter
      (fun (k,v) ->
        Info.blue "   %s%s --> %s%!"
          (String.make (indent - (String.length k)) ' ')
          k
          (Yojson.Basic.pretty_to_string v)
      ) l
    | _ -> assert false

  end (* module Corpus_desc *)
