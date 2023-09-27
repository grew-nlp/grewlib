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

  let is_conll t = match t.kind with
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
  let search ?(json_label=false) ~config null update request cluster_item_list corpus =
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
          Clustered.update (update sent_id graph matching) cluster_value_list null acc2
      ) acc matchings
    ) (Clustered.empty null) corpus

  (* ---------------------------------------------------------------------------------------------------- *)
  type status = 
    | Ok
    | Timeout of float
    | Over of float
  
  let bounded_search ~config ?(ordering = None) bound timeout null update request cluster_item_list corpus =
    let len = size corpus in
    let permut_fct = match ordering with
    | Some "length" -> let perm = permut_length corpus in fun x -> perm.(x)
    | Some "shuffle" -> let mix = Array_.shuffle_N len in fun x -> mix.(x)
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
              CCList.foldi
                (fun acc2 pos_in_graph matching ->
                  incr matching_counter; 
                  check graph_counter;
                  if !status <> Ok
                  then acc2
                  else
                    let cluster_value_list = 
                      List.map 
                        (fun cluster_item ->
                          Matching.get_clust_value_opt ~config cluster_item request graph matching 
                        ) cluster_item_list in
                      Clustered.update (update graph_index sent_id pos_in_graph nb_in_graph matching) cluster_value_list null acc2
                ) acc matchings in
            loop new_acc (graph_counter + 1)
          end in
    loop (Clustered.empty null) 0 
    |> (fun x -> match !status with
    | Ok -> (x, "complete", 1.)
    | Timeout r -> (x, "timeout", r)
    | Over r -> (x, "max_results", r)
    )

  let compile dir marshal_file t =
    let out_ch = open_out_bin (Filename.concat dir marshal_file) in
    Marshal.to_channel out_ch t [];
    close_out out_ch
  
end

(* ==================================================================================================== *)
module Corpus_desc = struct
  open Yojson.Basic.Util

  type t = Yojson.Basic.t

  let get_id corpus_desc = corpus_desc |> member "id" |> to_string

  let get_field_opt field corpus_desc = corpus_desc |> member field |> to_string_option

  let get_config corpus_desc =
    try corpus_desc |> member "config" |> to_string_option |> CCOption.map_or ~default:(Conll_config.build "ud") Conll_config.build
    with Type_error _ -> Error.run "[Corpus_desc.get_config] \"config\" field must be a string in %s" (get_id corpus_desc)

  let get_directory corpus_desc =
    try corpus_desc |> member "directory" |> to_string
    with Type_error _ -> Error.run "[Corpus_desc.get_directory] \"directory\" field is mandatory and must be a string in %s" (get_id corpus_desc)

  let is_rtl corpus_desc =
    try corpus_desc |> member "rtl" |> to_bool
    with Type_error _ -> false

  let is_audio corpus_desc =
    try corpus_desc |> member "audio" |> to_bool
    with Type_error _ -> false

    let is_dynamic corpus_desc =
    try corpus_desc |> member "dynamic" |> to_bool
    with Type_error _ -> false

  let get_display corpus_desc =
      try Some (corpus_desc |> member "display" |> to_int)
      with Type_error _ -> None

  let get_kind corpus_desc =
    try match (corpus_desc |> member "kind" |> to_string_option, corpus_desc |> member "columns" |> to_string_option) with
      | (None, columns_opt) | (Some "conll", columns_opt) -> Corpus.Conll (CCOption.map Conll_columns.build columns_opt)
      | (Some "pst",_) -> Pst
      | (Some "amr",_) -> Amr
      | (Some "dmrs",_) -> Dmrs
      | (Some "ucca",_) -> Ucca
      | (Some "json",_) -> Json
      | (Some x,_) -> Error.run "[Corpus.load_json] Unknown \"kind\":\"%s\" field in corpus: \"%s\"" x (get_id corpus_desc)
    with Type_error _ -> Error.run "[Corpus.load_json] \"kind\" must be a string in corpus: \"%s\"" (get_id corpus_desc)


  let load_json json_file =
    json_file |> Yojson.Basic.from_file |> to_list

  exception Dir_not_found of string

  (* get the list of paths for all file with [extension] in the [folder] *)
  (* raises Dir_not_found if the directory does not exist *)
  let get_files base folder extension = 
    let full_folder = Filename.concat base folder in 
    try 
      let files = Sys.readdir full_folder in
      Array.fold_right
        (fun file acc ->
          if Filename.extension file = extension
          then (Filename.concat full_folder file) :: acc
          else acc
        ) files []
    with Sys_error _ -> raise (Dir_not_found full_folder)

  let get_full_files corpora_folder corpus_desc =
    let folder = get_directory corpus_desc in
    match member "files" corpus_desc with
    | `Null -> get_files corpora_folder folder ".conllu"
    | `String s -> get_files corpora_folder folder s
    | `List l -> List.map (fun f -> Filename.concat folder (to_string f)) l
    | _ -> failwith "Type error"

  (* ---------------------------------------------------------------------------------------------------- *)
  let build_corpus corpora_folder corpus_desc =
    let config = get_config corpus_desc in
    let conll_corpus = Conll_corpus.load_list ~config (get_full_files corpora_folder corpus_desc) in
    let columns = Conll_corpus.get_columns conll_corpus in
    let items =
      CCArray.filter_map (fun (sent_id,conll) ->
        try
          let graph = G_graph.of_json (Conll.to_json conll) in
          Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
          with Error.Build (msg, loc_opt) ->
            Error.warning "[build_corpus, sent_id=%s%s] skipped: %s"
              sent_id
              (match loc_opt with None -> "" | Some loc -> "; " ^ (Loc.to_string loc))
              msg;
            None
        ) (Conll_corpus.get_data conll_corpus) in
      { Corpus.items; kind=Conll (Some columns) }

  (* ---------------------------------------------------------------------------------------------------- *)
  let load_corpus_opt corpora_folder corpus_desc =
    let corpus_folder = Filename.concat corpora_folder (get_directory corpus_desc) in
    let marshal_file = Filename.concat corpus_folder ((get_id corpus_desc) ^ ".marshal") in
    try
      let in_ch = open_in_bin marshal_file in
      let data = (Marshal.from_channel in_ch : Corpus.t) in
      close_in in_ch;
      Some data
    with Sys_error _ -> None

  (* ---------------------------------------------------------------------------------------------------- *)
  let table_and_desc corpora_folder corpus_desc corpus =
    let corpus_id = get_id corpus_desc in
    let config = get_config corpus_desc in
    let corpus_folder = Filename.concat corpora_folder (get_directory corpus_desc) in

    (* write table file *)
    let stat = Conll_stat.build ~config ("upos", None) ("ExtPos", Some "upos") corpus in
    let html = Conll_stat.to_html corpus_id ("upos", None) ("ExtPos", Some "upos") stat in
    let out_file = Filename.concat corpus_folder (corpus_id ^ "_table.html") in
    let () = CCIO.with_out out_file (fun oc -> CCIO.write_line oc html) in

    let (nb_trees, nb_tokens) = Conll_corpus.sizes corpus in
    let desc = `Assoc (CCList.filter_map CCFun.id [
        Some ("nb_trees", `Int nb_trees);
        Some ("nb_tokens", `Int nb_tokens);
          (
            if is_dynamic corpus_desc
            then Some ("update", `Int (int_of_float ((Unix.gettimeofday ()) *. 1000.)))
            else None
          )
        ]) in
      let () = Yojson.Basic.to_file (Filename.concat corpus_folder (corpus_id ^ "_desc.json")) desc in
      ()

  (* ---------------------------------------------------------------------------------------------------- *)
  (* [grew_match] is a folder where tables, logs and corpus desc are stored *)
  let build_marshal_file corpora_folder corpus_desc =
    let config = get_config corpus_desc in
    let corpus_id = get_id corpus_desc in
    let full_files = get_full_files corpora_folder corpus_desc in
    List.iter (fun x -> printf "+++++++ %s\n%!" x) full_files;
    let corpus_folder = Filename.concat corpora_folder (get_directory corpus_desc) in
    let marshal_file = Filename.concat corpus_folder ((get_id corpus_desc) ^ ".marshal") in

    let log_file =
      match get_kind corpus_desc with
      | Conll _ ->
        begin
          let log = Filename.concat corpus_folder (sprintf "%s.log" corpus_id) in
          try 
            close_out (open_out log); 
            Some log
          with exc -> Error.warning "Cannot create file %s (%s)" log (Printexc.to_string exc); None
        end
      | _ -> None in

    try
      let (data : Corpus.t) = match get_kind corpus_desc with
        | Conll columns ->
          let conll_corpus = Conll_corpus.load_list ?log_file ~config ?columns full_files in
          let columns = Conll_corpus.get_columns conll_corpus in
          table_and_desc corpora_folder corpus_desc conll_corpus;
          let items = CCArray.filter_map (fun (sent_id,conllx) ->
              try
                let graph = G_graph.of_json (Conll.to_json conllx) in
                Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
              with Error.Build (msg, loc_opt) ->
                Error.warning "[build_marshal_file, sent_id=%s%s] skipped: %s"
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
              with exc -> Error.warning "[id=%s] PST skipped [exception: %s]" sent_id (Printexc.to_string exc); None
            ) pst_corpus in
          {Corpus.items; kind= Pst }

        | Amr ->
          let amr_corpus = match full_files with
            | [one] -> Amr_corpus.load one
            | _ -> failwith "AMR multi-files corpus is not handled"
          in
          let items = CCArray.filter_map (fun (sent_id,amr) ->
              try
                let json = Amr.to_json ~unfold:true amr in
                let graph = G_graph.of_json json in
                let text = match G_graph.get_meta_opt "text" graph with Some t -> t | None -> "__missing text metadata__" in
                Some {Corpus.sent_id; text; graph }
              with exc -> Error.warning "[id=%s] AMR skipped [exception: %s]" sent_id (Printexc.to_string exc); None
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
      let _ = Error.info "[%s] %d graphs loaded" (get_id corpus_desc) (Array.length data.items) in
      let out_ch = open_out_bin marshal_file in
      Marshal.to_channel out_ch data [];
      close_out out_ch
    with
    | Conll_error json -> Error.warning "[Conll_error] fail to load corpus %s, skip it\nexception: %s" (get_id corpus_desc) (Yojson.Basic.pretty_to_string json)
    | Error.Run (msg,_) -> Error.warning "[Error] %s, fail to load corpus %s: skip it" msg (get_id corpus_desc)
    | exc -> Error.warning "[Error] fail to load corpus %s, skip it\nexception: %s" (get_id corpus_desc) (Printexc.to_string exc)

  (* get the list of paths for all file with [extension] in the [folder] *)
  let list_files base folder extension = 
    let full_folder = Filename.concat base folder in 
    try 
      let files = Sys.readdir full_folder in
      Array.fold_right
        (fun file acc ->
          if Filename.extension file = extension
          then (Filename.concat folder file) :: acc
          else acc
        ) files []
    with Sys_error _ -> Error.run "Directory not found: %s" full_folder

  let get_files corpora_folder corpus_desc =
    let folder = get_directory corpus_desc in
    match member "files" corpus_desc with
    | `Null -> list_files corpora_folder folder ".conllu"
    | `String s -> list_files corpora_folder folder s
    | `List l -> List.map (fun f -> Filename.concat folder (to_string f)) l
    | _ -> failwith "Type error"


  (* ---------------------------------------------------------------------------------------------------- *)
  let compile ?(force=false) corpora_folder corpus_desc =
    let full_files = get_full_files corpora_folder corpus_desc in
    let corpus_folder = Filename.concat corpora_folder (get_directory corpus_desc) in
    let marshal_file = Filename.concat corpus_folder ((get_id corpus_desc) ^ ".marshal") in
    let really_marshal () = build_marshal_file corpora_folder corpus_desc in
    if force
    then really_marshal ()
    else
      try
        let marshal_time = (Unix.stat marshal_file).Unix.st_mtime in
        if List.exists (fun f -> (Unix.stat f).Unix.st_mtime > marshal_time) full_files
        then really_marshal () (* one of the data files is more recent than the marshal file *)
        else Error.info "--> %s is uptodate" (get_id corpus_desc)
      with
      | Unix.Unix_error _ ->
        (* the marshal file does not exists *)
        really_marshal ()

  (* ---------------------------------------------------------------------------------------------------- *)
  let clean corpora_folder corpus_desc  =
  let corpus_folder = Filename.concat corpora_folder (get_directory corpus_desc) in
  let marshal_file = Filename.concat corpus_folder ((get_id corpus_desc) ^ ".marshal") in
  if Sys.file_exists marshal_file then Unix.unlink marshal_file

end (* module Corpus_desc *)
