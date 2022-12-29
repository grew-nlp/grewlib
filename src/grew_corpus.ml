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
open Conllx
open Amr

open Grew_types
open Grew_utils
open Grew_loader
open Grew_edge
open Grew_graph
open Grew_rule
open Grew_grs

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
    | Conll of Conllx_columns.t option (* value is None in Corpus_desc and Some c once the corpus is really loaded *)
    | Pst | Amr | Gr | Json | Dmrs

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
           let text = match List.assoc_opt "text" (Conllx.get_meta conllx) with Some t -> t | None -> "__missing text metadata__" in
           let graph = conllx |> Conllx.to_json |> G_graph.of_json in
           { sent_id; text; graph }
        ) (Conllx_corpus.get_data conllx_corpus) in
    { kind = Conll (Some (Conllx_corpus.get_columns conllx_corpus)); items }

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

  let foldi_left fct init t =
    CCArray.foldi
      (fun acc i item -> fct acc i item.sent_id item.graph)
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
      of_conllx_corpus (Conllx_corpus.of_lines ?log_file ?config lines)

  let from_string ?ext ?log_file ?config s =
    match ext with
    | Some ".json" -> { kind=Json; items = from_json (Yojson.Basic.from_string s)}
    | Some ".conll" | Some ".conllu" | Some ".cupt" | Some ".orfeo" | Some ".frsemcor" 
    | _ -> (* TODO: use Conll by default --> more robust stuff needed *)
      let lines = Str.split (Str.regexp "\n") s in
      of_conllx_corpus (Conllx_corpus.of_lines ?log_file ?config lines)

  let from_file ?ext ?log_file ?config file =
    let extension = match ext with Some e -> e | None -> Filename.extension file in
    match extension with
    | ".conll" | ".conllu" | ".cupt" | ".orfeo" | ".frsemcor" ->
      of_conllx_corpus (Conllx_corpus.load ?log_file ?config file)
    | ".amr" | ".txt" ->
      of_amr_file file
    | ".json" ->
      begin
        try { kind=Json; items = from_json ~loc: (Loc.file file) (Yojson.Basic.from_file file)}
        with Yojson.Json_error msg -> Error.run ~loc:(Loc.file file) "Error in the JSON file format: %s" msg
      end
    | ext -> Error.run "Cannot load file `%s`, unknown extension `%s`" file ext

  let from_dir ?log_file ?config dir =
    let files = Sys.readdir dir in
    let (conll_files, amr_files, txt_files) =
      Array.fold_right
        (fun file (conll_acc, amr_acc, txt_acc) ->
           match Filename.extension file with
           | ".conll" | ".conllu" | ".cupt" | ".orfeo" -> (file::conll_acc, amr_acc, txt_acc)
           | ".amr" -> (conll_acc, file::amr_acc, txt_acc)
           | ".txt" -> (conll_acc, amr_acc, file::txt_acc)
           | _ -> (conll_acc, amr_acc, txt_acc)
        ) files ([],[],[]) in

    (* txt files are interpreted as AMR files only if there is no conll-like files (eg: UD containts txt files in parallel to conllu) *)
    match (conll_files, amr_files, txt_files) with
    | ([],[],[]) -> Error.run "The directory `%s` does not contain any graphs" dir
    | (conll_files,[],_) -> of_conllx_corpus (Conllx_corpus.load_list ?log_file ?config conll_files)
    | ([],amr_files, txt_files) -> (amr_files @ txt_files) |> List.map of_amr_file |> merge
    | _ -> Error.run "The directory `%s` contains both Conll data and Amr data" dir

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
  let search ~config null update request cluster_item_list corpus =
    fold_left
    (fun acc sent_id graph ->
      let matchings = Matching.search_request_in_graph ~config request graph in
      List.fold_left
      (fun acc2 matching -> 
        let cluster_value_list = 
          List.map 
          (fun cluster_item ->
            Matching.get_clust_value_opt ~config cluster_item request graph matching 
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
            check graph_counter;
            let graph_index = permut_fct graph_counter in
            let graph = get_graph graph_index corpus in
            let sent_id = get_sent_id graph_index corpus in
            let matchings = Matching.search_request_in_graph ~config request graph in
            let nb_in_graph = List.length matchings in
            let new_acc = 
              CCList.foldi
                (fun acc2 pos_in_graph matching ->
                  incr matching_counter; 
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
end

(* ==================================================================================================== *)
module Corpus_desc = struct

  type t = {
    id: string;
    lang: string option;
    kind: Corpus.kind;
    config: Conllx_config.t; (* "ud" is used as the default: TODO make config mandatory in desc? *)
    directory: string;
    files: string list;
    rtl: bool;
    audio: bool;
    dynamic: bool; (* the corpus is supposed to be updated dynamically *)
    display: int option; (* None --> dep, Some -1 --> graph, Some i≥0 --> subgraph at depth i *)
    preapply: string option;
  }

  let get_id corpus_desc = corpus_desc.id
  let get_lang_opt corpus_desc = corpus_desc.lang
  let get_config corpus_desc = corpus_desc.config
  let get_directory corpus_desc = corpus_desc.directory
  let is_rtl corpus_desc = corpus_desc.rtl
  let is_audio corpus_desc = corpus_desc.audio
  let get_display corpus_desc = corpus_desc.display
  
  let build id directory = { id; lang=None; kind=Conll None; config=Conllx_config.build "ud"; directory; files=[]; rtl=false; audio=false; dynamic=false; display=None; preapply=None}
  (* ---------------------------------------------------------------------------------------------------- *)
  let extensions = function
    | Corpus.Conll _ -> [".conll"; ".conllu"; ".cupt"; ".orfeo"; "frsemcor"]
    | Amr -> [".amr"; ".txt"]
    | Pst -> [".const"]
    | Json -> [".json"]
    | Gr -> [".gr"]
    | Dmrs -> [".json"]

  (* ---------------------------------------------------------------------------------------------------- *)
  (* if [files] is empty, all files of the directory with correct suffix are considered *)
  let get_full_files { kind; directory; files } =
    let file_list = match files with
      | [] ->
        begin
          try
            Array.fold_left
              (fun acc file -> if List.mem (Filename.extension file) (extensions kind) then file::acc else acc)
              [] (Sys.readdir directory)
          with Sys_error _ -> Error.run "[Corpus] cannot read directory %s" directory
        end
      | l -> l in
    List.map (fun f -> Filename.concat directory f) file_list

  (* ---------------------------------------------------------------------------------------------------- *)
  let build_corpus corpus_desc =
    let config = corpus_desc.config in
    match corpus_desc.kind with
    | Conll _ ->
      let conll_corpus = Conllx_corpus.load_list ~config (get_full_files corpus_desc) in
      let columns = Conllx_corpus.get_columns conll_corpus in

      let items =
        CCArray.filter_map (fun (sent_id,conll) ->
            try
              let init_graph = G_graph.of_json (Conllx.to_json conll) in
              let graph = match corpus_desc.preapply with
                | Some grs -> Grs.apply ~config grs init_graph
                | None -> init_graph in
              Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
            with Error.Build (msg, loc_opt) ->
              Error.warning "[build_corpus, sent_id=%s%s] skipped: %s"
                sent_id
                (match loc_opt with None -> "" | Some loc -> "; " ^ (Loc.to_string loc))
                msg; None
          ) (Conllx_corpus.get_data conll_corpus) in
      { Corpus.items; kind=Conll (Some columns) }
    | _ -> Error.bug "[Corpus_desc.build_corpus] is available only on Conll format"

  (* ---------------------------------------------------------------------------------------------------- *)
  let load_corpus_opt corpus_desc =
    let marshal_file = (Filename.concat corpus_desc.directory corpus_desc.id) ^ ".marshal" in
    try
      let in_ch = open_in_bin marshal_file in
      let data = (Marshal.from_channel in_ch : Corpus.t) in
      close_in in_ch;
      Some data
    with Sys_error _ -> None


  (* ---------------------------------------------------------------------------------------------------- *)
  let load_json json_file =
    let open Yojson.Basic.Util in

    let parse_corpus json =
      let id =
        try json |> member "id" |> to_string
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"id\" field is mandatory and must be a string" json_file in

      let lang =
        try Some (json |> member "lang" |> to_string)
        with Type_error _ -> None in

      let kind =
        try match (json |> member "kind" |> to_string_option, json |> member "columns" |> to_string_option) with
          | (None, columns_opt) | (Some "conll", columns_opt) -> Corpus.Conll (CCOption.map Conllx_columns.build columns_opt)
          | (Some "pst",_) -> Pst
          | (Some "amr",_) -> Amr
          | (Some "dmrs",_) -> Dmrs
          | (Some "json",_) -> Json
          | (Some x,_) -> Error.run "[Corpus.load_json] Unknown \"kind\":\"%s\" field in file: \"%s\"" x json_file
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"kind\" must be a string" json_file in

      let config =
        try json |> member "config" |> to_string_option |> (function Some c -> Conllx_config.build c | None -> Conllx_config.build "ud")
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"config\" field must be a string" json_file in

      let directory =
        try json |> member "directory" |> to_string
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"directory\" field is mandatory and must be a string" json_file in

      let preapply =
        try json |> member "preapply" |> to_string_option
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"preapply\" field must be a string" json_file in

      let display =
        try Some (json |> member "display" |> to_int)
        with Type_error _ -> None in

      let files =
        try json |> member "files" |> to_list |> filter_string
        with Type_error _ -> [] in

      let rtl =
        try json |> member "rtl" |> to_bool
        with Type_error _ -> false in

      let audio =
        try json |> member "audio" |> to_bool
        with Type_error _ -> false in

      let dynamic =
        try json |> member "dynamic" |> to_bool
        with Type_error _ -> false in

      { id; lang; kind; config; directory; files; rtl; audio; dynamic; preapply; display; } in


    let json =
      try Yojson.Basic.from_file json_file with 
      | Sys_error _ -> Error.run "[Corpus.load_json] file `%s` not found" json_file
      | Yojson.Json_error msg -> Error.run "[Corpus.load_json] invalid JSON file `%s`:\n%s" json_file msg in
  
    try List.map parse_corpus (json |> member "corpora" |> to_list)
    with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] Unexpected JSON data" json_file

  (* ---------------------------------------------------------------------------------------------------- *)
  let grew_match_table_and_desc corpus_desc dir_opt corpus =
    let name = corpus_desc.id in
    match dir_opt with
    | None -> ()
    | Some dir ->
      let stat = Conllx_stat.build ~config:corpus_desc.config ("upos", None) ("ExtPos", Some "upos") corpus in
      let html = Conllx_stat.to_html name ("upos", None) ("ExtPos", Some "upos")  stat in
      let out_file = Filename.concat dir (name ^ "_table.html") in
      CCIO.with_out out_file (fun oc -> CCIO.write_line oc html);

      let (nb_trees, nb_tokens) = Conllx_corpus.sizes corpus in
      let desc = `Assoc (CCList.filter_map CCFun.id [
          Some ("nb_trees", `Int nb_trees);
          Some ("nb_tokens", `Int nb_tokens);
          (
            if corpus_desc.dynamic
            then Some ("update", `Int (int_of_float ((Unix.gettimeofday ()) *. 1000.)))
            else None
          )
        ]) in
      Yojson.Basic.to_file (Filename.concat dir (name ^ "_desc.json")) desc;

      (* ---------------------------------------------------------------------------------------------------- *)
  exception Skip
  let ensure_dir dir =
    try (* catch if dir does not exist *)
      begin (* test if "dir" exists but is not a directory *)
        match Unix.stat dir with
        | { Unix.st_kind = Unix.S_DIR } -> ()
        | _ -> Error.warning "grew_match option ignored: %s already exists and is not directory" dir; raise Skip
      end; ()
    with Unix.Unix_error (Unix.ENOENT,_,_) ->
      begin (* dir does not exist -> try to create it *)
        try Unix.mkdir dir 0o755
        with exc -> Error.warning "grew_match option ignored: cannot create dir %s (%s)" dir (Printexc.to_string exc); raise Skip
      end

  (* ---------------------------------------------------------------------------------------------------- *)
  (* [grew_match] is a folder where tables, logs and corpus desc is stored *)
  let build_marshal_file ?grew_match corpus_desc =
    let config = corpus_desc.config in
    let full_files = get_full_files corpus_desc in
    let marshal_file = (Filename.concat corpus_desc.directory corpus_desc.id) ^ ".marshal" in

    let (grew_match_dir, log_file) =
      match (corpus_desc.kind, grew_match) with
      | (Conll _, Some dir) ->
        begin
          try
            ensure_dir dir;
            let log = Filename.concat dir (sprintf "%s.log" corpus_desc.id) in
            try close_out (open_out log); (Some dir, Some log)
            with exc -> Error.warning "grew_match option ignored: cannot create file in dir %s (%s)" dir (Printexc.to_string exc); raise Skip
          with Skip -> (None,None)
        end
      | _ -> (None, None) in

    try
      let (data : Corpus.t) = match corpus_desc.kind with
        | Conll columns ->
          let conll_corpus = Conllx_corpus.load_list ?log_file ~config:corpus_desc.config ?columns full_files in
          let columns = Conllx_corpus.get_columns conll_corpus in
          grew_match_table_and_desc corpus_desc grew_match_dir conll_corpus;
          let items = CCArray.filter_map (fun (sent_id,conllx) ->
              try
                let init_graph = G_graph.of_json (Conllx.to_json conllx) in
                let graph = match corpus_desc.preapply with
                  | Some grs -> Grs.apply ~config grs init_graph
                  | None -> init_graph in
                Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
              with Error.Build (msg, loc_opt) ->
                Error.warning "[build_marshal_file, sent_id=%s%s] skipped: %s"
                  sent_id
                  (match loc_opt with None -> "" | Some loc -> "; " ^ (Loc.to_string loc))
                  msg; None
            ) (Conllx_corpus.get_data conll_corpus) in
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

        | Json | Dmrs ->
          let items = Array.concat (
              List.map (
                fun file ->
                  try Corpus.from_json ~loc: (Loc.file file) (Yojson.Basic.from_file file)
                  with Yojson.Json_error msg -> Error.run ~loc:(Loc.file file) "Error in the JSON file format: %s" msg
              ) full_files
            ) in
          {Corpus.items; kind= corpus_desc.kind }

        | Gr -> Error.run "Gr corpora are not supported in file compilation" in
      let _ = Error.info "[%s] %d graphs loaded" corpus_desc.id (Array.length data.items) in
      let out_ch = open_out_bin marshal_file in
      Marshal.to_channel out_ch data [];
      close_out out_ch
    with
    | Conllx_error json -> Error.warning "[Conllx_error] fail to load corpus %s, skip it\nexception: %s" corpus_desc.id (Yojson.Basic.pretty_to_string json)
    | Error.Run (msg,_) -> Error.warning "[Libgrew error] %s, fail to load corpus %s: skip it" msg corpus_desc.id
    | exc -> Error.warning "[Error] fail to load corpus %s, skip it\nexception: %s" corpus_desc.id (Printexc.to_string exc)


  (* ---------------------------------------------------------------------------------------------------- *)
  let compile ?(force=false) ?grew_match corpus_desc =
    let full_files = get_full_files corpus_desc in
    let marshal_file = (Filename.concat corpus_desc.directory corpus_desc.id) ^ ".marshal" in
    let really_marshal () = build_marshal_file ?grew_match corpus_desc in
    if force
    then really_marshal ()
    else
      try
        let marshal_time = (Unix.stat marshal_file).Unix.st_mtime in
        if List.exists (fun f -> (Unix.stat f).Unix.st_mtime > marshal_time) full_files
        then really_marshal () (* one of the data files is more recent than the marshal file *)
        else Error.info "--> %s is uptodate" corpus_desc.id
      with
      | Unix.Unix_error _ ->
        (* the marshal file does not exists *)
        really_marshal ()

  (* ---------------------------------------------------------------------------------------------------- *)
  let clean {kind; id; directory; files; preapply}  =
    let marshal_file = (Filename.concat directory id) ^ ".marshal" in
    if Sys.file_exists marshal_file then Unix.unlink marshal_file

end (* module Corpus_desc *)