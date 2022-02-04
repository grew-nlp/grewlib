(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Conllx
open Libamr

open Grew_base
open Grew_loader
open Grew_edge
open Grew_graph
open Grew_grs

(* ==================================================================================================== *)
module Pst_corpus = struct
  let load_files files =
    let sub_corp = List.map
        (fun file ->
           let line_list = File.read file in
           List.mapi
             (fun i line ->
                match Str.split (Str.regexp "\t") line with
                | [pst] -> (sprintf "%s_%05d" file (i+1), pst)
                | [id; pst] -> (id, pst)
                | _ -> failwith "Pst syntax error"
             ) line_list
        ) files in
    Array.of_list (List.flatten sub_corp)
end

(* ==================================================================================================== *)
module Corpus = struct
  type kind = Conll | Pst | Amr | Gr | Json | Dmrs

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
    { kind = Conll; items }

  let of_amr_file file =
    try
      let amr_corpus = Amr_corpus.load file in
      let items =
        Array.map
          (fun (sent_id, amr) ->
             let json = Amr.to_json amr in
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
    | Conll | Dmrs -> true
    | _ -> false

  let get_text position t = t.items.(position).text


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
    | ".gr" ->
      let item = Loader.gr file |> G_graph.of_ast ~config:(Conllx_config.build "basic") |> item_of_graph in
      { kind= Gr; items = [| item |] }
    | ".json" ->
      begin
        try { kind=Json; items = from_json ~loc: (Loc.file file) (Yojson.Basic.from_file file)}
        with Yojson.Json_error msg -> Error.run ~loc:(Loc.file file) "Error in the JSON file format: %s" msg
      end
    | ".melt" | ".brown" ->
      let lines = File.read file in
      let config = match config with Some c -> c | None -> Conllx_config.build "ud" in
      let items = List.mapi (fun i line -> {
            sent_id= sprintf "%05d" (i + 1);
            text= "__No_text__";
            graph= G_graph.of_brown ~config line;
          }) lines |> Array.of_list in
      { items; kind=Conll }
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
end

(* ==================================================================================================== *)
module Corpus_desc = struct

  type t = {
    id: string;
    kind: Corpus.kind;
    config: Conllx_config.t; (* "ud" is used as the default: TODO make config mandatory in desc? *)
    columns: Conllx_columns.t option;
    directory: string;
    files: string list;
    rtl: bool;
    audio: bool;
    preapply: string option;
  }

  let get_id corpus_desc = corpus_desc.id
  let get_config corpus_desc = corpus_desc.config
  let get_directory corpus_desc = corpus_desc.directory
  let is_rtl corpus_desc = corpus_desc.rtl
  let is_audio corpus_desc = corpus_desc.audio


  (* ---------------------------------------------------------------------------------------------------- *)
  let extensions = function
    | Corpus.Conll -> [".conll"; ".conllu"; ".cupt"; ".orfeo"; "frsemcor"]
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
    | Conll ->
      let conll_corpus = Conllx_corpus.load_list ~config (get_full_files corpus_desc) in
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
      { Corpus.items; kind=Conll }
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

    let json =
      try Yojson.Basic.from_file json_file
      with 
      | Sys_error _ -> Error.run "[Corpus.load_json] file `%s` not found" json_file
      | Yojson.Json_error msg -> Error.run "[Corpus.load_json] invalid JSON file `%s`:\n%s" json_file msg
    in

    let parse_one json =
      let id =
        try json |> member "id" |> to_string
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"id\" field is mandatory and must be a string" json_file in

      let kind =
        try match json |> member "kind" |> to_string_option with
          | None | Some "conll" -> Corpus.Conll
          | Some "pst" -> Pst
          | Some "amr" -> Amr
          | Some "dmrs" -> Dmrs
          | Some "json" -> Json
          | Some x -> Error.run "[Corpus.load_json] Unknown \"kind\":\"%s\" field in file: \"%s\"" x json_file
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"kind\" must be a string" json_file in

      let config =
        try json |> member "config" |> to_string_option |> (function Some c -> Conllx_config.build c | None -> Conllx_config.build "ud")
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"config\" field must be a string" json_file in

      let columns =
        try json |> member "columns" |> to_string_option |> (CCOption.map Conllx_columns.build)
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"columns\" field must be a string" json_file in

      let directory =
        try json |> member "directory" |> to_string
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"directory\" field is mandatory and must be a string" json_file in

      let preapply =
        try json |> member "preapply" |> to_string_option
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"preapply\" field must be a string" json_file in

      let files =
        try json |> member "files" |> to_list |> filter_string
        with Type_error _ -> [] in

      let rtl =
        try json |> member "rtl" |> to_bool
        with Type_error _ -> false in

      let audio =
        try json |> member "audio" |> to_bool
        with Type_error _ -> false in

      { id; kind; config; columns; directory; files; rtl; audio; preapply; } in

    List.map parse_one (json |> member "corpora" |> to_list)

  (* ---------------------------------------------------------------------------------------------------- *)
  let grew_match_table_and_desc ?config ~tmp_gm2 dir_opt name corpus =
    match dir_opt with
    | None -> ()
    | Some dir ->
      let stat = Conllx_stat.build ?config ("upos", None) ("ExtPos", Some "upos") corpus in
      let html = Conllx_stat.to_html ~tmp_gm2 name ("upos", None) ("ExtPos", Some "upos")  stat in
      let out_file = Filename.concat dir (name ^ "_table.html") in
      CCIO.with_out out_file (fun oc -> CCIO.write_line oc html);

      let (nb_trees, nb_tokens) = Conllx_corpus.sizes corpus in
      let desc = `Assoc (CCList.filter_map CCFun.id [
        Some ("nb_trees", `Int nb_trees);
        Some ("nb_tokens", `Int nb_tokens);
        (
          if List.exists (fun suf -> CCString.suffix ~suf name) ["latest"; "dev"; "master"; "conv"]
          then Some ("update", `Int (int_of_float ((Unix.gettimeofday ()) *. 1000.)))
          else None
        )
      ]) in
      Yojson.Basic.to_file (Filename.concat dir (name ^ "_desc.json")) desc;

      let date =
        if List.exists (fun suf -> CCString.suffix ~suf name) ["latest"; "dev"; "master"; "conv"]
        then let t = Unix.gmtime (Unix.time ()) in
          sprintf "&nbsp;updated: %d/%02d/%02d %02d:%02d" (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
        else "" in
      let valid =
        if CCString.suffix ~suf:"conv" name
        then
          let corpus = String.sub name 0 ((String.length name) - 5) in
          sprintf "&nbsp;<a href=\"_valid/%s.valid\"><button class=\"btn btn-primary btn-results btn-sm\">Validation</button></a>" corpus
        else "" in
      let (nb_trees, nb_tokens) = Conllx_corpus.sizes corpus in
      let meta = sprintf "&nbsp;[%d trees, %d tokens]%s%s" nb_trees nb_tokens date valid in
      let out_file = Filename.concat dir (name ^ "_desc.html") in
      CCIO.with_out out_file (fun oc -> CCIO.write_line oc meta)

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
  let build_marshal_file ?grew_match ~tmp_gm2 corpus_desc =
    let config = corpus_desc.config in
    let full_files = get_full_files corpus_desc in
    let marshal_file = (Filename.concat corpus_desc.directory corpus_desc.id) ^ ".marshal" in

    let (grew_match_dir, log_file) =
      match (corpus_desc.kind, grew_match) with
      | (Conll, Some dir) ->
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
      let items = match corpus_desc.kind with
        | Conll ->
          let conll_corpus = Conllx_corpus.load_list ?log_file ~config:corpus_desc.config ?columns:corpus_desc.columns full_files in
          grew_match_table_and_desc ~config:corpus_desc.config ~tmp_gm2 grew_match_dir corpus_desc.id conll_corpus;
          CCArray.filter_map (fun (sent_id,conllx) ->
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
            ) (Conllx_corpus.get_data conll_corpus)

        | Pst ->
          let pst_corpus = Pst_corpus.load_files full_files in
          CCArray.filter_map (fun (sent_id,pst) ->
              try
                let graph = G_graph.of_pst (Parser.phrase_structure_tree pst) in
                Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph }
              with exc -> Error.warning "[id=%s] PST skipped [exception: %s]" sent_id (Printexc.to_string exc); None
            ) pst_corpus

        | Amr ->
          let amr_corpus = match full_files with
            | [one] -> Amr_corpus.load one
            | _ -> failwith "AMR multi-files corpus is not handled"
          in
          CCArray.filter_map (fun (sent_id,amr) ->
              try
                let json = Amr.to_json amr in
                let graph = G_graph.of_json json in
                let text = match G_graph.get_meta_opt "text" graph with Some t -> t | None -> "__missing text metadata__" in
                Some {Corpus.sent_id; text; graph }
              with exc -> Error.warning "[id=%s] AMR skipped [exception: %s]" sent_id (Printexc.to_string exc); None
            ) amr_corpus
        | Json | Dmrs ->
          Array.concat (
            List.map (
              fun file ->
                try Corpus.from_json ~loc: (Loc.file file) (Yojson.Basic.from_file file)
                with Yojson.Json_error msg -> Error.run ~loc:(Loc.file file) "Error in the JSON file format: %s" msg
            ) full_files
          )
        | Gr -> Error.run "Gr corpora are not supported in file compilation" in
      let _ = Error.warning "[%s] %d graphs loaded" corpus_desc.id (Array.length items) in
      let out_ch = open_out_bin marshal_file in
      let (data : Corpus.t) = {Corpus.items; kind=corpus_desc.kind } in
      Marshal.to_channel out_ch data [];
      close_out out_ch
    with
    | Conllx_error json -> Error.warning "[Conllx_error] fail to load corpus %s, skip it\nexception: %s" corpus_desc.id (Yojson.Basic.pretty_to_string json)
    | Error.Run (msg,_) -> Error.warning "[Libgrew error] %s, fail to load corpus %s: skip it" msg corpus_desc.id
    | exc -> Error.warning "[Error] fail to load corpus %s, skip it\nexception: %s" corpus_desc.id (Printexc.to_string exc)


  (* ---------------------------------------------------------------------------------------------------- *)
  let compile ?(force=false) ?grew_match ?(tmp_gm2=false) corpus_desc =
    let full_files = get_full_files corpus_desc in
    let marshal_file = (Filename.concat corpus_desc.directory corpus_desc.id) ^ ".marshal" in
    let really_marshal () = build_marshal_file ?grew_match ~tmp_gm2 corpus_desc in
    if force
    then really_marshal ()
    else
      try
        let marshal_time = (Unix.stat marshal_file).Unix.st_mtime in
        if List.exists (fun f -> (Unix.stat f).Unix.st_mtime > marshal_time) full_files
        then really_marshal () (* one of the data files is more recent than the marshal file *)
        else Error.warning "--> %s is uptodate" corpus_desc.id
      with
      | Unix.Unix_error _ ->
        (* the marshal file does not exists *)
        really_marshal ()

  (* ---------------------------------------------------------------------------------------------------- *)
  let clean {kind; id; directory; files; preapply}  =
    let marshal_file = (Filename.concat directory id) ^ ".marshal" in
    if Sys.file_exists marshal_file then Unix.unlink marshal_file
end