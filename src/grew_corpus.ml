open Printf
open Log
open Conll
open Conllx
open Libamr

open Grew_base
open Grew_domain
open Grew_loader
open Grew_edge
open Grew_graph
open Grew_grs

(* ==================================================================================================== *)
module Pst_corpus = struct
  let load files =
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
  type kind = Conll | Pst | Amr

  type item = {
    sent_id: string;
    text: string;
    graph: G_graph.t;
    kind: kind;
  }

  type t = {
    domain: Domain.t option;
    items: item array;
  }

  let fold_left fct init t =
    Array.fold_left
      (fun acc item -> fct acc item.graph)
      init t.items

  let size t = Array.length t.items

  let get_domain_opt t = t.domain

  let get_graph position t = t.items.(position).graph
  let get_sent_id position t = t.items.(position).sent_id

  let is_conll position t = t.items.(position).kind = Conll

  let get_text position t = t.items.(position).text


  let permut_length t =
    let items_with_length =
      Array.mapi
        (fun i item -> (i,G_graph.size item.graph)) t.items in
    let _ = Array.sort
        (fun (_,s1) (_,s2) -> Stdlib.compare s1 s2)
        items_with_length in
    Array.map fst items_with_length

end

(* ==================================================================================================== *)
module Corpus_desc = struct

  type t = {
    id: string;
    kind: Corpus.kind;
    config: Conllx_config.t;
    columns: Conllx_columns.t option;
    dom_file: string option;
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
    | Corpus.Conll -> [".conll"; ".conllu"; ".cupt"; ".orfeo"]
    | Amr -> [".amr"; ".txt"]
    | Pst -> [".const"]


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
  let build_domain corpus_desc = match corpus_desc.dom_file with
    | None -> None
    | Some file ->
      try Some (Grs.domain_build (Grew_loader.Loader.domain file))
      with _ ->
        Log.fwarning "corpus \"%s\", fail to load domain \"%s\", load corpus without domain" corpus_desc.id file;
        None

  (* ---------------------------------------------------------------------------------------------------- *)
  let build_corpus corpus_desc =
    let domain = build_domain corpus_desc in
    let config = corpus_desc.config in
    match corpus_desc.kind with
    | Conll ->
      let conll_corpus = Conll_corpus.load_list ~tf_wf:true (get_full_files corpus_desc) in
      let items =
        CCArray.filter_map (fun (sent_id,conll) ->
            try
              let init_graph = G_graph.of_conll ?domain ~config conll in
              let graph = match corpus_desc.preapply with
                | Some grs -> Grs.apply ~config grs init_graph
                | None -> init_graph in
              Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph; kind=Conll}
            with Error.Build (msg, loc_opt) ->
              Log.fwarning "[build_corpus, sent_id=%s%s] skipped: %s"
              sent_id
              (match loc_opt with None -> "" | Some loc -> "; " ^ (Loc.to_string loc))
              msg; None
          ) conll_corpus in
      { Corpus.domain; items }
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

    let json = Yojson.Basic.from_file json_file in

    let parse_one json =
      let id =
        try json |> member "id" |> to_string
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"id\" field is mandatory and must be a string" json_file in

      let kind =
        try match json |> member "kind" |> to_string_option with
          | None | Some "conll" -> Corpus.Conll
          | Some "pst" -> Pst
          | Some "amr" -> Amr
          | Some x -> Error.run "[Corpus.load_json] Unknown \"kind\":\"%s\" field in file: \"%s\"" x json_file
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"kind\" must be a string" json_file in

      let config =
        try json |> member "config" |> to_string_option |> (function Some c -> Conllx_config.build c | None -> Conllx_config.default)
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"config\" field must be a string" json_file in

      let columns =
        try json |> member "columns" |> to_string_option |> (CCOpt.map Conllx_columns.build)
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"columns\" field must be a string" json_file in

      let dom_file =
        try json |> member "domain" |> to_string_option
        with Type_error _ -> Error.run "[Corpus.load_json, file \"%s\"] \"domain\" field must be a string" json_file in

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

      { id; kind; config; columns; dom_file; directory; files; rtl; audio; preapply; } in

    List.map parse_one (json |> member "corpora" |> to_list)

  (* ---------------------------------------------------------------------------------------------------- *)
  let grew_match_table_and_desc ?config dir_opt name corpus =
    match dir_opt with
    | None -> ()
    | Some dir ->
      let stat = Conllx_stat.build ?config Conllx_stat.Upos corpus in
      let html = Conllx_stat.to_html name stat in
      let out_file = Filename.concat dir (name ^ "_table.html") in
      CCIO.with_out out_file (fun oc -> CCIO.write_line oc html);

      let date =
        if List.exists (fun suf -> CCString.suffix ~suf name) ["latest"; "dev"; "master"; "conv"]
        then let t = Unix.gmtime (Unix.time ()) in
          sprintf "&nbsp;last update: %d/%02d/%02d" (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
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
        | _ -> Log.fwarning "grew_match option ignored: %s already exists and is not directory" dir; raise Skip
      end; ()
    with Unix.Unix_error (Unix.ENOENT,_,_) ->
      begin (* dir does not exist -> try to create it *)
        try Unix.mkdir dir 0o755
        with exc -> Log.fwarning "grew_match option ignored: cannot create dir %s (%s)" dir (Printexc.to_string exc); raise Skip
      end

  (* ---------------------------------------------------------------------------------------------------- *)
  (* [grew_match] is a folder where tables, logs and corpus desc is stored *)
  let build_marshal_file ?grew_match corpus_desc =

    let domain = build_domain corpus_desc in
    let config = corpus_desc.config in
    let full_files = get_full_files corpus_desc in
    let marshal_file = (Filename.concat corpus_desc.directory corpus_desc.id) ^ ".marshal" in

    let (grew_match_dir, log_file) =
      match (corpus_desc.kind, grew_match) with
      | (Corpus.Amr,_) | (Pst,_) | (Conll, None) -> (None, None)
      | (Conll, Some dir) ->
        try
          ensure_dir dir;
          let log = Filename.concat dir (sprintf "%s.log" corpus_desc.id) in
          try close_out (open_out log); (Some dir, Some log)
          with exc -> Log.fwarning "grew_match option ignored: cannot create file in dir %s (%s)" dir (Printexc.to_string exc); raise Skip
        with Skip -> (None,None) in

    try
      let items = match corpus_desc.kind with
        | Conll ->
          let conll_corpus = Conllx_corpus.load_list ~config:corpus_desc.config ?columns:corpus_desc.columns full_files in
          grew_match_table_and_desc ~config:corpus_desc.config grew_match_dir corpus_desc.id conll_corpus;
          CCArray.filter_map (fun (sent_id,conllx) ->
              try
                let init_graph = G_graph.of_grew_json (Conllx.to_json conllx) in
                let graph = match corpus_desc.preapply with
                  | Some grs -> Grs.apply ~config grs init_graph
                  | None -> init_graph in
                Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph; kind=Conll}
            with Error.Build (msg, loc_opt) ->
              Log.fwarning "[build_marshal_file, sent_id=%s%s] skipped: %s"
              sent_id
              (match loc_opt with None -> "" | Some loc -> "; " ^ (Loc.to_string loc))
              msg; None
            ) (Conllx_corpus.get_data conll_corpus)

        | Pst ->
          let pst_corpus = Pst_corpus.load full_files in
          CCArray.filter_map (fun (sent_id,pst) ->
              try
                let graph = G_graph.of_pst ?domain (Parser.phrase_structure_tree pst) in
                Some {Corpus.sent_id; text=G_graph.to_sentence graph; graph; kind=Pst}
              with exc -> Log.fwarning "[id=%s] PST skipped [exception: %s]" sent_id (Printexc.to_string exc); None
            ) pst_corpus

        | Amr ->
          let amr_corpus = match full_files with
            | [one] -> Amr_corpus.load one
            | _ -> failwith "AMR multi-files corpus is not handled"
          in
          CCArray.filter_map (fun (sent_id,text,amr) ->
              try
                let gr = Amr.to_gr amr in
                let graph = G_graph.of_ast ?domain ~config (Parser.gr gr) in
                Some {Corpus.sent_id; text; graph; kind=Amr}
              with exc -> Log.fwarning "[id=%s] AMR skipped [exception: %s]" sent_id (Printexc.to_string exc); None
            ) amr_corpus in
      let _ = Log.fmessage "[%s] %d graphs loaded" corpus_desc.id (Array.length items) in
      let out_ch = open_out_bin marshal_file in
      let (data : Corpus.t) = {Corpus.domain; items} in
      Marshal.to_channel out_ch data [];
      close_out out_ch
    with
    | Conll_error json -> Log.fwarning "[Conll_error] fail to load corpus %s, skip it\nexception: %s" corpus_desc.id (Yojson.Basic.pretty_to_string json)
    | Conllx_error json -> Log.fwarning "[Conllx_error] fail to load corpus %s, skip it\nexception: %s" corpus_desc.id (Yojson.Basic.pretty_to_string json)
    | Error.Run (msg,_) -> Log.fwarning "[Libgrew error] %s, fail to load corpus %s: skip it" msg corpus_desc.id
    | exc -> Log.fwarning "[Error] fail to load corpus %s, skip it\nexception: %s" corpus_desc.id (Printexc.to_string exc)


  (* ---------------------------------------------------------------------------------------------------- *)
  let compile ?grew_match corpus_desc =
    let full_files = get_full_files corpus_desc in
    let marshal_file = (Filename.concat corpus_desc.directory corpus_desc.id) ^ ".marshal" in
    let really_marshal () = build_marshal_file ?grew_match corpus_desc in
    try
      let marshal_time = (Unix.stat marshal_file).Unix.st_mtime in
      match corpus_desc.dom_file with
      | Some f when (Unix.stat f).Unix.st_mtime > marshal_time ->
        (* the domain file is more recent than the marshal file *)
        really_marshal ()
      | _ when List.exists (fun f -> (Unix.stat f).Unix.st_mtime > marshal_time) full_files ->
        (* one of the data files is more recent than the marshal file *)
        really_marshal ()
      | _ -> Log.fmessage "--> %s is uptodate" corpus_desc.id
    with
    | Unix.Unix_error _ ->
      (* the marshal file does not exists *)
      really_marshal ()

  (* ---------------------------------------------------------------------------------------------------- *)
  let clean {kind; id; dom_file; directory; files; preapply}  =
    let marshal_file = (Filename.concat directory id) ^ ".marshal" in
    if Sys.file_exists marshal_file then Unix.unlink marshal_file
end