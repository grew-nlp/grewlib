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
open Grew_types
open Grew_utils
open Grew_graph
open Grew_grs
open Grew_corpus

(* ==================================================================================================== *)
module Corpusbank = struct
  type t = Corpus_desc.t String_map.t

  let iter ?(filter=fun _ -> true) fct t =
    String_map.iter
    (fun corpus_id corpus_desc ->
      if filter corpus_id
      then fct corpus_id corpus_desc
    ) t

  let fold ?(filter=fun _ -> true) fct t init =
    String_map.fold
    (fun corpus_id corpus_desc acc ->
      if filter corpus_id
      then fct corpus_id corpus_desc acc
      else acc
    ) t init

  let read_files files =
    let data =
      List.fold_left
        (fun acc file ->
          List.fold_left
            (fun acc2 desc ->
              let id = Corpus_desc.get_id desc in
                if String_map.mem id acc2
                then Error.run "Duplicate definition of corpus_id `%s`" id
                else String_map.add id desc acc2
            ) acc (Corpus_desc.load_json file)
      ) String_map.empty files in
    data

  let read_directory dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun file -> Filename.extension file = ".json")
    |> List.map (fun file -> Filename.concat dir file)
    |> read_files

  let load input =
    match File.get_path_status input with
    | Directory -> read_directory input
    | File -> read_files [input]
    | Dont_exist -> Error.run "corpusbank `%s` not found" input

  let build_filter patterns =
    let extended_patterns = List.map (fun s -> "*"^s^"*") patterns in
    let string_pattern =
      match extended_patterns with
      | [] -> "match(*)"
      | _ ->String.concat " & " (List.map (fun s -> "match("^s^")") extended_patterns) in
    Info.green "<========= Corpora matching: %s =========>" string_pattern;
    let anon_regexp = List.map (fun s -> Re.compile (Re.Glob.glob ~anchored:true s)) extended_patterns in
    let filter id =
      match anon_regexp with
      | [] -> true
      | l -> List.for_all (fun re -> Re.execp re id) l in
    filter

  let get_corpus_desc_opt corpusbank corpus_id =
    String_map.find_opt corpus_id corpusbank

  type status =
    | Ok
    | Need_validate
    | Need_compile
    | Need_build
    | Need_rebuild of string list
    | Err of string

  let string_of_status = function
    | Ok -> "Ok"
    | Need_validate -> "Need_validate"
    | Need_compile -> "Need_compile"
    | Need_build -> "Need_build"
    | Need_rebuild _ -> "Need_rebuild"
    | Err _ -> "Error"

  let print_of_status stat s =
    match stat with
    | "Ok" -> Info.green s
    | "Error" -> Info.red s
    | "Need_build" -> Info.magenta s
    | _ -> Info.blue s

  let _color_of_status = function
    | "Ok" -> ANSITerminal.green
    | "Error" -> ANSITerminal.red
    | "Need_build" -> ANSITerminal.magenta
    | _ -> ANSITerminal.blue


  let grs_timestamps = ref String_map.empty

  let get_grs_timestamp grs_file =
    match String_map.find_opt grs_file !grs_timestamps with
    | Some s -> s
    | None ->
      (* Load with a default config... just for getting the timestamp --> config must be in GRS!! *)
      let grs = Grs.load ~config:(Conll_config.build "sud") grs_file in
      let grs_timestamp = grs |> Grs.get_timestamp_opt |> CCOption.get_exn_or "Bug grs_timestamp" in
      grs_timestamps := String_map.add grs_file grs_timestamp !grs_timestamps;
      grs_timestamp

  let status_when_build_ok corpus_desc =
    let directory = Corpus_desc.get_directory corpus_desc in
    match File.get_path_status directory with
    | File -> Err (sprintf "`%s` exists but it is not a directory" directory)
    | Dont_exist -> Err (sprintf "directory `%s` does not exist" directory)
    | Directory ->
      if Corpus_desc.need_compile corpus_desc
      then Need_compile
        else if Corpus_desc.need_validate corpus_desc
        then Need_validate
        else Ok

  let build_status_map ~filter corpusbank =
    let rec update corpus_id acc =
      match get_corpus_desc_opt corpusbank corpus_id with
      | None -> String_map.add corpus_id (Err (sprintf "No desc found for `%s`" corpus_id)) acc
      | Some corpus_desc ->
        match String_map.find_opt corpus_id acc with
        | Some _ -> acc (* already computed in a previous recursive call *)
        | None ->
          match (Corpus_desc.get_field_opt "src" corpus_desc, Corpus_desc.get_field_opt "grs" corpus_desc) with
          | (None, None) -> String_map.add corpus_id (status_when_build_ok corpus_desc) acc
          | (None, Some _) -> Error.build "corpus `%s` is described with a `grs` but without `src`" corpus_id
          | (Some _, None) -> Error.build "corpus `%s` is described with a `src` but without `grs`" corpus_id
          | (Some src_corpus_id, Some grs_file) ->
            let directory = Corpus_desc.get_directory corpus_desc in
            match File.get_path_status directory with
            | File -> String_map.add corpus_id (Err (sprintf "corpus `%s`: `%s` exists but it is not a directory" corpus_id directory)) acc
            | Dont_exist ->  String_map.add corpus_id Need_build acc
            | Directory ->
              let new_acc = update src_corpus_id acc in (* update status of src_corpus first *)
              begin
                match String_map.find src_corpus_id new_acc with
                | Err _ -> String_map.add corpus_id (Err (sprintf "depend on erroneous `%s`" src_corpus_id)) acc
                | Need_build | Need_rebuild _ -> String_map.add corpus_id (Need_rebuild [sprintf "depend on unbuilt `%s`" src_corpus_id]) acc
                | _ ->
                  match get_corpus_desc_opt corpusbank src_corpus_id with
                  | None -> assert false (* None is caught in previous match as `Err` *)
                  | Some src_corpus_desc ->
                    try
                      let src_files = Corpus_desc.get_files src_corpus_desc in
                      let directory = Corpus_desc.get_directory corpus_desc in
                      let old_tar_files = ref (String_set.of_list (Corpus_desc.get_files corpus_desc)) in
                      let grs_timestamp = get_grs_timestamp grs_file in
                      let msg_list = List.fold_left
                        (fun acc src ->
                          let tar_basename = Filename.basename src in (* TODO: handle replacement in names like ud -> sud *)
                          let tar = Filename.concat directory tar_basename in
                          old_tar_files := String_set.remove tar !old_tar_files;
                          if max grs_timestamp (File.last_modif src) > (File.last_modif tar)
                          then (sprintf "file `%s` need to be rebuilt" tar) :: acc
                          else acc
                        ) [] src_files in
                      match (msg_list, !old_tar_files |> String_set.to_seq |> List.of_seq) with
                      | ([], []) -> String_map.add corpus_id (status_when_build_ok corpus_desc) acc
                      | (msg_list, unwanted_files) ->
                        let unwanted_msg_list = List.map (fun f -> sprintf "file `%s` must be removed" f) unwanted_files in
                        String_map.add corpus_id (Need_rebuild (msg_list @ unwanted_msg_list)) acc
                    with exc -> String_map.add corpus_id (Err (sprintf "Unexpected exception, for corpus_id `%s`, %s" corpus_id (Printexc.to_string exc))) acc
              end in
    fold ~filter (
      fun corpus_id _ acc -> update corpus_id acc
    ) corpusbank String_map.empty

  let print_status ?(verbose=false) ?(filter=(fun _ -> true)) corpusbank =
    let status = build_status_map ~filter corpusbank in
    let counters = ref String_map.empty in
    let count s =
      let new_sum = match String_map.find_opt s !counters with None -> 1 | Some n -> n+1 in
      counters := String_map.add s new_sum !counters in
    String_map.iter (
      fun corpus_id stat_corpus ->
        let string_status = string_of_status stat_corpus in
        let () = count string_status in
        let print = print_of_status string_status in
        match stat_corpus with
        | Ok -> if verbose then    print "OK -------------> %s" corpus_id
        | Need_validate ->         print "need validate --> %s" corpus_id;
        | Need_compile ->          print "need compile ---> %s" corpus_id;
        | Need_build ->            print "need build -----> %s" corpus_id;
        | Need_rebuild msg_list -> print "need rebuild ---> %s" corpus_id;
            if verbose then List.iter (fun msg -> print "    ➔ %s%!" msg) msg_list
        | Err msg -> Info.red "Error ----------> %s [%s]" corpus_id msg;
          ()

    ) status;
    Info.print "----------------------------------%!";
    String_map.iter
      (fun stat count ->
        (print_of_status stat ) "%15s ----> %d%!" stat count
      ) !counters;

    Info.print "%15s ----> %d%!" "Total" (String_map.fold (fun _ c acc -> c + acc) !counters 0);
    Info.print "----------------------------------%!"

  let transform grs_config columns grs strat text_from_tokens (src_config, src_file) (tar_config, tar_file) =
    let fix = if text_from_tokens then Conll.text_from_tokens else CCFun.id in
    let src_corpus = Corpus.from_file ~config:src_config src_file in
    let out_ch = open_out tar_file in
    Corpus.iteri
      (fun _ _ gr ->
      (* Counter.print index len sent_id; *)
      (* Grew_grs.Grs.simple_rewrite ~config grs strat gr *)
        match Grs.simple_rewrite ~config:grs_config grs strat gr with
          | [graph] -> fprintf out_ch "%s\n" (graph |> G_graph.to_json |> Conll.of_json |> fix |> Conll.to_string ~config:tar_config ~columns)
          | _ -> Error.run "More than one normal form (src_file=%s)" src_file
      ) src_corpus;
      (* Counter.finish (); *)
      (* final (); *)
      close_out out_ch

  let rec build_derived corpusbank corpus_desc =
    let corpus_id = Corpus_desc.get_id corpus_desc in
    let columns = 
      Corpus_desc.get_field_opt "columns" corpus_desc 
      |> CCOption.map_or (Conll_columns.of_list << (CCString.split_on_char ' ')) ~default:Conll_columns.default in
    match (Corpus_desc.get_field_opt "src" corpus_desc, Corpus_desc.get_field_opt "grs" corpus_desc, Corpus_desc.get_field_opt "strat" corpus_desc) with
    | (None, _, _) -> () (* this is a native corpus *)
    | (Some _, None, _) -> Info.red "ERROR: in description for corpus_id: `%s`, src but no grs" corpus_id
    | (Some src_corpus_id, Some grs_file, strat_opt) ->
      match get_corpus_desc_opt corpusbank src_corpus_id with
      | None -> Info.red "ERROR: no description for src_corpus_id: `%s`" src_corpus_id
      | Some src_corpus_desc ->
          (* first, recursively build until native corpus *)
          let () = build_derived corpusbank src_corpus_desc in

          let directory = Corpus_desc.get_directory corpus_desc in
          let text_from_tokens = Corpus_desc.get_flag "text_from_tokens" corpus_desc in
          File.ensure_directory directory;

          let grs_config =
            match Corpus_desc.get_field_opt "grs_config" corpus_desc with
            | Some c -> Conll_config.build c
            | None -> Corpus_desc.get_config corpus_desc in (* If no grs_config is defined, tar_config is used *)

          let grs = Grs.load ~config:grs_config grs_file in
          let grs_timestamp = grs |> Grs.get_timestamp_opt |> CCOption.get_exn_or "Bug grs_timestamp" in
          let strat = strat_opt |> CCOption.get_or ~default:"main" in

          let src_files = Corpus_desc.get_files src_corpus_desc in
          let src_config = Corpus_desc.get_config src_corpus_desc in

          let tar_config = Corpus_desc.get_config corpus_desc in

          let old_tar_files = ref (String_set.of_list (Corpus_desc.get_files corpus_desc)) in
          let tgz_needed = ref false in

          let () = List.iter
            (fun src ->
              let tar_basename = Filename.basename src in (* TODO: handle replacement in names like ud -> sud *)
              let tar = Filename.concat directory tar_basename in
              old_tar_files := String_set.remove tar !old_tar_files;
              if max grs_timestamp (File.last_modif src) > (File.last_modif tar)
              then
                begin
                  Info.green "corpus `%s` build %s" corpus_id (Filename.basename tar) ;
                  tgz_needed := true;
                  transform grs_config columns grs strat text_from_tokens (src_config,src) (tar_config,tar)
                end
            ) src_files in

          let () = String_set.iter
            (fun f ->
              Info.green "remove file %s\n%!" f;
              Unix.unlink f
            ) !old_tar_files in

          let () =
            if !tgz_needed
            then
              let build_dir = Corpus_desc.get_build_directory corpus_desc in
              let corpus_id = Corpus_desc.get_id corpus_desc in
              let name = Filename.basename directory in
              let tgz_file = Filename.concat build_dir (sprintf "%s.tgz" corpus_id) in
              let parent = Filename.dirname directory in
              let cmd = sprintf "cd %s && tar zcf %s %s/*.conllu" parent tgz_file name in
              begin
                match Sys.command cmd with
                | 0 -> Info.green "updated file %s" tgz_file;
                | _ -> Warning.magenta "error in %s.tgz production" tgz_file
              end in

          ()

  let compile ?(filter=fun _ -> true) corpusbank =
    let status_map = build_status_map ~filter corpusbank in
    iter ~filter
      (fun corpus_id corpus_desc ->
        match String_map.find corpus_id status_map with
        | Need_compile -> Corpus_desc.compile corpus_desc
        | Need_build | Need_rebuild _ -> Warning.magenta "Skip `%s`, build is needed before compile" corpus_id
        | Err msg -> Warning.magenta "Skip `%s`, Error: %s" corpus_id msg
        | Ok | Need_validate -> ()
      ) corpusbank

  let build ?(filter=fun _ -> true) corpusbank =
    let status_map = build_status_map ~filter corpusbank in
    iter ~filter
      (fun corpus_id corpus_desc ->
        match String_map.find corpus_id status_map with
        | Need_build | Need_rebuild _ -> build_derived corpusbank corpus_desc
        | Err msg -> Warning.magenta "Skip `%s`, Error: %s" corpus_id msg
        | Need_compile | Ok | Need_validate -> ()
      ) corpusbank
end
