open Log
open Printf 

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

module IntSet = Set.Make (struct type t = int let compare = Pervasives.compare end)

module IntMap = Map.Make (struct type t = int let compare = Pervasives.compare end)

module Pid = struct
  type t = int
  let compare = Pervasives.compare
end

module Pid_map =
  struct 
    include Map.Make (Pid)
(** returns the image of a map [m]*)

    exception True
    let exists fct map =
      try
        iter 
          (fun key value -> 
            if fct key value 
            then raise True
          ) map;
        false
      with True -> true

    let range key_set m = 
      IntSet.fold (fun k s -> (IntSet.add (find k m) s)) key_set IntSet.empty

    let keys m = 
      fold (fun k v s -> (IntSet.add k s)) m IntSet.empty

(* union of two maps*)
    let union_map m m' = fold (fun k v m'' -> (add k v m'')) m m'

    exception MatchNotInjective

(*
 * union of two injective maps having different ranges :
 * \forall x \neq y \in m: m(x) \neq m(y)
 * \forall x' \neq y' \in m': m'(x) \neq m'(y)W
 * \forall x \in m /\ m': m(x) = m'(x)
 * \forall x \in m : x \not\in\m' => \forall y \in m' m(x) \neq m'(y)
 *)
    let union_if m m' = 
      let keys_m = keys m in
      let keys_m' = keys m' in
      let inter_keys = IntSet.inter keys_m keys_m' in
      if IntSet.for_all (fun elt -> (find elt m) = (find elt m')) inter_keys
      then 
        let keys_s_m' = IntSet.diff keys_m' inter_keys in
        let range_m = range keys_m m in 
        let range_m' = range keys_s_m' m' in
        if (IntSet.inter range_m range_m') = IntSet.empty
        then union_map m m'
        else raise MatchNotInjective
      else raise MatchNotInjective          
  end
    
module Loc = struct
  type t = string * int 

  let to_string (file,line) = Printf.sprintf "(file: %s, line: %d)" (Filename.basename file) line

  let opt_to_string = function
    | None -> ""
    | Some x -> to_string x
end

 



module File = struct
  let write data name =
    let out_ch = open_out name in
    fprintf out_ch "%s\n" data;
    close_out out_ch

  let read file = 
    let in_ch = open_in file in
    let rev_lines = ref [] in
    try
      while true do
        let line = input_line in_ch in
        if (Str.string_match (Str.regexp "^[ \t]*$") line 0) || (line.[0] = '%')
        then ()
        else rev_lines := line :: !rev_lines
      done; assert false
    with End_of_file -> 
      close_in in_ch;
      List.rev !rev_lines
 end

module Array_ = struct
  let dicho_mem elt array =
    let rec loop low high =
      (if low > high 
      then false
      else
        match (low+high)/2 with
        | middle when array.(middle) = elt -> true
        | middle when array.(middle) < elt -> loop (middle+1) high
        | middle -> loop low (middle - 1)
      ) in 
    loop 0 ((Array.length array) - 1)

  (* dichotomic search in a sorted array *)
  let dicho_find elt array =
    let rec loop low high =
      (if low > high then raise Not_found);
      match (low+high)/2 with
      | middle when array.(middle) = elt -> middle
      | middle when array.(middle) < elt -> loop (middle+1) high
      | middle -> loop low (middle - 1) in 
    loop 0 ((Array.length array) - 1)
      
  let dicho_find_assoc elt array =
    let rec loop low high =
      (if low > high then raise Not_found);
      match (low+high)/2 with
      | middle when fst array.(middle) = elt -> middle
      | middle when fst array.(middle) < elt -> loop (middle+1) high
      | middle -> loop low (middle - 1) in 
    loop 0 ((Array.length array) - 1)
end


module List_ = struct
  let rec rm elt = function
    | [] -> raise Not_found
    | x::t when x=elt -> t
    | x::t -> x::(rm elt t)

  let pos x l = 
    let rec loop i = function
    | [] -> None
    | h::t when h=x -> Some i
    | _::t -> loop (i+1) t in
    loop 0 l

  let rec opt = function
    | [] -> []
    | None :: t -> opt t
    | Some x :: t -> x :: (opt t)

  let rec opt_map f = function
    | [] -> []
    | x::t -> 
        match f x with
        | None -> opt_map f t
        | Some r -> r :: (opt_map f t)

  let rec flat_map f = function
    | [] -> []
    | x::t -> (f x)@(flat_map f t)

  let iteri fct = 
    let rec loop i = function 
      | [] -> ()
      | h::t -> (fct i h); (loop (i+1) t) in
    loop 0
      
  let mapi fct = 
    let rec loop i = function 
      | [] -> []
      | h::t -> let head = fct i h in head :: (loop (i+1) t)
    in loop 0

  let foldi_left f init l =
    fst 
      (List.fold_left 
         (fun (acc,i) elt -> (f i acc elt, i+1))
         (init,0) l
      )

  let rec remove elt = function
    | [] -> raise Not_found
    | a::tail when a = elt -> tail
    | a::tail -> a::(remove elt tail)

  let to_string string_of_item sep = function
    | [] -> ""
    | h::t -> List.fold_left (fun acc elt -> acc ^ sep ^ (string_of_item elt)) (string_of_item h) t

  let rec sort_insert elt = function
    | [] -> [elt]
    | h::t when elt<h -> elt::h::t 
    | h::t -> h::(sort_insert elt t)

  let rec sort_mem elt = function
    | [] -> false
    | h::_ when elt<h -> false
    | h::_ when elt=h -> true
    | h::t (* when elt>h *) -> sort_mem elt t

  let rec sort_assoc key = function
    | [] -> None
    | (k,_)::_ when key<k -> None
    | (k,_)::t when key>k -> sort_assoc key t
    | (_,v)::_ -> Some v 

  let rec sort_remove_assoc key = function
    | [] -> []
    | (k,_)::_ as t when key<k -> t
    | (k,v)::t when key>k -> (k,v) :: (sort_remove_assoc key t)
    | (_,v)::t -> t


  exception Usort

  let rec usort_remove key = function 
    | [] -> raise Not_found
    | x::t when key < x -> raise Not_found
    | x::t when key = x -> t
    | x::t -> x::(usort_remove key t)

  let usort_insert ?(compare=Pervasives.compare) elt l =
    let rec loop = function
      | [] -> [elt]
      | x::t when compare elt x < 0 -> elt :: x :: t
      | x::t when compare elt x > 0 -> x :: (loop t)
    | _ -> raise Usort in
    try Some (loop l) with Usort -> None

  let rec sort_disjoint l1 l2 = 
    match (l1,l2) with
    | [], _ | _, [] -> true
    | h1::t1 , h2::t2 when h1<h2 -> sort_disjoint t1 l2
    | h1::t1 , h2::t2 when h1>h2 -> sort_disjoint l1 t2
    | _ -> false 

  let sort_is_empty_inter l1 l2 = 
    let rec loop = function
      | [], _ | _, [] -> true
      | x1::t1, x2::t2 when x1 < x2 -> loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> false in
    loop (l1,l2) 

  let sort_inter l1 l2 = 
    let rec loop = function
      | [], _ | _, [] -> []
      | x1::t1, x2::t2 when x1 < x2 -> loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> x1 :: loop (t1, t2) in
    loop (l1,l2)
      
  exception Not_disjoint
  let sort_disjoint_union ?(compare=Pervasives.compare) l1 l2 = 
    let rec loop = function
      | [], l | l, [] -> l
      | x1::t1, x2::t2 when (compare x1 x2) < 0 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when (compare x1  x2) > 0 -> x2 :: loop (x1::t1, t2)
      | _ -> raise Not_disjoint in
    loop (l1,l2)
      
  let sort_include l1 l2 = 
    let rec loop = function
      | [], l -> true
      | l, [] -> false
      | x1::t1, x2::t2 when x1 < x2 -> false
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> loop (t1, t2) in
    loop (l1,l2)
      
  let sort_included_diff l1 l2 = 
    let rec loop = function
      | [], l -> failwith "[sort_included_diff] not included"
      | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> failwith "[sort_included_diff] not included"
      | x1::t1, x2::t2 -> loop (t1, t2) in
    loop (l1,l2)

  let sort_diff l1 l2 = 
    let rec loop = function
      | [], l -> []
      | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> loop (t1, t2) in
    loop (l1,l2)

  let foldi_left f init l =
    fst 
      (List.fold_left 
	 (fun (acc,i) elt -> (f i acc elt, i+1))
	 (init,0) l
      )
end


module Massoc = struct
  (* Massoc is implemented with caml lists *)
  (* invariant: we suppose that all 'a list in the structure are not empty! *) 
  type 'a t = (int * 'a list) list

  let empty = []

  let is_empty t = (t=[])

  let rec assoc key = function
    | [] -> []
    | (h,_)::_ when key<h -> []
    | (h,v)::t when key=h -> v
    | (h,_)::t (* when key>h *) -> assoc key t

  let to_string elt_to_string t = 
    List_.to_string 
      (fun (i,elt_list) -> 
        sprintf "%d -> [%s]" i (List_.to_string elt_to_string "," elt_list)
      ) "; " t
    
  let iter fct t =
    List.iter 
      (fun (key,list) ->
        List.iter 
          (fun elt -> fct key elt)
          list
      ) t

  let rec add key elt = function
    | [] -> Some [(key, [elt])]
    | (h,list)::t when h=key -> 
        (match List_.usort_insert elt list with 
        | Some new_list -> Some ((h, new_list)::t)
        | None -> None
        )
    | ((h,_)::_) as t when key<h -> Some ((key,[elt])::t)
    | (h,l)::t (* when key>h *) -> 
        match (add key elt t) with Some t' -> Some ((h,l)::t') | None -> None

  let fold_left fct init t =
    List.fold_left 
      (fun acc (key,list) ->
        List.fold_left 
          (fun acc2 elt ->
            fct acc2 key elt)
          acc list)
      init t

  let rec remove key value = function
    | [] -> raise Not_found 
    | (h,_)::_ when key<h -> raise Not_found 
    | (h,[v])::t when key=h && value=v -> t 
    | (h,list)::t when key=h -> (h,List_.usort_remove value list)::t
    | (h,list)::t (* when key>h *) -> (h,list) :: (remove key value t)

  let rec remove_key key = function
    | [] -> raise Not_found
    | (h,_)::_ when key<h -> raise Not_found 
    | (h,list)::t when key=h -> t
    | (h,list)::t (* when key>h *) -> (h,list) :: (remove_key key t)

  let rec mem key value = function
    | [] -> false
    | (h,_)::_ when key<h -> false
    | (h,list)::t when key=h -> List_.sort_mem value list
    | (h,list)::t (* when key>h *) -> mem key value t

  let rec mem_key key = function
    | [] -> false
    | (h,_)::_ when key<h -> false
    | (h,_)::t when key=h -> true
    | (h,_)::t (* when key>h *) -> mem_key key t

  exception Not_disjoint
  let disjoint_union t1 t2 = 
    let rec loop = function
      | [], t | t, [] -> t
      | ((h1,l1)::t1, (h2,l2)::t2) when h1 < h2 -> (h1,l1)::(loop (t1,((h2,l2)::t2)))
      | ((h1,l1)::t1, (h2,l2)::t2) when h1 > h2 -> (h2,l2)::(loop (((h1,l1)::t1),t2))
      | ((h1,l1)::t1, (h2,l2)::t2) (* when h1=h2*) ->
          try (h1,List_.sort_disjoint_union l1 l2)::(loop (t1, t2))
          with List_.Not_disjoint -> raise Not_disjoint
    in loop (t1, t2)

  exception Duplicate
  let merge_key i j t =
    try
      let i_list = List.assoc i t in
      disjoint_union (remove_key i t) [j,i_list]
    with 
    | Not_found -> (* no key i *) t
    | Not_disjoint -> raise Duplicate


  let exists fct t = List.exists (fun (key,list) -> List.exists (fun value -> fct key value) list) t
end

module Error = struct

  exception Build of (string * Loc.t option)
  exception Run of (string * Loc.t option)
  exception Bug of (string * Loc.t option)

  let build_ ?loc message = 
    Log.fmessage "[%s] %s" (match loc with None -> "?" | Some x -> Loc.to_string x) message;
    raise (Build (message, loc))
  let build ?loc = Printf.ksprintf (build_ ?loc)

  let run_ ?loc message = raise (Run (message, loc))
  let run ?loc = Printf.ksprintf (run_ ?loc)

  let bug_ ?loc message = raise (Bug (message, loc))
  let bug ?loc = Printf.ksprintf (bug_ ?loc)
end



module Id = struct
  type name = string

  type t = int

  type table = name array

  let build ?(loc:Loc.t option) string table =
    try Array_.dicho_find string table
    with Not_found -> Error.build ?loc "Identifier '%s' not found" string

  let build_opt string table = 
    try Some (Array_.dicho_find string table)
    with Not_found -> None
end

module Html = struct
  let css = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">" 
      
  let enter out_ch ?title ?header base_name =
    fprintf out_ch "<html>\n";
    (match title with
    | Some t -> fprintf out_ch "<head>\n%s\n<title>%s</title>\n</head>\n" css t
    | None -> fprintf out_ch "<head>\n%s\n</head>\n" css
    );
    fprintf out_ch "<body>\n";

    (match header with None -> () | Some s -> fprintf out_ch "%s\n" s);

    (match title with
    | Some t -> fprintf out_ch "<h1>%s</h1>\n" t 
    | None -> ()
    )
  let leave out_ch = 
    fprintf out_ch "</body>\n";
    fprintf out_ch "</html>\n";
end      

module Conll = struct
  type line = {
      num: int;
      phon: string;
      lemma: string;
      pos1: string;
      pos2: string;
      morph: (string * string) list;
      gov: int;
      dep_lab: string;
    }
        
  let parse_morph = function
    | "_" -> []
    | morph ->  
        List.map 
          (fun feat -> 
            match Str.split (Str.regexp "=") feat with
            | [feat_name] -> (feat_name, "true")
            | [feat_name; feat_value] -> (feat_name, feat_value)
            | _ -> Log.fcritical "Cannot not parse CONLL feat '%s' (too many '=')" morph
          ) (Str.split (Str.regexp "|") morph)
          
  let escape_quote s = Str.global_replace (Str.regexp "\"") "\\\"" s

  let parse line = 
    match Str.split (Str.regexp "\t") line with
    | [ num; phon; lemma; pos1; pos2; morph; gov; dep_lab; _; _ ] ->      
        {num = int_of_string num;
         phon = escape_quote phon;
         lemma = escape_quote lemma;
         pos1 = pos1;
         pos2 = pos2;
         morph = parse_morph morph;
         gov = int_of_string gov;
         dep_lab = dep_lab;
       }
    | _ -> Log.fcritical "Cannot not parse CONLL line '%s'" line
end

(* This module defiens a type for lexical parameter (i.e. one line in a lexical file) *)
module Lex_par = struct

  type item = string list * string list (* first list: pattern parameters $id , second list command parameters @id *)

  type t = item list

  let rm_peripheral_white s = 
    Str.global_replace (Str.regexp "\\( \\|\t\\)*$") ""
    (Str.global_replace (Str.regexp "^\\( \\|\t\\)*") "" s)

  let load ?loc dir nb_p nb_c file =
    try
      let full_file = 
        if Filename.is_relative file
        then Filename.concat dir file
        else file in
      
      let lines = File.read full_file in
      let param =
          (List.map
             (fun line ->
               let line = rm_peripheral_white line in
               match Str.split (Str.regexp "##") line with
               | [args] when nb_c = 0 ->
                   (match Str.split (Str.regexp "#") args with
                   | l when List.length l = nb_p -> (l,[])
                   | _ -> Error.bug 
                         "Illegal param line in file \"%s\", the line \"%s\" doesn't contain %d args"
                         full_file line nb_p)
               | [args; values] ->
                   (match (Str.split (Str.regexp "#") args, Str.split (Str.regexp "#") values) with
                   | (lp,lc) when List.length lp = nb_p && List.length lc = nb_c -> (lp,lc)
                   | _ -> Error.bug 
                         "Illegal param line in file \"%s\", the line \"%s\" doesn't contain %d args and %d values"
                         full_file line nb_p nb_c)
               | _ -> Error.bug "Illegal param line in file '%s' line '%s'" full_file line
             ) lines
          ) in
      param
    with Sys_error _ -> Error.build ?loc "External lexical file '%s' not found" file

  let filter index atom t = 
    (match List.filter (fun (x,_) -> List.nth x index = atom) t with
    | [] -> None
    | t' -> Some t'
    )

  let get_command_value index = function
    | [(_,one)] -> List.nth one index
    | [] -> Error.bug "[Lex_par.get_command_value] empty parameter"
    | (_,[sing])::tail when index=0 -> 
        Printf.sprintf "%s/%s" 
          sing 
          (List_.to_string (function (_,[s]) -> s | _ -> Error.bug "[Lex_par.get_command_value] inconsistent param") "/" tail)
    | l -> Error.run "Lexical parameter are not functionnal"

end

(* copy from leopar *)
module Timeout = struct
  exception Stop

  let counter = ref 0.
  let timeout = ref None
  
  let start () = counter := Unix.time ()

  let check () =
    match !timeout with 
    | None -> ()
    | Some delay ->
        if Unix.time () -. !counter > delay
        then raise Stop
end
