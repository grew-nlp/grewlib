open Ocamlbuild_plugin
 
let get_version () =
  let in_ch = open_in "VERSION" in
  let v = input_line in_ch in
  close_in in_ch;
  v

let () =
  dispatch begin function
  | After_rules ->
    let version = get_version () in
    let pp_src = S[A"-pp"; A("cppo -D 'VERSION "^version^"'")] in
    flag ["ocaml"; "ocamldep"] & pp_src;
    flag ["ocaml"; "compile"] & pp_src; 
  | _ -> ()
  end
