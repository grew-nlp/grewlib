open Ocamlbuild_plugin
 
let get_version () =
  let in_ch = open_in "VERSION" in
  let v = input_line in_ch in
  close_in in_ch;
  v

let () =
  dispatch begin function
  | After_rules ->
  	let v = get_version () in
    let pp_src = S[A"-pp"; A("camlp4o pa_macro.cmo -DVERSION=\\\""^v^"\\\" -DDEP2PICT")] in
    flag ["ocaml"; "ocamldep"] & pp_src;
    flag ["ocaml"; "compile"] & pp_src; 
  | _ -> ()
  end
