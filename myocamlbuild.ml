(* OASIS_START *)
(* OASIS_STOP *)
open Ocamlbuild_plugin;;


let dispatch = function
  | After_rules -> begin

      rule "cstubs: *_bindgen.ml -> *_generated.ml"
          ~deps:["%_bindgen.byte"]
          ~prods:["%_generated.ml";"%_cstubs.c"]
          (fun env _ ->
             let gen = env "%_bindgen.byte" in
             Cmd (S[ P gen ])
          ) ;

      rule "cstubs: *_typegen.ml -> *_typestubs.c"
          ~deps:["%_typegen.byte"]
          ~prods:["%_typestubs.c"]
          (fun env _ ->
             let gen = env "%_typegen.byte" in
             Cmd (S[ P gen ])
          ) ;

      rule "cstub: *_typestubs.c -> *_typestubs"
         ~deps:["%_typestubs.c"]
         ~prods:["%_typestubs"]
         (fun env _ ->
            let src = env "%_typestubs.c" in
            let bin = env "%_typestubs" in
            let oenv = BaseEnvLight.load () in
            let cc = BaseEnvLight.var_get "bytecomp_c_compiler" oenv in
            let stdlib = BaseEnvLight.var_get "standard_library" oenv in
            let ctypes = BaseEnvLight.var_get "pkg_ctypes_stubs" oenv in
            Cmd (S [Sh cc; A src;
                    A"-I"; P ctypes; A"-I"; P stdlib;
                    A"-o"; A bin])
         );

      rule "cstub: *_typestubs -> *_generated_types.ml"
          ~deps:["%_typestubs"]
          ~prods:["%_generated_types.ml"]
          (fun env _ ->
             let bin = env "%_typestubs" in
             let out = env "%_generated_types.ml" in
             Cmd (S [P bin; Sh">>"; A out])
          );

      (* fix oasis not passing ocamlfind packages to ocamlc when compiling c code *)
      flag ["c"; "compile";  "pkg_ctypes.stubs"]
        (S [ A "-package"; A "ctypes.stubs" ])
    end
  | _ -> ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
