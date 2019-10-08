module C = Configurator.V1

let include_candidates =
  [ "/usr/include"
  ; "/usr/local/include"
  ; "/opt/include" ]
and lib_candidates =
  [ "/lib"
  ; "/usr/lib"
  ; "/usr/local/lib"
  ; "/opt/lib" ]


let () =
  C.main ~name:"foo" begin fun c ->
    let lmdb_pc =
      match C.Pkg_config.get c with
      | None -> None
      | Some pc ->
          C.Pkg_config.query pc
            ~package:"liblmdb"
    in
    let lmdb =
      match lmdb_pc with
      | Some lmdb -> lmdb
      | None ->
          let include_path =
            try
              List.find
                (fun path -> Sys.file_exists (path ^ "/lmdb.h"))
                include_candidates
            with Not_found -> failwith "lmdb.h not found"
          and lib_path =
            try
              List.find
                (fun path -> Sys.file_exists (path ^ "/liblmdb.a"))
                lib_candidates
            with Not_found -> failwith "liblmdb.a not found"
          in
          let open C.Pkg_config in
          { cflags = [ "-I" ^ include_path ]
          ; libs   = [ "-L" ^ lib_path; "-llmdb" ] }
    in

    C.Flags.write_sexp "cflags.sexp" lmdb.cflags;
    C.Flags.write_sexp "clibs.sexp" lmdb.libs
  end
