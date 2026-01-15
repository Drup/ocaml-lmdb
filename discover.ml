module C = Configurator.V1

let split_env var =
  try
    Str.(split (regexp_string ":")) (Sys.getenv var)
  with Not_found -> []
let include_candidates =
  split_env "CPATH" @
  [ "/usr/include"
  ; "/usr/local/include"
  ; "/opt/include" ]
and lib_candidates =
  split_env "LIBRARY_PATH" @
  [ "/lib"
  ; "/usr/lib"
  ; "/usr/local/lib"
  ; "/usr/lib/x86_64-linux-gnu/"
  ; "/opt/lib" ]


let () =
  C.main ~name:"foo" begin fun c ->
    let lmdb_pc =
      match C.Pkg_config.get c with
      | None -> None
      | Some pc ->
          C.Pkg_config.query pc
            ~package:"lmdb"
    and lmdb_guess =
      let include_path =
        List.find_opt
          (fun path -> Sys.file_exists (path ^ "/lmdb.h"))
          include_candidates
      and lib_path =
        List.find_opt
          (fun path ->
             Sys.file_exists (path ^ "/liblmdb.a"))
          lib_candidates
      in
      match include_path, lib_path with
      | Some include_path, Some lib_path ->
        let open C.Pkg_config in
        Some
          { cflags = [ "-I" ^ include_path ]
          ; libs   = [ "-L" ^ lib_path; "-llmdb" ] }
      | _ -> None
    in

    let copy src dst =
      let src = open_in_bin src
      and dst = open_out_bin dst in
      let buf = Bytes.create 4096 in
      while
        match input src buf 0 (Bytes.length buf) with
        | 0 -> false
        | l -> output dst buf 0 l; true
      do () done;
      close_in src;
      close_out dst;
    in

    match lmdb_pc, lmdb_guess with
    | Some lmdb as o, _
    | None, (Some lmdb as o) ->
      Printf.eprintf "Using system lmdb (%s)"
        (if o == lmdb_pc then "pkgconf" else "guessed");
      copy "../src/stubs_system.inc" "stubs.inc";
      C.Flags.write_sexp "cflags.sexp" lmdb.cflags;
      C.Flags.write_sexp "clibs.sexp" lmdb.libs;
    | _  ->
      prerr_endline "No lmdb found on system - falling back to shipped lmdb";
      copy "../src/stubs_shipped.inc" "stubs.inc";
      C.Flags.write_sexp "cflags.sexp" [];
      C.Flags.write_sexp "clibs.sexp" [];
  end
