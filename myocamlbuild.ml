open Ocamlbuild_plugin

let flags () =
  List.iter
    (fun flag -> pflag ["ocaml"; "byte"; flag] "dllib"
        (fun param -> S [A "-dllib"; A param]))
    ["compile"; "link"];

  pflag ["ocaml"; "byte"; "link"] "dllpath"
    (fun param -> S [A "-dllpath"; A param]);

  pflag ["ocamlmklib"] "dllpath" (fun param -> S [A ("-dllpath"); A param])

let rules () =
  let conffile = !Options.build_dir / "_config" in

  let guess_config () =
    let get_env k =
      try
        Sys.getenv "CPATH"
        |> String.split_on_char ':'
      with Not_found -> []
    in
    let include_candidates =
      get_env "CPATH" @
      [ "/usr/include"
      ; "/usr/local/include"
      ; "/opt/include" ]
    and lib_candidates =
      get_env "LIBRARY_PATH" @
      [ "/lib"
      ; "/usr/lib"
      ; "/usr/local/lib"
      ; "/usr/lib/x86_64-linux-gnu"
      ; "/opt/lib" ]
    in
    let include_path =
      List.find_opt
        (fun path -> Sys.file_exists (path / "lmdb.h"))
        include_candidates
    and lib_path =
      List.find_opt
        (fun path ->
           Sys.file_exists (path / "liblmdb.a"))
        lib_candidates
    in
    Command.execute @@
    match include_path, lib_path with
    | Some include_path, Some lib_path ->
      Printf.eprintf "Found lmdb: %s/lmdb.h %s/liblmdb.a\n"
        include_path lib_path;
      Echo (
        [ "system_lmdb: true\n"
        ; "cflags: " ^ "-I" ^ include_path ^ "\n"
        ; "libs: "   ^ "-L" ^ lib_path; " -l" ^ "lmdb\n"
        ],
        conffile
      )
    | _, _ ->
      prerr_endline "Using shipped lmdb";
      Echo (
        [ "system_lmdb: false\n"
        ; "cflags: " ^ "-Isrc\n"
        ; "libs:\n"
        ],
        conffile
      )


  and pkgconf () =
    let pkgconf =
      let pkgconf = Command.search_in_path "pkgconf" in
      fun query ->
        Command.run_spec_and_read @@ S
          [ A pkgconf
          ; A ("--" ^ query)
          ; A "lmdb" ]
    in
    Command.execute @@
    Echo (
      [ "system_lmdb: true\n"
      ; "cflags: " ^ pkgconf "cflags"
      ; "libs: "   ^ pkgconf "libs"
      ],
      conffile
    )
  in

  let config =
    if not @@ Sys.file_exists conffile
    then if Sys.file_exists "_config"
      then
        Command.execute @@ ln_f "_config" conffile
      else begin
        try pkgconf () with
        | Not_found
        | Failure _ ->
          prerr_endline "pkg-config failed, guessing config";
          try guess_config () with Not_found ->
            failwith "No lmdb library found"
      end;

    let config =
      let read_file file =
        let ch = open_in file in
        let rec seq () =
          match input_line ch with
          | exception End_of_file -> close_in ch; Seq.Nil
          | line -> Seq.Cons (line, seq)
        in seq
      in
      read_file conffile
      |> Seq.map begin fun s ->
        Scanf.sscanf s "%s@: %s@!"
          (fun k v -> k,v)
      end
      |> List.of_seq
    in
    let tagify k v =
      String.split_on_char ' ' v
      |> List.filter ((<>) "")
      |> List.map (Printf.sprintf "%s(%s)" k)
    in
    tag_file "src/lmdb_stubs.c" @@
    tagify "ccopt" @@ List.assoc "cflags" config;
    let libs = List.assoc "libs" config in
    tag_file "src/lmdb.cma" @@ tagify "cclib" libs;
    tag_file "src/lmdb.cmxa" @@ tagify "cclib" libs;
    tag_file "src/liblmdb_stubs.a" @@ tagify "ldopt" libs;
    config
  in

  rule "copy sources from liblmdb"
    ~prod:"src/%(file:<*.[hc]>)"
    ~dep:"liblmdb/libraries/liblmdb/%(file)"
    begin fun env _build ->
      ln_f
        (env "liblmdb/libraries/liblmdb/%(file)")
        (env "src/%(file)")
    end;


  let system_lmdb = List.assoc "system_lmdb" config = "true" in
  rule ~insert:`top "lmdb C stubs"
    ~doc:"lmdb C stubs, possibly with lmdb backend included"
    ~deps:(if not system_lmdb then ["src/midl.h"; "src/lmdb.h"] else [])
    ~prod:"src/liblmdb_stubs.clib"
    begin fun _env _build ->
      let libs =
        if system_lmdb
        then
          [ "lmdb_stubs.o" ]
        else
          [ "lmdb_stubs.o"
          ; "mdb.o"
          ; "midl.o"
          ; "module.o" ]
      in
      Echo (List.map (fun f -> f ^ "\n") libs,
            "src/liblmdb_stubs.clib")
    end;

  begin
    let tags =
      [ "dllib(-llmdb_stubs)" ; "cclib(-llmdb_stubs)" ]
    in
    tag_file "src/lmdb.cma" tags;
    tag_file "src/lmdb.cmxa" tags;

    let tags = ["ccopt(-pthread)"] in
    tag_file "src/mdb.c" tags;
    tag_file "src/midl.c" tags;
    tag_file "src/module.c" tags;
  end;

  ocaml_lib ~dir:"src" "src/lmdb";
  dep ["ocaml"; "compile"; "use_lmdb"] ["src/lmdb.cmi"];
  dep ["ocaml"; "compile"; "native"; "use_lmdb"] ["src/lmdb.cmx"];
  dep ["ocaml"; "link"; "use_lmdb"] ["src/liblmdb_stubs.a"];
  dep ["ocaml"; "link"; "shared"] ["src/liblmdb_stubs.a"];

  rule "test lmdb"
    ~stamp:"test"
    ~dep:"tests/test.byte"
    (fun _env _build -> Cmd(S[ P "tests/test.byte" ]));

  let install_base =
    [ "META"
    ; "lmdb.mli"
    ; "lmdb_bindings.mli"
    ; "lmdb.cmi"
    ; "lmdb_bindings.cmi"
    ; "lmdb.cma"
    ; "liblmdb_stubs.a"
    ]
  and install_native =
    [ "lmdb.cmx"
    ; "lmdb.cmxa"
    ; "lmdb.a"
    ; "lmdb.cmxs"
    ]
  in

  rule "build lmdb"
    ~stamp:"build"
    ~deps:((install_base @ install_native) |> List.map ((/) "src"))
    (fun _ _ -> Nop);

  rule "install lmdb"
    ~doc:"use findlib to install the lmdb library"
    ~stamp:"install"
    ~deps:((install_base @ install_native) |> List.map ((/) "src"))
    begin fun env build ->
      let install_files =
        [ "META"
        ; "lmdb.cmt"
        ; "lmdb.cmti"
        ; "dlllmdb_stubs.so"
        ]
        @ install_base @ install_native
        |> List.map ((/) "src")
        |> List.filter Sys.file_exists
      in
      Seq(
        [ Cmd(S
          [ A "mkdir"
          ; P "_install"
          ])
        ; Cmd(S(
          [ V "OCAMLFIND"
          ; A "install"
          ; A "-patch-version"; A "1.1.3"
          ; A "lmdb"
          ] @ List.map (fun f -> P f) install_files
          ))
        ])
    end;
;;

let () =
  dispatch @@ function
  | Before_hygiene
  | After_hygiene -> ()
  | Before_options ->
    Options.use_ocamlfind := true;
    Options.make_links := false;
  | After_options
  | Before_rules -> flags ()
  | After_rules -> rules ()
