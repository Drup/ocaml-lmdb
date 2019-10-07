module C = Configurator.V1

let () =
  C.main ~name:"foo" begin fun c ->
    let lmdb =
      match C.Pkg_config.get c with
    | None -> failwith "No pkg-config in $PATH"
    | Some pc -> match
          C.Pkg_config.query_expr_err pc
          ~package:"liblmdb"
          ~expr:"liblmdb"
      with
        | Error msg -> failwith ("liblmdb not found: " ^ msg)
        | Ok conf -> conf
    in

    C.Flags.write_sexp "cflags.sexp" lmdb.cflags;
    C.Flags.write_sexp "clibs.sexp" lmdb.libs
  end
