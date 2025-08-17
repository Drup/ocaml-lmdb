open Lmdb

let benchmark env repeat =
  let errors = ref 0 in

  let bench name conv_key conv_val key value n =
    let map = Map.(create Nodup ~key:conv_key ~value:conv_val) env ~name in
    let bench map cycles =
      let open Map in
      for i=0 to cycles-1 do
        add map (key i) (value i)
      done;
      for i=0 to cycles-1 do
        let v = get map (key i) in
        if (v <> value i)
        then incr errors;
      done;
      drop ~delete:false map;
    in
    name, bench map, n
  in

  let open Benchmark in
  let samples =
    let n = 500 in
    throughputN ~repeat 1
      [ bench "string"   Conv.string   Conv.string string_of_int string_of_int n
      ; bench "int32_be" Conv.int32_be Conv.string Int32.of_int string_of_int n
      ; bench "int32_le" Conv.int32_le Conv.string Int32.of_int string_of_int n
      ; bench "int64_be" Conv.int64_be Conv.string Int64.of_int string_of_int n
      ; bench "int64_le" Conv.int64_le Conv.string Int64.of_int string_of_int n
      ]
  in
  tabulate samples;
  !errors

let () =
  let env =
    Env.create Rw
      ~flags:Env.Flags.(no_subdir + no_sync + write_map + no_lock + no_mem_init)
      ~map_size:104857600
      ~max_maps:10
      "/tmp/lmdb_test.db"
  in
  let n =
    if Array.length Sys.argv = 2
    then int_of_string @@ Sys.argv.(1)
    else 1
  in
  assert (benchmark env n = 0);
  Sys.remove "/tmp/lmdb_test.db"
