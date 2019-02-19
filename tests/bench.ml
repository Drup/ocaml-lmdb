open Lmdb

let env =
  Env.create rw
    ~flags:Env.Flags.(no_subdir + no_sync + write_map + no_lock + no_mem_init)
    ~map_size:104857600
    ~max_dbs:10
    "/tmp/lmdb_test.db"

let benchmark repeat =
  let errors = ref 0 in

  let bench name conv_key conv_val key value n =
    let db = Db.(create ~key:conv_key ~value:conv_val) env ~name in
    let bench db cycles =
      let open Db in
      for i=0 to cycles-1 do
        put db (key i) (value i)
      done;
      for i=0 to cycles-1 do
        let v = get db (key i) in
        if (v <> value i)
        then incr errors;
      done;
      drop ~delete:false db;
    in
    name, bench db, n
  in

  let open Benchmark in
  let samples =
    let n = 500 in
    throughputN ~repeat 1
      [ bench "string"   Db.Conv.string   Db.Conv.string string_of_int string_of_int n
      ; bench "int32_be" Db.Conv.int32_be Db.Conv.string Int32.of_int string_of_int n
      ; bench "int32_le" Db.Conv.int32_le Db.Conv.string Int32.of_int string_of_int n
      ; bench "int64_be" Db.Conv.int64_be Db.Conv.string Int64.of_int string_of_int n
      ; bench "int64_le" Db.Conv.int64_le Db.Conv.string Int64.of_int string_of_int n
      ]
  in
  tabulate samples;
  !errors

let () =
  let n =
    if Array.length Sys.argv = 2
    then int_of_string @@ Sys.argv.(1)
    else 1
  in
  assert (benchmark n = 0)
