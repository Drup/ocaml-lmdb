open Lmdb

let () =
  let errors = ref 0 in
  let env =
    Env.create
      ~flags:Env.Flags.(no_subdir + no_sync + write_map + no_lock + no_mem_init)
      ~map_size:104857600
      ~max_dbs:1
      "/tmp/lmdb_bench.db"
  in

  let map_hostint = IntDb.create ~create:true env "intmap" in

  let bench map cycles =
    let open IntDb in
    drop ~delete:false map;
    for i=0 to cycles-1 do
      put map i (string_of_int i)
    done;
    for i=0 to cycles-1 do
      let v = get map i in
      if (v <> string_of_int i)
      then incr errors;
    done
  in

  let open Benchmark in
  let samples =
    let n = 500 in
    throughputN ~repeat:5 1
      [ "IntDb", bench map_hostint, n ]
  in
  tabulate samples;
  if !errors > 0
  then begin
    Printf.eprintf "%i errors\n" !errors;
    exit 1;
  end
  else
    exit 0;
