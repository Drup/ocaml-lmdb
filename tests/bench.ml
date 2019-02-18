open Lmdb

let errors = ref 0
let env =
  Env.create rw
    ~flags:Env.Flags.(no_subdir + no_sync + write_map + no_lock + no_mem_init)
    ~map_size:104857600
    ~max_dbs:2
    "/tmp/lmdb_bench.db"

let bench
    (type key elt)
    (module Db : S with type key = key and type elt = elt) name key value n =
  let map_host = Db.create ~create:true env name in
  let bench map cycles =
    let open Db in
    drop ~delete:false map;
    for i=0 to cycles-1 do
      put map (key i) (value i)
    done;
    for i=0 to cycles-1 do
      let v = get map (key i) in
      if (v <> value i)
      then incr errors;
    done
  in
  name, bench map_host, n

let () =   
  let open Benchmark in
  let samples =
    let n = 500 in
    throughputN ~repeat:5 1 [
      bench (module IntDb) "int" (fun i -> i) string_of_int n ;
      bench (module Db) "string" string_of_int string_of_int n ;
    ]
  in
  tabulate samples;
  if !errors > 0
  then begin
    Printf.eprintf "%i errors\n" !errors;
    exit 1;
  end
  else
    exit 0;
