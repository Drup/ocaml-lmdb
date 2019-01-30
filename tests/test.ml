open Lmdb

let env =
  Env.create rw
    ~flags:Env.Flags.(no_subdir + no_sync + no_lock + no_mem_init)
    ~map_size:104857600
    ~max_dbs:10
    "/tmp/lmdb_test.db"

let benchmark repeat =
  let errors = ref 0 in

  let bench_functor
      (type key elt)
      name
      (module Db : S with type key = key and type elt = elt) key value n =
    let map_host = Db.(create new_db) env ~name in
    let bench map cycles =
      let open Db in
      for i=0 to cycles-1 do
        put map (key i) (value i)
      done;
      for i=0 to cycles-1 do
        let v = get map (key i) in
        if (v <> value i)
        then incr errors;
      done;
      drop ~delete:false map;
    in
    name, bench map_host, n
  in

  let bench_poly name conv_key conv_val key value n =
    let map_host = Map.(create new_db ~conv_key ~conv_val) env ~name in
    let bench map cycles =
      let open Map in
      for i=0 to cycles-1 do
        put map (key i) (value i)
      done;
      for i=0 to cycles-1 do
        let v = get map (key i) in
        if (v <> value i)
        then incr errors;
      done;
      drop ~delete:false map;
    in
    name, bench map_host, n
  in

  let open Benchmark in
  let samples =
    let n = 500 in
    throughputN ~repeat 1
      [ bench_functor "functor int" (module IntDb) (fun i -> i) string_of_int n
      ; bench_functor "functor string" (module Db) string_of_int string_of_int n
      ; bench_poly "poly int" Map.Conv.int Map.Conv.string (fun i -> i) string_of_int n
      ; bench_poly "poly string" Map.Conv.string Map.Conv.string string_of_int string_of_int n
      ; bench_poly "poly int32_be" Map.Conv.int32_be Map.Conv.string (fun i -> i) string_of_int n
      ; bench_poly "poly int32_le" Map.Conv.int32_le Map.Conv.string (fun i -> i) string_of_int n
      ; bench_poly "poly int64_be" Map.Conv.int64_be Map.Conv.string (fun i -> i) string_of_int n
      ; bench_poly "poly int64_le" Map.Conv.int64_le Map.Conv.string (fun i -> i) string_of_int n
      ]
  in
  tabulate samples;
  !errors

let[@warning "-26-27"] capabilities () =
  let env_rw = (env :> [ `Read | `Write ] Env.t) in
  let env_ro = (env :> [ `Read ] Env.t) in
  (* let env_rw = (env_ro :> [ `Read | `Write ] Env.t) in <- FAILS *)
  (* ignore @@ (rw :> [ `Read ] cap); <- FAILS *)
  (* ignore @@ (ro :> [ `Read | `Write ] cap); <- FAILS *)
  ignore @@ Txn.go rw env_rw ?txn:None @@ fun txn_rw ->
  let txn_ro = (txn_rw :> [ `Read ] Txn.t) in
  let dbi = Db.(create new_db) ~txn:txn_rw env ~name:"test" in
  Db.put ~txn:txn_rw dbi "4" "IV";
  (* Db.put ~txn:txn_ro dbi "4" "IV"; <- FAILS*)
  assert (Db.get ~txn:txn_rw dbi "4" = "IV");
  assert (Db.get ~txn:txn_ro dbi "4" = "IV");
  Db.Cursor.go ro
    ~txn:(txn_rw :> [ `Read ] Txn.t)
    (dbi :> [ `Read ] Db.t) @@ fun cursor ->
  assert (Db.Cursor.seek cursor "4" = "IV");
;;

let () =
  let open Alcotest in
  let benchmark () = check int "error count" 0 @@ benchmark 5 in
  try
  run "Lmdb"
    [ "capabilities", [ "capabilities", `Quick, capabilities ]
    ; "Benchmark", [ "Benchmark", `Slow, benchmark ]
    ];
  with
  | e -> Env.close env; raise e
