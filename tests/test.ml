open Alcotest
open Lmdb

let filename =
  let rec tmp_filename base suffix n =
    let name = Printf.sprintf "%s.%u%s" base n suffix in
    if Sys.file_exists name;
    then tmp_filename base suffix (n+1)
    else name
  in
  tmp_filename "/tmp/lmdb_test" ".db" 0

let env =
  Env.create rw
    ~flags:Env.Flags.(no_subdir + no_sync + no_lock + no_mem_init)
    ~map_size:104857600
    ~max_dbs:10
    filename

let () =
  at_exit @@ fun () ->
  Env.close env;
  Sys.remove filename

let map =
  Map.(create new_db
         ~conv_key:Conv.int32_be ~conv_val:Conv.int32_be
         ~flags:Flags.(dup_sort)
         ~name:"testmap" env)

let test_with_map f () =
  Map.drop ~delete:false map;
  try
    f map;
    Map.drop ~delete:false map;
  with
    e ->
    Map.drop ~delete:false map;
    raise e

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

let test_map =
  "Map",
  let open Map in
  [ "append", `Quick,
    begin fun () ->
      drop map;
      let rec loop n =
        if n < 1073741823 then begin
          append map n (lnot n);
          loop (n / 3 * 4);
        end
      in loop 12
    end
  ; "put", `Quick,
    ( fun () -> put map 4285 (lnot 4285) )
  ; "get", `Quick,
    ( fun () -> check int "blub" (lnot 4285) (get map 4285) )
  ; "Exists", `Quick,
    begin fun () ->
      check_raises "Exists" Exists  @@ fun () ->
      put map ~flags:PutFlags.no_overwrite 4285 0
    end
  ; "remove", `Quick,
    ( fun () -> remove map 4285 )
  ; "Not_found", `Quick,
    begin fun () ->
      check_raises "Not_found" Not_found (fun () -> get map 4285 |> ignore)
    end
  ; "stress", `Slow,
    begin fun () ->
      let buf = String.make 1024 'X' in
      let n = 10000 in
      let map =
        create new_db
          ~conv_key:Conv.int32_be ~conv_val:Conv.string
          ~name:"map2" env
      in
      for _i=1 to 100 do
        for i=0 to n do
          put map i buf;
        done;
        for i=0 to n do
          let v =
            try
            get map i
            with Not_found ->
              failwith ("got Not_found for " ^ string_of_int i)
          in
          if (v <> buf)
          then fail "memory corrupted ?"
        done;
        drop ~delete:false map
      done
    end
  ]

let test_cursor =
  "Cursor",
  let open Cursor in
  let check_kv = check (pair int int) in
  [ "append", `Quick,
    begin fun () ->
      Map.drop map;
      go rw map ?txn:None @@ fun cursor ->
      let rec loop n =
        if n < 1073741823 then begin
          append cursor n (lnot n);
          loop (n / 3 * 4);
        end
      in loop 12;
    end
  ; "first", `Quick,
    begin fun () ->
      go rw map ?txn:None @@ fun cursor ->
      first cursor |> check_kv "first 12" (12, (lnot 12))
    end
  ; "put first", `Quick,
    begin fun () ->
      go rw map ?txn:None @@ fun cursor ->
      for i=0 to 9 do put cursor i i done
    end
  ; "walk", `Quick,
    begin fun () ->
      go rw map ?txn:None @@ fun cursor ->
      first cursor              |> check_kv "first" (0,0);
      check_raises "walk before first" Not_found
        (fun () -> prev cursor |> ignore);
      next cursor               |> check_kv "next"      (1,1);
      seek cursor 5             |> check int "seek 5"   5;
      prev cursor               |> check_kv "prev"      (4,4);
      current cursor            |> check_kv "current"   (4,4);
      remove cursor;
      current cursor            |> check_kv "shift after remove" (5,5);
      next cursor               |> check_kv "next"      (6,6);
      (* XXX BUG in lmdb backend ? Behaviour wrongly documented ?
       * check_raises "Error" (Error 0)
        (fun () -> put_here cursor 400 4);*)
      (fun () -> put_here cursor 400 4) ();
      current cursor            |> check_kv "shift after put_here" (6,4);
      last cursor |> ignore;
      check_raises "fail when walking beyond last key" Not_found
        (fun () -> next cursor |> ignore);
    end
  ; "walk dup", `Quick,
    begin fun () ->
      go rw map ?txn:None @@ fun cursor ->
      for i=0 to 9 do put cursor 10 i done;
      first_dup cursor          |> check int "first"    0;
      next_dup cursor           |> check int "next"     1;
      seek_dup cursor 10 5      |> check_kv "seek 5"    (10,5);
      prev cursor               |> check_kv "prev"      (10,4);
      current cursor            |> check_kv "current"   (10,4);
      remove cursor;
      current cursor            |> check_kv "cursor moved forward after remove" (10,5);
      first_dup cursor           |> check int "first"   0;
      check_raises "fail when walking before first dup" Not_found
        (fun () -> prev_dup cursor |> ignore);
      last_dup cursor           |> check int "last"     9;
      check_raises "fail when walking beyond last dup" Not_found
        (fun () -> next_dup cursor |> ignore);
      seek_dup cursor 10 7      |> check_kv "seek_dup"  (10,7);
      (* XXX BUG in lmdb backend ? Behaviour wrongly documented ?
       * check_raises "Error" (Error 0)
        (fun () -> put_here cursor 10 4);*)
      (fun () -> put_here cursor 10 4) ();
      current cursor            |> check_kv "cursor moved forward after remove" (10,4);
    end
  ; "put", `Quick,
    begin fun () ->
      go rw map ?txn:None @@ fun cursor ->
      put cursor 4285 (lnot 4285)
    end
  ; "get", `Quick,
    begin fun () ->
      go rw map ?txn:None @@ fun cursor ->
      check int "retrieve correct value for key" (lnot 4285) (get cursor 4285)
    end
  ; "Exists", `Quick,
    begin fun () ->
      check_raises "failure when adding existing key" Exists  @@ fun () ->
      go rw map ?txn:None @@ fun cursor ->
      put cursor ~flags:PutFlags.no_overwrite 4285 0
    end
  ; "Not_found", `Quick,
    begin fun () ->
      check_raises "failure on non-existing key" Not_found  @@ fun () ->
      go rw map ?txn:None @@ fun cursor ->
      get cursor 4287 |> ignore
    end
  ]

let test_int =
  let open Map in
  let make_test name conv =
    name, `Quick,
    begin fun () ->
      let map =
        (create new_db
           ~conv_key:conv ~conv_val:conv
           ~flags:Flags.(dup_sort)
           ~name env)
      in
      let rec loop n =
        if n < 1073741823 then begin
          (try append map n n with Exists -> fail "Ordering on keys");
          (try append map 1 n with Exists -> fail "Ordering on values");
          loop (n / 3 * 4);
        end
      in loop 12;
      drop ~delete:true map;
    end
  in
  "Int",
  [ make_test "int32_be" Conv.int32_be
  ; make_test "int32_le" Conv.int32_le
  ; make_test "int64_be" Conv.int64_be
  ; make_test "int64_le" Conv.int64_le
  ]

let () =
  run "Lmdb"
    [ "capabilities", [ "capabilities", `Quick, capabilities ]
    ; test_map
    ; test_cursor
    ; test_int
    ]
