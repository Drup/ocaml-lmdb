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
  Env.create
    ~flags:Env.Flags.(no_subdir + no_sync + no_lock + no_mem_init)
    ~map_size:104857600
    ~max_dbs:10
    filename
let () =
  at_exit @@ fun () ->
  Sys.remove filename

module Map = Make (Values.Key.Int) (Values.Elt.Int)

let test_map =
  "Map",
  let open Map in
  let map =
    Map.(create env "Map")
  in
    [ "append(_dup)", `Quick, begin fun () ->
      Map.drop map;
      let rec loop n =
        if n <= 536870912 then begin
          let rec loop_dup m =
            if m <= 536870912 then begin
              put map n m;
              loop_dup (m * 2);
            end
          in loop_dup n;
          loop (n * 2);
        end
      in loop 12;
    end
  ; "put", `Quick,
    ( fun () -> put map 4285 42 )
  ; "put overwrite", `Quick,
    ( fun () -> put map 4285 2 )
  ; "put no_overwrite", `Quick, begin fun () ->
      check_raises "Exists" Exists  @@ fun () ->
      put map ~flags:PutFlags.no_overwrite 4285 0
    end
  ; "put no_dup_data", `Quick, begin fun () ->
      let map =
        Map.(create ~create:true env "Map.dup")
      in
      ignore @@ Txn.go ?txn:None rw env @@ fun txn ->
      put ~txn map ~flags:PutFlags.no_dup_data 4285 0;
      check_raises "Exists" Exists
        (fun () -> put ~txn map ~flags:PutFlags.no_dup_data 4285 0);
      Txn.abort txn
    end
  ; "get", `Quick,
    ( fun () -> check int "blub" 2 (get map 4285) )
  ; "remove", `Quick,
    ( fun () -> remove map 4285 )
  ; "Not_found", `Quick, begin fun () ->
      check_raises "Not_found" Not_found (fun () -> get map 4285 |> ignore)
    end
  ; "stress", `Slow, begin fun () ->
      let buf = String.make 1024 'X' in
      let n = 10000 in
      let map = IntDb.create env "map2" in
      for _i=1 to 100 do
        for i=0 to n do
          IntDb.put map i buf;
        done;
        for i=0 to n do
          let v =
            try
            IntDb.get map i
            with Not_found ->
              failwith ("got Not_found for " ^ string_of_int i)
          in
          if (v <> buf)
          then fail "memory corrupted ?"
        done;
        IntDb.drop ~delete:false map
      done;
    end
  ]

let test_cursor =
  "Cursor",
  let open Map.Cursor in
  let map = Map.(create env "Cursor") in
  let check_kv = check (pair int int) in
  [ "append(_dup)", `Quick,
    begin fun () ->
      Map.drop map;
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      let rec loop n =
        if n <= 536870912 then begin
          let rec loop_dup m =
            if m <= 536870912 then begin
              put cursor n m;
              loop_dup (m * 2);
            end
          in loop_dup n;
          loop (n * 2);
        end
      in loop 12;
    end
  ; "first", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      first cursor |> check_kv "first 12" (12, 12)
    end
  ; "put first", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      for i=0 to 9 do put cursor i i done
    end
  ; "walk", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      first cursor              |> check_kv "first" (0,0);
      check_raises "walk before first" Not_found
        (fun () -> prev cursor |> ignore);
      seek cursor 5             |> check int "seek 5"    5;
      prev cursor               |> check_kv  "prev"      (4,4);
      get cursor            |> check_kv  "current"   (4,4);
      get cursor            |> check_kv  "shift after remove" (5,5);
      get cursor            |> check_kv  "shift after put_here" (6,4);
      get cursor            |> check_kv  "replace added" (6,4);
      remove cursor ();
      seek_dup cursor 6         |> check int "seek_dup 6" 4;
      last cursor |> ignore;
      check_raises "walking beyond last key" Not_found
        (fun () -> next cursor |> ignore);
    end
  ; "walk dup", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      for i=0 to 9 do put cursor 10 i done;
      next cursor               |> check_kv  "next"      (12,12);
      prev cursor               |> check_kv  "prev"      (10,9);
      first_dup cursor          |> check_kv  "first_dup" (10,0);
      next_dup cursor           |> check_kv  "next_dup"  (10,1);
      seek_dup cursor 5         |> check int "seek_dup 5" 5;
      prev cursor               |> check_kv  "prev"      (10,4);
      get cursor            |> check_kv  "current"   (10,4);
      get cursor            |> check_kv  "cursor moved forward after remove" (10,5);
      remove cursor ();
      first_dup cursor          |> check_kv "first"      (10,0);
      check_raises "fail when walking before first dup" Not_found
        (fun () -> prev_dup cursor |> ignore);
      last_dup cursor           |> check_kv "last"       (10,9);
      check_raises "fail when walking beyond last dup" Not_found
        (fun () -> next_dup cursor |> ignore);
      get cursor            |> check_kv  "seek_dup"  (10,7);
      seek_dup cursor 7         |> check int "seek_dup"  7;
    end
  ; "put", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      put cursor 4285 42
    end
  ; "put no_overwrite", `Quick, begin fun () ->
      check_raises "failure when adding existing key" Exists @@ fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      put cursor ~flags:PutFlags.no_overwrite 4285 0
    end
  ; "put dup", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      put cursor 4285 42
    end
  ; "put dup no_dup_data", `Quick, begin fun () ->
      check_raises "failure when adding existing key-value" Exists @@ fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      put cursor ~flags:PutFlags.no_dup_data 4285 42
    end
  ; "get", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      check int "retrieve correct value for key" 42 (seek cursor 4285)
    end
  ; "Not_found", `Quick, begin fun () ->
      check_raises "failure on non-existing key" Not_found @@ fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      seek cursor 4287 |> ignore
    end
  ; "first gets first dup", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      put cursor 0 0;
      put cursor 0 1;
      put cursor 0 2;
      last cursor |> ignore;
      first cursor |> check_kv "first dup" (0,0)
    end
  ; "last gets last dup", `Quick, begin fun () ->
      ignore @@ go rw map ?txn:None @@ fun cursor ->
      put cursor 536870913 5;
      put cursor 536870913 6;
      put cursor 536870913 7;
      first cursor |> ignore;
      last cursor |> check_kv "last dup" (536870913,7)
    end
  ]

let test_int =
  let open Map in
  let make_test name =
    name, `Quick,
    begin fun () ->
      let map = (create env name) in
      let rec loop n =
        if n < 1073741823 then begin
          (try append map n n
           with Exists -> fail "Ordering on keys");
          (try append map 1 n
           with Exists -> fail "Ordering on values");
          loop (n / 3 * 4);
        end
      in loop 12;
      drop ~delete:true map;
    end
  in
  "Int",
  [ make_test "int"
  ]

let () =
  run "Lmdb"
    [ test_map
    ; test_cursor
    ; test_int
    ]
