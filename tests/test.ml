open Alcotest
open Lmdb

let () =
  let (s,major,minor,patch) = Lmdb.version in
  Printf.printf "Version: %s\nOr: (%i,%i,%i)\nOCaml Version: %s\n%!"
    s major minor patch Sys.ocaml_version

let filename =
  let rec tmp_filename base suffix n =
    let name = Printf.sprintf "%s.%u%s" base n suffix in
    if Sys.file_exists name;
    then tmp_filename base suffix (n+1)
    else name
  in
  tmp_filename "lmdb_test" ".db" 0

let env =
  Env.create Rw
    ~flags:Env.Flags.(no_subdir + no_sync + no_mem_init)
    ~map_size:524288
    ~max_maps:10
    filename
let () =
  at_exit @@ fun () ->
  Env.close env;
  Sys.remove filename;
  Sys.remove (filename ^ "-lock")

let[@warning "-26-27"] capabilities () =
  let map =
    Map.(create Nodup
           ~key:Conv.int32_be_as_int
           ~value:Conv.int32_be_as_int
           ~name:"Capabilities") env
  in
  let env_rw = env in
  let env_ro = env in
  (* let env_rw = (env_ro :> [ `Read | `Write ] Env.t) in <- FAILS *)
  (* ignore @@ (rw :> [ `Read ] Cap.t); <- FAILS *)
  (* ignore @@ (ro :> [ `Read | `Write ] cap); <- FAILS *)
  ignore @@ Txn.go Rw env_rw ?txn:None @@ fun txn_rw ->
  let txn_ro = (txn_rw :> [ `Read ] Txn.t) in
  Map.add ~txn:txn_rw map 4 4;
  (* Map.add ~txn:txn_ro map 4 4; <- FAILS *)
  assert (Map.get ~txn:txn_rw map 4 = 4);
  assert (Map.get ~txn:txn_ro map 4 = 4);
  Cursor.go Ro map
    ~txn:(txn_rw :> [ `Read ] Txn.t) @@ fun cursor ->
  assert (Cursor.get cursor 4 = 4);
  (* Cursor.first_dup cursor; <- FAILS *)
;;


let check_kv = check (pair int int)

let test_types =
  let map =
    Map.(create Nodup
           ~key:Conv.int32_be_as_int
           ~value:Conv.int32_be_as_int
           ~name:"Capabilities") env
  in
  "types",
  [ "value restriction", `Quick, begin fun () ->
        ignore @@ Txn.go Rw ?txn:None env @@ fun txn ->
        Map.stat ~txn map |> ignore;
        Map.add ~txn map 1 1;
      end
  ; "can read from writable", `Quick, begin fun () ->
      ignore @@ Txn.go Rw env
        (fun (txn : [> `Write] Txn.t) -> Map.stat ~txn map |> ignore);
    end
  ; "ro txn on rw env", `Quick, begin fun () ->
      Txn.go Ro env ignore |> ignore
    end
  ]

let unimap =
  Map.(create Nodup
         ~key:Conv.int32_be_as_int
         ~value:Conv.int32_be_as_int
         ~name:"Nodup") env
and dupmap =
  Map.(create Dup
         ~key:Conv.int32_be_as_int
         ~value:Conv.int32_be_as_int
         ~name:"Dup") env
and unimap_filled =
  Map.(create Nodup
         ~key:Conv.int32_be_as_int
         ~value:Conv.int32_be_as_int
         ~name:"NodupFilled") env
and dupmap_filled =
  Map.(create Dup
         ~key:Conv.int32_be_as_int
         ~value:Conv.int32_be_as_int
         ~name:"DupFilled") env

let () =
  let rec loop n =
    if n <= 536870912 then begin
      Map.(add unimap_filled ~flags:Flags.append_dup) n n;
      loop (n * 2);
    end
  in loop 12;

  let rec loop n =
    if n <= 536870912 then begin
      let rec loop_dup m =
        if m <= 536870912 then begin
          Map.(add dupmap_filled ~flags:Flags.append_dup) n m;
          loop_dup (m * 2);
        end
      in loop_dup n;
      loop (n * 2);
    end
  in loop 12

let test_map =
  "map",
  [ "add uni", `Quick, begin fun () ->
      (try Map.remove unimap 4285 with Not_found -> ());
      Map.add unimap 4285 42;
      Map.get unimap 4285 |> check int "check after add" 42;
      check_raises "key collision" Exists
        (fun () -> Map.add unimap 4285 42);
    end
  ; "add dup", `Quick, begin fun () ->
      (try Map.remove unimap 4285 with Not_found -> ());
      Map.add dupmap 4285 42;
      Map.add dupmap 4285 43;
      Map.get dupmap 4285 |> check int "check after add" 42;
      check_raises "key collision" Exists
        (fun () -> Map.(add ~flags:Flags.no_overwrite) dupmap 4285 45);
      check_raises "value collision" Exists
        (fun () -> Map.(add ~flags:Flags.no_dup_data) dupmap 4285 42);
    end
  ; "set uni", `Quick, begin fun () ->
      Map.set unimap 4285 1;
      Map.set unimap 4285 2;
      Map.get unimap 4285 |> check int "set overwrites" 2;
    end
  ; "set dup", `Quick, begin fun () ->
      Map.set dupmap 4285 1;
      Map.set dupmap 4285 2;
      Map.get dupmap 4285 |> check int "set overwrites" 2;
    end
  ; "get uni", `Quick, begin fun () ->
      Map.set unimap 4285 1;
      Map.get unimap 4285 |> check int "correct read" 1;
    end
  ; "get dup", `Quick, begin fun () ->
      Map.set dupmap 4285 1;
      Map.add dupmap 4285 2;
      Map.get dupmap 4285 |> check int "get returns first value" 1;
    end
  ; "remove", `Quick, begin fun () ->
      Map.set unimap 4285 1;
      Map.remove unimap 4285;
      check_raises "removed" Not_found (fun () -> Map.get unimap 4285 |> ignore)
    end
  ; "close", `Quick, begin fun () ->
      let map =
        Map.(create Nodup
             ~key:Conv.int32_be_as_int
             ~value:Conv.int32_be_as_int
             ~name:"close") env
      in
      Map.close map;
      check_raises "closed map" (Invalid_argument "Lmdb")
        (fun () -> Map.set map 0 0)
    end
  ]
;;

let test_cursor =
  "cursor",
  [ "fold_left uni", `Quick, begin fun () ->
      Cursor.fold_left 12 unimap_filled
        ~f:begin fun n key value ->
          check int "key" n key;
          check int "values" n value;
          (n * 2)
        end
      |> check int "last_key" 805306368
    end
  ; "fold_right uni", `Quick, begin fun () ->
      Cursor.fold_right unimap_filled 402653184
        ~f:begin fun key value n ->
          check int "key" n key;
          check int "values" n value;
          (n / 2)
        end
      |> check int "last_key" 6
    end
  ; "iter uni", `Quick, begin fun () ->
      let n = ref 12 in
      Cursor.iter unimap_filled
        ~f:begin fun key value ->
          check int "key" !n key;
          check int "values" !n value;
          n := value * 2;
        end;
      check int "last_kv" 805306368 !n
    end
  ; "fold_left dup", `Quick, begin fun () ->
      Cursor.fold_left (12, 12) dupmap_filled
        ~f:begin fun (n,m) key value ->
          check_kv "kv pair" (n,m) (key,value);
          if m*2 <= 536870912
          then (n, m * 2)
          else (n * 2, n * 2)
        end
      |> fst |> check int "last_key" 805306368
    end
  ; "fold_right dup", `Quick, begin fun () ->
      Cursor.fold_right dupmap_filled (402653184, 402653184)
        ~f:begin fun  key value (n,m) ->
          check_kv "kv pair" (n,m) (key,value);
          if m > n
          then (n, m / 2)
          else (n / 2, 402653184)
        end
      |> fst |> check int "last_key" 6
    end
  ; "iter dup", `Quick, begin fun () ->
      let kv = ref (12,12) in
      Cursor.iter dupmap_filled
        ~f:begin fun key value ->
          check_kv "kv pair" !kv (key,value);
          Printf.eprintf "%u %u\n" key value;
          if value*2 <= 536870912
          then kv := (key, value * 2)
          else kv := (key * 2, key * 2)
        end;
      check_kv "last_kv" (805306368,805306368) !kv
    end
  ; "fold_left_all", `Quick, begin fun () ->
      Cursor.fold_left_all 12 dupmap_filled
        ~f:begin fun n key values ->
          check int "key" n key;
          let rec loop_dup i m =
            if m <= 536870912 then begin
              check int "dup" m values.(i);
              loop_dup (i+1) (m * 2);
            end
            else check int "no extra dups" i (Array.length values)
          in loop_dup 0 key;
          (key * 2)
        end
      |> check int "last_key" 805306368
    end
  ; "fold_right_all", `Quick, begin fun () ->
      Cursor.fold_right_all dupmap_filled 402653184
        ~f:begin fun key values n ->
          check int "key" n key;
          let rec loop_dup i m =
            if m <= 536870912 then begin
              check int "dup" m values.(i);
              loop_dup (i+1) (m * 2);
            end
            else check int "no extra dups" i (Array.length values)
          in loop_dup 0 key;
          (key / 2)
        end
      |> check int "last_key" 6
    end
  ; "iter_all", `Quick, begin fun () ->
      let n = ref 12 in
      Cursor.iter_all dupmap_filled
        ~f:begin fun key values ->
          check int "key" !n key;
          let rec loop_dup i m =
            if m <= 536870912 then begin
              check int "dup" m values.(i);
              loop_dup (i+1) (m * 2);
            end
            else check int "no extra dups" i (Array.length values)
          in loop_dup 0 key;
          n := (key * 2)
        end;
      check int "last_key" 805306368 !n
    end
  ; "add uni", `Quick, begin fun () ->
      (try Map.remove unimap 4285 with Not_found -> ());
      Cursor.go Rw unimap @@ fun cursor ->
      Cursor.add cursor 4285 42;
      Cursor.get cursor 4285 |> check int "check after set" 42;
      check_raises "key collision" Exists
        (fun () -> Cursor.add cursor 4285 42);
    end
  ; "add dup", `Quick, begin fun () ->
      (try Map.remove dupmap 4285 with Not_found -> ());
      Cursor.go Rw dupmap @@ fun cursor ->
      Cursor.add cursor 4285 42;
      Cursor.add cursor 4285 43;
      Cursor.get cursor 4285 |> check int "check after add" 42;
      check_raises "key collision" Exists
        (fun () -> Cursor.(add ~flags:Flags.no_overwrite) cursor 4285 45);
      check_raises "value collision" Exists
        (fun () -> Cursor.(add ~flags:Flags.no_dup_data) cursor 4285 42);
    end
  ; "set uni", `Quick, begin fun () ->
      Cursor.go Rw unimap @@ fun cursor ->
      Cursor.set cursor 4285 1;
      Cursor.set cursor 4285 2;
      Cursor.get cursor 4285 |> check int "set overwrites" 2;
    end
  ; "set dup", `Quick, begin fun () ->
      Cursor.go Rw dupmap @@ fun cursor ->
      Cursor.set cursor 4285 1;
      Cursor.set cursor 4285 2;
      Cursor.get cursor 4285 |> check int "set overwrites" 2;
    end
  ; "get uni", `Quick, begin fun () ->
      Cursor.go Rw unimap @@ fun cursor ->
      Cursor.set cursor 4285 1;
      Cursor.get cursor 4285 |> check int "correct read" 1;
    end
  ; "get dup", `Quick, begin fun () ->
      Cursor.go Rw dupmap @@ fun cursor ->
      Cursor.set cursor 4285 1;
      Cursor.add cursor 4285 2;
      Cursor.get cursor 4285 |> check int "get returns first value" 1;
    end
  ; "remove", `Quick, begin fun () ->
      Cursor.go Rw unimap @@ fun cursor ->
      Cursor.set cursor 4285 1;
      Cursor.remove cursor;
      check_raises "removed" Not_found
        (fun () -> Cursor.get cursor 4285 |> ignore)
    end
  ; "walk uni", `Quick, begin fun () ->
      let open Cursor in
      go Rw unimap ?txn:None @@ fun cursor ->
      for i=0 to 9 do Cursor.add cursor i i done;
      first cursor              |> check_kv "first"                     (0,0);
      check_raises "walk before first" Not_found
        (fun () -> prev cursor |> ignore);
      next_nodup cursor         |> check_kv  "next_nodup"               (1,1);
      seek cursor 5             |> check_kv  "seek 5"                   (5,5);
      prev cursor               |> check_kv  "prev"                     (4,4);
      current cursor            |> check_kv  "current"                  (4,4);
      remove cursor;
      current cursor            |> check_kv  "shift after remove"       (5,5);
      next_nodup cursor         |> check_kv  "next_nodup"               (6,6);
      replace cursor 4; (* delete (6,6), add (6,4) *)
      current cursor            |> check_kv  "replace"                  (6,4);
      last cursor |> ignore;
      check_raises "walking beyond last key"                            Not_found
        (fun () -> next cursor |> ignore);
    end
  ; "walk dup", `Quick, begin fun () ->
      let open Cursor in
      go Rw dupmap ?txn:None @@ fun cursor ->
      for i=0 to 9 do add cursor i i done;
      for i=0 to 9 do add cursor 10 i done;
      prev cursor               |> check_kv  "prev"                     (10,8);
      next cursor               |> check_kv  "next"                     (10,9);
      first_dup cursor          |> check int "first_dup"                0;
      next_dup cursor           |> check int "next_dup"                 1;
      seek_dup cursor 10 5;
        current cursor          |> check_kv  "seek 5"                   (10,5);
      prev cursor               |> check_kv  "prev"                     (10,4);
      current cursor            |> check_kv  "current"                  (10,4);
      remove cursor;
      current cursor            |> check_kv  "cursor moved forward after remove" (10,5);
      first_dup cursor          |> check int "first"                    0;
      check_raises "fail when walking before first dup"                 Not_found
        (fun () -> prev_dup cursor |> ignore);
      last_dup cursor           |> check int "last"                     9;
      check_raises "fail when walking beyond last dup"                  Not_found
        (fun () -> next_dup cursor |> ignore);
      seek_dup cursor 10 7;
      current cursor            |> check_kv  "seek_dup"                 (10,7);
    end
  ; "first/last get first/last values", `Quick, begin fun () ->
      let open Cursor in
      go Rw dupmap ?txn:None @@ fun cursor ->
      set cursor ~flags:Flags.(none)       0 0;
      add cursor ~flags:Flags.(append_dup) 0 1;
      add cursor ~flags:Flags.(append_dup) 0 2;
      add cursor ~flags:Flags.(append + append_dup) 536870913 5;
      add cursor ~flags:Flags.(append_dup) 536870913 6;
      add cursor ~flags:Flags.(append_dup) 536870913 7;
      first cursor |> check_kv "first value" (0,0);
      last cursor |> check_kv "last value" (536870913,7);
    end
  ; "*_all", `Quick, begin fun () ->
      let open Cursor in
      go Rw dupmap_filled ?txn:None @@ fun cursor ->
      get_all cursor 100663296 |> check (array int) "get_all" [|100663296; 201326592; 402653184|];
      current cursor |> check_kv "cursor after get_all" (100663296, 402653184);
      last_all cursor |> snd |> check (array int) "last_all" [|402653184|];
      prev_all cursor |> snd |> check (array int) "prev_all" [|201326592; 402653184|];
      prev_nodup cursor |> ignore;
      next_all cursor |> snd |> check (array int) "next_all" [|201326592; 402653184|];
      prev_nodup cursor |> ignore;
      next_nodup cursor |> ignore;
      current cursor |> check_kv "cursor after prev_all" (201326592, 201326592;);
      current_all cursor |> snd |> check (array int) "current_all" [|201326592; 402653184|];
      current cursor |> check_kv "cursor after current_all" (201326592, 402653184);
      current cursor |> check_kv "cursor after next_all" (201326592, 402653184);
      first_all cursor |> snd |> check (array int) "first_all"
        [|12; 24; 48; 96; 192; 384; 768; 1536; 3072; 6144; 12288; 24576; 49152
         ; 98304; 196608; 393216; 786432; 1572864; 3145728; 6291456; 12582912
         ; 25165824; 50331648; 100663296; 201326592; 402653184|];
      current cursor |> check_kv "cursor after first_all" (12, 402653184);
    end
  ; "get_multiple", `Quick, begin fun () ->
      (try Map.remove unimap 0 with Not_found -> ());
      let open Cursor in
      go Rw dupmap ?txn:None @@ fun cursor ->
      seek cursor 0 |> ignore;
      remove cursor ~all:true;
      for i=0 to 65536 do
        add cursor ~flags:Flags.append_dup 0 i
      done;
      let values = get_all cursor 0 in
      for i=0 to 65536 do
        if i <> values.(i)
        then check int "order in many dups got with get_all" i values.(i)
      done;
    end
  ; "wrong map", `Quick,
    begin fun () ->
      let map2 =
        Map.(create Nodup
               ~key:Conv.int32_be_as_int
               ~value:Conv.int32_be_as_int
               ~name:"wrongmap") env
      in
      check_raises "wrong cursor" (Invalid_argument "Lmdb.Cursor.fold: Got cursor for wrong map") begin fun () ->
        Cursor.go Ro map2 @@ fun cursor ->
        Cursor.fold_left ~cursor () unimap ~f:(fun _ _ _ -> ());
      end;
    end
  ]
;;

let test_int =
  let make_test name conv =
    name, `Quick,
    begin fun () ->
      let map =
        Map.(create Dup
           ~key:conv
           ~value:conv
           ~name) env
      in
      let rec loop n =
        if n < 1073741823 then begin
          (try Map.(add ~flags:Flags.append     map n n)
           with Exists -> fail "Ordering on keys");
          (try Map.(add ~flags:Flags.append_dup map 1 n)
           with Exists -> fail "Ordering on values");
          loop (n / 3 * 4);
        end
      in loop 12;
      Map.drop ~delete:true map;
    end
  in
  "Int",
  [ make_test "int32_be" Conv.int32_be_as_int
  ; make_test "int32_le" Conv.int32_le_as_int
  ; make_test "int64_be" Conv.int64_be_as_int
  ; make_test "int64_le" Conv.int64_le_as_int
  ]

let test_stress =
  "threaded GC stress",
  let stress duration () =
    let map =
      Map.(create Nodup
             ~key:Conv.string
             ~value:Conv.string
             ~name:"map.string") env
    in
    let mutex = Mutex.create () in
    let errors = ref 0 in
    let running = ref true in
    let n = 100 in
    let rec worker thread_id =
      let offset = thread_id * n in
      for i = offset to offset + n - 1 do
        let dig = Digest.string @@ string_of_int i in
        Map.add map dig dig
      done;
      for i = offset to offset + n - 1 do
        let dig = Digest.string @@ string_of_int i in
        if Map.get map dig <> dig
        then begin
          Mutex.lock mutex;
          incr errors;
          Mutex.unlock mutex;
        end
        else Map.remove map dig
      done;
      if !running then worker thread_id
    in
    let rec stress_gc () =
      Gc.minor ();
      Thread.yield ();
      if !running then stress_gc ()
    in
    Gc.full_major ();
    Thread.create stress_gc () |> ignore;
    let threads = Array.init 8 (Thread.create worker) in
    Thread.delay duration;
    running := false;
    Array.iter Thread.join threads;
    check int "wrong reads" 0 !errors;
    Map.drop ~delete:true map
  in
  [ "stress 1s", `Quick, stress 1.
  (*; "stress 500s", `Slow, stress 500. too slow *)
  ]

let test_regress =
  "regression tests",
  [ "unnamed dbi", `Quick, begin fun () ->
        Map.(open_existing Nodup ~key:Conv.string ~value:Conv.string) env
        |> ignore
      end
  ; "dup unnamed dbi", `Quick, begin fun () ->
      check_raises "no duplicates on unnamed map"
        (Invalid_argument "Lmdb.Map.create: The unnamed map does not support duplicates")
      @@ fun () ->
      let unnamed_dup =
        Map.(open_existing Dup ~key:Conv.string ~value:Conv.string) env
      in
      check bool "compare dups" true (Map.compare_val unnamed_dup "5" "1" > 0);
      Cursor.go Rw unnamed_dup @@ fun cursor ->
      Cursor.add cursor "dup_entry" "1";
      Cursor.add cursor "dup_entry" "2";
      check (pair string (array string)) "dup entries" ("dup entry", [|"1";"2"|])
        (Cursor.current_all cursor);
    end
  ; "recycle map slots", `Quick, begin fun () ->
      for i=0 to 19 do
          Map.(create Nodup
             ~key:Conv.int32_be_as_int
             ~value:Conv.int32_be_as_int
             ~name:("recycle_" ^ string_of_int i)) env
          |> Map.close
      done;
      let rec exhaust ~txn maps i =
        match
          Map.(create Nodup ~txn
             ~key:Conv.int32_be_as_int
             ~value:Conv.int32_be_as_int)
             ~name:("recycle_" ^ string_of_int i)
             env;
        with
        | map -> exhaust ~txn (map :: maps) (i+1)
        | exception e ->
          check (testable Fmt.exn (=)) "max_maps exhausted"
            (Error ~-30791 (* MDB_DBS_FULL *)) e;
          maps
      in
      ignore @@ Txn.go Rw env begin fun txn ->
        let maps = exhaust ~txn [] 0 in
        let n = List.length maps in
        List.iter Map.close maps;
        let maps = exhaust ~txn [] n in
        List.iter Map.close maps
      end
    end
  ; "exhaust max_maps", `Quick, begin fun () ->
      let rec exhaust ~txn maps i =
        match
          Map.(create Nodup ~txn
             ~key:Conv.int32_be_as_int
             ~value:Conv.int32_be_as_int
             ~name:("exhaust_" ^ string_of_int i)) env;
        with
        | map -> exhaust ~txn (map :: maps) (i+1)
        | exception e ->
          (*
          List.iter Map.close maps;
             lmdb manual says:
               Do not close a handle if an existing transaction has modified
               its database.
             This is true for us here, too, since we just created the
             databases. Committing the transaction after closing the handles
             would fail with MDB_BAD_DBI, because it would either try to close
             or commit the map handles.
          *)
          check (testable Fmt.exn (=)) "max_maps exhausted"
            (Error ~-30791 (* MDB_DBS_FULL *)) e;
          maps
      in
      match Txn.go Rw env (fun txn -> exhaust ~txn [] 0) with
      | None -> ()
      | Some maps ->
          (*
            Without cleanup later map creations will obviously fail with
            MDB_DBS_FULL.
          *)
          match "close" with
          | "close" -> List.iter Map.close maps
          | "GC" -> Gc.full_major ()
          | _ -> ()
    end
  ; "abort txn with new map handles", `Quick, begin fun () ->
      let exception Maps of (int,int,[`Uni]) Map.t list in
      let rec exhaust ~txn maps i =
        match
          Map.(create Nodup ~txn
             ~key:Conv.int32_be_as_int
             ~value:Conv.int32_be_as_int
             ~name:("exhaust_" ^ string_of_int i)) env;
        with
        | map -> exhaust ~txn (map :: maps) (i+1)
        | exception e ->
          check (testable Fmt.exn (=)) "max_maps exhausted"
            (Error ~-30791 (* MDB_DBS_FULL *)) e;
          raise (Maps maps)
      in
      try ignore @@ Txn.go Rw env (fun txn -> exhaust ~txn [] 0)
      with Maps maps ->
      begin match "none" with
        | "close" -> List.iter Map.close maps
        | "GC" -> Gc.full_major ()
        | _ -> ()
      end;
      let map = Map.(create Nodup ~key:Conv.int32_le_as_int ~value:Conv.string) env ~name:"trigger_error" in
      ignore @@ Txn.go Rw env Txn.abort;
      Gc.full_major ();
      Map.drop ~delete:true map
    end
  ]

let test_txn =
  "transaction",
  let map = Map.(create Nodup ~key:Conv.int32_le_as_int ~value:Conv.string) env ~name:"double txn_abort" in
  [ "abort", `Quick, begin fun () ->
      ignore @@ Txn.go Rw env begin fun txn ->
        Map.add ~txn map 13 "blub";
        Txn.abort txn;
      end;
      check_raises "expecting Not_found" Not_found
        (fun () -> Map.get map 13 |> ignore);
    end
  ; "wrong envirronment", `Quick,
    begin fun () ->
      let env2 =
        Env.create Ro
          ~flags:Env.Flags.(no_subdir + no_sync + no_lock + no_mem_init)
          filename
      in
      check_raises "wrong txn" (Invalid_argument "Lmdb: transaction from wrong environment.") begin fun () ->
        ignore @@ Txn.go Ro env2
          (fun txn -> Map.get ~txn map 0 |> ignore);
      end;
      Env.close env2;
    end
  ; "MAP_FULL triggered by txn_commit", `Quick, begin fun () ->
        (* mdb_txn_commit may return MDB_MAP_FULL.
         * In that case we did call mdb_txn_abort, which resulted in the
         * transaction being freed twice. *)
        check_raises "expecting Map_full from txn_commit" Map_full begin fun () ->
          for i=100 to max_int do
            Map.(add ~flags:Flags.append) map i "blub" (* Calls Txn.trivial *)
          done
        end;
        let map_size = Env.(info env).map_size in
        Env.set_map_size env (2 * map_size);
        check pass "resized map" () ();
        Map.add map 1 "blub";
        check pass "add successfull" () ();
        Map.drop ~delete:false map;
    end
  ; "MAP_FULL triggered by Map.add", `Quick, begin fun () ->
        check_raises "expecting Map_full from Txn.go" Map_full begin fun () ->
          ignore @@ Txn.go Rw env @@ fun txn ->
          let bulk = String.make 1024 '#' in
          for i=100 to max_int do
            Map.(add ~txn ~flags:Flags.append) map i bulk
          done
        end;
        Map.add map 1 "blub";
        check pass "add successfull" () ();
        Map.drop ~delete:false map;
    end
  ; "MAP_FULL not passed on to Txn.go", `Quick, begin fun () ->
        check_raises "expecting MDB_BAD_TXN from Txn.go" (Error ~-30782) begin fun () ->
          ignore @@ Txn.go Rw env @@ fun txn ->
          let bulk = String.make 1024 '#' in
          check_raises "expecting Map_full from Map.add" Map_full @@ fun () ->
          for i=100 to max_int do
            Map.(add ~txn ~flags:Flags.append) map i bulk
          done
        end;
        Map.add map 1 "blub";
        check pass "add successfull" () ();
      end
  ]

let () =
  run "Lmdb"
    [ "capabilities", [ "capabilities", `Quick, capabilities ]
    ; test_types
    ; test_map
    ; test_cursor
    ; test_int
    ; test_regress
    ; test_txn
    ; test_stress
    ; Pr.test env
    ]
