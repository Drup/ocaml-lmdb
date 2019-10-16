open Alcotest
open Lmdb

let test env =
  "Problem reports",
  [ "#15", `Slow, begin fun () ->
        let t = Map.(create Nodup ~key:Conv.string ~value:Conv.string) env ~name:"pr#15" in
        (* Put some entries *)
        let rec put_count t = function
          | 0 -> ()
          | count ->
            let value_bytes = Bytes.make (10 * 8 * 1024) '1' in
            Map.put t (string_of_int count) (Bytes.to_string value_bytes) ;
            put_count t (count - 1)
        in
        let count = 250 in
        put_count t count ;
        assert ((Map.stat t).entries = count) ;
        (* Iterate using cursor and print keys *)
        ignore @@ Lmdb.Cursor.go Ro t (fun cur ->
            (* Triggering GC here also SEGFAULTs *)
            Gc.full_major () ;
            let rec print_keys = function
              | 0 -> ()
              | count ->
                let key, _ = Cursor.next cur in
                print_endline key ;
                print_keys (count - 1)
            in
            print_keys count );
      end
  ; "double txn_abort", `Quick, begin fun () ->
        (* mdb_txn_commit may return MDB_MAP_FULL.
         * In that case we did call mdb_txn_abort, which resulted in the
         * transaction being freed twice. *)
        let map = Map.(create Nodup ~key:Conv.int32_le_as_int ~value:Conv.string) env ~name:"double txn_abort" in
        check_raises "expecting MDB_MAP_FULL from txn_commit" Lmdb.(Error ~-30792) begin fun () ->
          for i=100 to max_int do
            Map.(put ~flags:Flags.append) map i "blub" (* Calls Txn.trivial *)
          done
        end;
        let map_size = Env.(info env).map_size in
        Env.set_map_size env (2 * map_size);
        check pass "resized map" () ();
        Map.put map 1 "blub";
        check pass "put successfull" () ();
        Map.drop ~delete:false map;
        check_raises "expecting MDB_MAP_FULL from Txn.go" (Error ~-30792) begin fun () ->
          ignore @@ Txn.go Rw env @@ fun txn ->
          let bulk = String.make 1024 '#' in
          for i=100 to max_int do
            Map.(put ~txn ~flags:Flags.append) map i bulk
          done
        end;
        Map.put map 1 "blub";
        check pass "put successfull" () ();
        Map.drop ~delete:false map;
        check_raises "expecting MDB_BAD_TXN from Txn.go" (Error ~-30782) begin fun () ->
          ignore @@ Txn.go Rw env @@ fun txn ->
          let bulk = String.make 1024 '#' in
          check_raises "expecting MDB_MAP_FULL from Map.put" (Error ~-30792) @@ fun () ->
          for i=100 to max_int do
            Map.(put ~txn ~flags:Flags.append) map i bulk
          done
        end;
        Map.put map 1 "blub";
        check pass "put successfull" () ();
      end
  ]
