open Lmdb
open Alcotest

let test env =
  "Problem reports",
  [ "#15", `Slow, begin fun () ->
        Env.set_map_size env 104857600;
        let t = Map.(create Nodup ~key:Conv.string ~value:Conv.string) env ~name:"pr#15" in
        (* Put some entries *)
        let rec put_count t = function
          | 0 -> ()
          | count ->
            let value_bytes = Bytes.make (10 * 8 * 1024) '1' in
            Map.add t (string_of_int count) (Bytes.to_string value_bytes) ;
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
