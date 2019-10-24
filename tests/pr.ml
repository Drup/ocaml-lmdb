open Lmdb

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
  ]
