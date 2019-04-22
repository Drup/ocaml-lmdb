let test =
  "Problem reports",
  [ "#15", `Slow, begin fun () ->
        (* Create env and db *)
        let kibi = 8 * 1024 in
        let mibi = kibi * 1024 in
        let env = Lmdb.Env.(create Rw ~flags:Flags.(no_subdir + no_lock)
                              ~map_size:(10 * mibi) ~max_maps:1 "testenv") in
        let t = Lmdb.Map.(create Nodup ~key:Conv.string ~value:Conv.string) env ~name:"testdb" in
        (* Put some entries *)
        let rec put_count t = function
          | 0 -> ()
          | count ->
            let value_bytes = Bytes.make (10 * kibi) '1' in
            Lmdb.Map.put t (string_of_int count) (Bytes.to_string value_bytes) ;
            put_count t (count - 1)
        in
        let count = 250 in
        put_count t count ;
        assert ((Lmdb.Map.stats t).entries = count) ;
        (* Iterate using cursor and print keys *)
        ignore @@ Lmdb.Cursor.(go Ro) t (fun cur ->
            (* Triggering GC here also SEGFAULTs *) 
            Gc.full_major () ;
            let rec print_keys = function
              | 0 -> ()
              | count ->
                let key, _ = Lmdb.Cursor.next cur in
                print_endline key ;
                print_keys (count - 1)
            in
            print_keys count );
        Sys.remove "testenv";
      end
  ]
