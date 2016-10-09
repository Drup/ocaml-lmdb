open Lmdb

let () =
  print_endline "get the env" ;
  let env = Env.(create ~flags:Flags.no_subdir "/tmp/foo.db") in

  print_endline "create the db" ;
  let db = Db.create ~create:true env in

  let s = Db.Txn.go ~rw:`Write db (fun t ->
    print_endline "put the key in the db" ;
    Db.Txn.put t "foo" (read_line ()) ;

    print_endline "get the key from the db" ;
    let s = Db.Txn.get t "foo" in
    print_endline s ;
    `Ok s)
  in
  match s with
    | Some s -> print_endline s
    | None -> print_endline (Db.get db "foo")
