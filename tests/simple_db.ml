open Lmdb

let () =
  print_endline "get the env" ;
  let env = Env.(create rw ~flags:Flags.no_subdir "/tmp/foo.db") in

  print_endline "create the db" ;
  let db = Db.(create new_db) env in

  print_endline "put the key in the db" ;
  Db.put db "foo" (read_line ()) ;

  print_endline "get the key from the db" ;
  let s = Db.get db "foo" in
  print_endline s ;
