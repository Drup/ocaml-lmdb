open Lmdb

let () =
  print_endline "get the env" ;
  let env = Env.(create ~flags:Flags.no_subdir ~max_dbs:1 "/tmp/foo.db") in

  print_endline "create the db" ;
  let db = Db.create ~create:true env "pouf" in

  print_endline "put the key in the db" ;
  Db.put db "foo" (read_line ()) ;

  print_endline "get the key from the db" ;
  let s = Db.get db "foo" in
  print_endline s ;
