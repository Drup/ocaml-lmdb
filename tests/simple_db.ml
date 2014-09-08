open Lmdb

module D = Make(Key.String)(Val.String)


let () =
  print_endline "get the env" ;
  let env = Env.(create ~flags:Flags.nosubdir "/tmp/foo.db") in

  print_endline "create the db" ;
  let db = D.create ~create:true env in

  let s = D.Txn.gow db (fun t ->
    print_endline "put the key in the db" ;
    D.Txn.put t "foo" (read_line ()) ;

    print_endline "get the key from the db" ;
    let s = D.Txn.get t "foo" in
    print_endline s ;
    `Ok s)
  in
  match s with
    | Some s -> print_endline s
    | None -> print_endline (D.get db "foo")
