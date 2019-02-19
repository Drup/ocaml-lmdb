open Lmdb

let () =
  print_endline "create an env" ;
  let env = Env.(create rw ~flags:Flags.no_subdir "/tmp/foo.db") in

  print_endline "create the map" ;
  let map =
    Map.(create ~key:Conv.string ~value:Conv.string env) in

  print_endline "put the key-value binding in the map" ;
  Map.put map "foo" (read_line ()) ;

  print_endline "get the value from the map" ;
  let s = Map.get map "foo" in
  print_endline s ;
