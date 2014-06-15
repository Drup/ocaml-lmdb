

let () =
  let (s,major,minor,patch) = Lmdb.version () in
  Printf.printf "Version: %s\nOr: (%i,%i,%i)\n%!" s major minor patch
  (* print_endline @@ Lmdb.version () *)
