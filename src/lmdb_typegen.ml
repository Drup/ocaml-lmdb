
let c_headers = {|
#include <lmdb.h>
|}

let () =
  let c_out = open_out "src/lmdb_typestubs.c" in
  let c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  Cstubs.Types.write_c c_fmt (module Lmdb_types.Make);
  Format.pp_print_flush c_fmt ();
