
let c_headers = {|
#include <lmdb.h>
|}

let () =
  let ml_out = open_out "src/lmdb_generated.ml"
  and c_out = open_out "src/lmdb_cstubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out
  and c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  Cstubs.write_c c_fmt ~prefix:"lmdb_stub_" (module Lmdb_bindings.Make);
  Cstubs.write_ml ml_fmt ~prefix:"lmdb_stub_" (module Lmdb_bindings.Make);
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out
