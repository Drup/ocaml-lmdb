# OCaml-lmdb [![Build Status](https://travis-ci.org/Drup/ocaml-lmdb.svg?branch=master)](https://travis-ci.org/Drup/ocaml-lmdb) [![docs](https://img.shields.io/badge/doc-online-blue.svg)][doc]

The [LMDB][] database is a fast in-file database that supports ACID transactions.

These bindings expose a typesafe yet low-overhead API. Both transactions and cursors are available. 
Database implementations are specialized both by keys and values.
Two module are predefined: `Lmdb.Db` (string keys and string values) and `Lmdb.IntDb` (int keys and string values). 
New implementation (which can use special LMDB features such as multi-values) can be added via a functorial interface.

Please consult the [documentation][doc] and a [simple example](tests/simple_db.ml).

```ocaml
let open Lmdb in
let env = Env.(create Rw ~flags:Flags.no_subdir ~max_maps:1) "mydb" in
let map = Map.create Nodup ~key:Conv.string ~value:Conv.string
	    ~name:"Camelidae" env in
Map.add map "Bactrian camel" "Elegant and beautiful animal with two humps."
```

[lmdb]: http://symas.com/mdb/#overview
[doc]: https://drup.github.io/ocaml-lmdb/dev/lmdb/Lmdb/index.html
