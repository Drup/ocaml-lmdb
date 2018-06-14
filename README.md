# OCaml-lmdb [![Build Status](https://travis-ci.org/Drup/ocaml-lmdb.svg?branch=master)](https://travis-ci.org/Drup/ocaml-lmdb) [![docs](https://img.shields.io/badge/doc-online-blue.svg)][doc]

The [LMDB][] database is a fast in-file database that supports ACID transactions.

These bindings expose a typesafe yet low-overhead API. Both transactions and cursors are available. 
Database implementations are specialized both by keys and values.
Two module are predefined: `Lmdb.Db` (string keys and string values) and `Lmdb.IntDb` (int keys and string values). 
New implementation (which can use special LMDB features such as multi-values) can be added via a functorial interface.

Please consult the [documentation][doc] and a [simple example](tests/simple_db.ml).

```ocaml
open Lmdb
let env = Env.create "mydb"
let db = Db.create ~create:true env "Camelidae" in
Db.put db "Bactrian camel" "Elegant and beautiful animal with two humps."
```

[lmdb]: http://symas.com/mdb/#overview
[doc]: https://drup.github.io/ocaml-lmdb/dev/Lmdb.html
