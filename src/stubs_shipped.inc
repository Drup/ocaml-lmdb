(foreign_library
 (archive_name lmdb_stubs)
 (language c)
 (names lmdb_stubs mdb midl)
 (include_dirs ../../liblmdb/libraries/liblmdb/)
 (flags
   :standard
   -Wall -Wextra -Wno-unused-parameter
   (:include "../../config/cflags.sexp")
 )
)
