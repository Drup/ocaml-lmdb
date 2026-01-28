(foreign_library
 (archive_name lmdb_stubs)
 (language c)
 (names lmdb_stubs)
 (flags
   :standard
   -Wall -Wextra -Wno-unused-parameter
   (:include "../../config/cflags.sexp")
 )
)
