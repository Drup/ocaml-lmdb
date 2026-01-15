(foreign_library
 (archive_name stubs)
 (language c)
 (names lmdb_stubs)
 (flags
   :standard
   -Wall -Wextra -Wno-unused-parameter
   (:include "../../config/cflags.sexp")
 )
)
