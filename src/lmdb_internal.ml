open Ctypes
open Foreign
open Unsigned

(** Generic types *)

let bool = view ~read:((<>) 0) ~write:(fun b -> if b then 1 else 0) int

let uint_as_int = view ~read:UInt.to_int ~write:UInt.of_int uint
let size_t_as_int = view ~read:Size_t.to_int ~write:Size_t.of_int size_t

(** MDB types *)

(*
/** Unix permissions for creating files, or dummy definition for Windows */
#ifdef _MSC_VER
typedef	int	mdb_mode_t;
#else
typedef	mode_t	mdb_mode_t;
#endif
*)
type mdb_mode_t = PosixTypes.mode_t
let mdb_mode_t = PosixTypes.mode_t

(*
#ifdef _WIN32
typedef	void *mdb_filehandle_t;
#else
typedef int mdb_filehandle_t;
#endif
*)
(* Unix.file_descr = int is not exported. *)
let mdb_filehandle_t : Unix.file_descr typ =
  view ~read:Obj.magic ~write:Obj.magic int

(* typedef struct MDB_env MDB_env; *)
type mdb_env = unit ptr
let mdb_env : mdb_env typ = ptr void

(* typedef struct MDB_txn MDB_txn; *)
type mdb_txn = unit ptr
let mdb_txn : mdb_txn typ = ptr void
let mdb_txn_opt : mdb_txn option typ = ptr_opt void

(* typedef unsigned int MDB_dbi; *)
type mdb_dbi = Unsigned.uint
let mdb_dbi : mdb_dbi typ = uint

(* typedef struct MDB_cursor MDB_cursor; *)
type mdb_cursor = unit ptr
let mdb_cursor : mdb_cursor typ = ptr void

(*
typedef struct MDB_val {
	size_t		 mv_size;	/**< size of the data item */
	void		*mv_data;	/**< address of the data item */
} MDB_val;
*)
type mdb_val
let mdb_val : mdb_val structure typ = structure "MDB_val"
let mv_size = field mdb_val "mv_size" size_t
let mv_data = field mdb_val "mv_data" (ptr void)
let () = seal mdb_val


(* typedef int  (MDB_cmp_func)(const MDB_val *a, const MDB_val *b); *)
let mdb_cmp_func = ptr mdb_val @-> ptr mdb_val @-> returning int

(* typedef void (MDB_rel_func)(MDB_val *item, void *oldptr, void *newptr, void *relctx); *)
let mdb_rel_func = ptr mdb_val @-> ptr void @-> ptr void @-> ptr void @-> returning void

(* mdb_env Environment Flags *)
type mdb_env_flag = Unsigned.uint
let mdb_env_flag = uint
let mdb_FIXEDMAP   = 0x01
let mdb_NOSUBDIR   = 0x4000
let mdb_NOSYNC     = 0x10000
let mdb_RDONLY     = 0x20000
let mdb_NOMETASYNC = 0x40000
let mdb_WRITEMAP   = 0x80000
let mdb_MAPASYNC   = 0x100000
let mdb_NOTLS      = 0x200000
let mdb_NOLOCK     = 0x400000
let mdb_NORDAHEAD  = 0x800000
let mdb_NOMEMINIT  = 0x1000000

(* mdb_dbi_open	Database Flags *)
type mdb_dbi_open_flag = Unsigned.uint
let mdb_dbi_open_flag = uint
let mdb_REVERSEKEY    = 0x02
let mdb_DUPSORT       = 0x04
let mdb_INTEGERKEY    = 0x08
let mdb_DUPFIXED      = 0x10
let mdb_INTEGERDUP    = 0x20
let mdb_REVERSEDUP    = 0x40
let mdb_CREATE        = 0x40000

(* mdb_put Write Flags *)
type mdb_put_flag = Unsigned.uint
let mdb_put_flag = uint
let mdb_NOOVERWRITE = 0x10
let mdb_NODUPDATA   = 0x20
let mdb_CURRENT     = 0x40
let mdb_RESERVE     = 0x10000
let mdb_APPEND      = 0x20000
let mdb_APPENDDUP   = 0x40000
let mdb_MULTIPLE    = 0x80000

type mdb_stat
let mdb_stat : mdb_stat structure typ = structure "MDB_stat"
let ms_psize          = field mdb_stat "ms_psize" uint_as_int
let ms_depth          = field mdb_stat "ms_depth" uint_as_int
let ms_branch_pages   = field mdb_stat "ms_branch_pages" size_t_as_int
let ms_leaf_pages     = field mdb_stat "ms_leaf_pages" size_t_as_int
let ms_overflow_pages = field mdb_stat "ms_overflow_pages" size_t_as_int
let ms_entries        = field mdb_stat "ms_entries" size_t_as_int
let () = seal mdb_stat

type mdb_envinfo
let mdb_envinfo : mdb_envinfo structure typ = structure "MDB_envinfo"
let me_mapaddr    = field mdb_envinfo "me_mapaddr" (ptr void)
let me_mapsize    = field mdb_envinfo "me_mapsize" size_t
let me_last_pgno  = field mdb_envinfo "me_last_pgno" size_t
let me_last_txnid = field mdb_envinfo "me_last_txnid" size_t
let me_maxreaders = field mdb_envinfo "me_maxreaders" uint
let me_numreaders = field mdb_envinfo "me_numreaders" uint
let () = seal mdb_envinfo

type mdb_cursor_op = Unsigned.uint
let mdb_cursor_op = uint

(* Improve later *)
type mdb_error = unit
let mdb_error : mdb_error typ =
  let read i =
    if i = 0 then ()
    else failwith (Printf.sprintf "Ldmb Error %i" i)
  in
  view ~read ~write:(fun () -> 0) int

(** Helpers *)


let ret_error = returning mdb_error

(** {2 MDB functions} *)

(** {3 Misc functions} *)

(* char *mdb_version(int *major, int *minor, int *patch); *)
let mdb_version =
  foreign "mdb_version" (ptr int @-> ptr int @-> ptr int @-> returning string)

(* char *mdb_strerror(int err); *)
let mdb_strerror =
  foreign "mdb_strerror" (int @-> returning string)

(** {3 Env functions} *)

(* int  mdb_env_create(MDB_env **env); *)
let mdb_env_create =
  foreign "mdb_env_create" (ptr mdb_env @-> ret_error)

(* int mdb_env_open(MDB_env *env, const char *path, unsigned int flags, mdb_mode_t mode); *)
let mdb_env_open =
  foreign "mdb_env_open" (mdb_env @-> string @-> mdb_env_flag @-> mdb_mode_t @-> ret_error)

(* int mdb_env_copy(MDB_env *env, const char *path); *)
let mdb_env_copy =
  foreign "mdb_env_copy" (mdb_env @-> string @-> ret_error)

(* int mdb_env_copyfd(MDB_env *env, mdb_filehandle_t fd); *)
let mdb_env_copyfd =
  foreign "mdb_env_copyfd" (mdb_env @-> mdb_filehandle_t @-> ret_error)

(* int mdb_env_stat(MDB_env *env, MDB_stat *stat); *)
let mdb_env_stat =
  foreign "mdb_env_stat" (mdb_env @-> ptr mdb_stat @-> ret_error)

(* int mdb_env_info(MDB_env *env, MDB_envinfo *stat); *)
let mdb_env_info =
  foreign "mdb_env_info" (mdb_env @-> mdb_envinfo @-> ret_error)

(* int mdb_env_sync(MDB_env *env, int force); *)
let mdb_env_sync =
  foreign "mdb_env_sync" (mdb_env @-> bool @-> ret_error)

(* void mdb_env_close(MDB_env *env); *)
let mdb_env_close =
  foreign "mdb_env_close" (mdb_env @-> returning void)

(** {4 Env options} *)

(* int mdb_env_set_flags(MDB_env *env, unsigned int flags, int onoff); *)
let mdb_env_set_flags =
  foreign "mdb_env_set_flags" (mdb_env @-> mdb_env_flag @-> bool @-> ret_error)

(* int mdb_env_get_flags(MDB_env *env, unsigned int *flags); *)
let mdb_env_get_flags =
  foreign "mdb_env_get_flags" (mdb_env @-> ptr mdb_env_flag @-> ret_error)

(* int mdb_env_get_path(MDB_env *env, const char **path); *)
let mdb_env_get_path =
  foreign "mdb_env_get_path" (mdb_env @-> ptr string @-> ret_error)

(* int mdb_env_get_fd(MDB_env *env, mdb_filehandle_t *fd); *)
let mdb_env_get_fd =
  foreign "mdb_env_get_fd" (mdb_env @-> ptr mdb_filehandle_t @-> ret_error)

(* int mdb_env_set_mapsize(MDB_env *env, size_t size); *)
let mdb_env_set_mapsize =
  foreign "mdb_env_set_mapsize" (mdb_env @-> size_t_as_int @-> ret_error)

(* int mdb_env_set_maxreaders(MDB_env *env, unsigned int readers); *)
let mdb_env_set_maxreaders =
  foreign "mdb_env_set_maxreaders" (mdb_env @-> uint_as_int @-> ret_error)

(* int mdb_env_get_maxreaders(MDB_env *env, unsigned int *readers); *)
let mdb_env_get_maxreaders =
  foreign "mdb_env_get_maxreaders" (mdb_env @-> ptr uint_as_int @-> ret_error)

(* int mdb_env_set_maxdbs(MDB_env *env, MDB_dbi dbs); *)
let mdb_env_set_maxdbs =
  foreign "mdb_env_set_maxdbs" (mdb_env @-> uint_as_int @-> ret_error)

(* int mdb_env_get_maxkeysize(MDB_env *env); *)
let mdb_env_get_maxkeysize =
  foreign "mdb_env_get_maxkeysize" (mdb_env @-> returning int)

(* int mdb_env_set_userctx(MDB_env *env, void *ctx); *)
let mdb_env_set_userctx =
  foreign "mdb_env_set_userctx" (mdb_env @-> ptr void @-> ret_error)

(* void *mdb_env_get_userctx(MDB_env *env); *)
let mdb_env_get_userctx =
  foreign "mdb_env_get_userctx" (mdb_env @-> returning (ptr void))

(* typedef void MDB_assert_func(MDB_env *env, const char *msg); *)
let mdb_assert_func = mdb_env @-> string @-> returning void

(* int mdb_env_set_assert(MDB_env *env, MDB_assert_func *func); *)
let mdb_env_set_assert =
  foreign "mdb_env_set_assert" (mdb_env @-> funptr mdb_assert_func @-> ret_error)

(** {3 Transactions} *)

(* int mdb_txn_begin(MDB_env *env, MDB_txn *parent, unsigned int flags, MDB_txn **txn); *)
let mdb_txn_begin =
  foreign "mdb_txn_begin" (mdb_env @-> mdb_txn_opt @-> uint @-> ptr mdb_txn @-> ret_error)

(* MDB_env *mdb_txn_env(MDB_txn *txn); *)
let mdb_txn_env =
  foreign "mdb_txn_env" (mdb_txn @-> returning mdb_env)

(* int mdb_txn_commit(MDB_txn *txn); *)
let mdb_txn_commit =
  foreign "mdb_txn_commit" (mdb_txn @-> ret_error)

(* void mdb_txn_abort(MDB_txn *txn); *)
let mdb_txn_abort =
  foreign "mdb_txn_abort" (mdb_txn @-> returning void)

(* void mdb_txn_reset(MDB_txn *txn); *)
let mdb_txn_reset =
  foreign "mdb_txn_reset" (mdb_txn @-> returning void)

(* int mdb_txn_renew(MDB_txn *txn); *)
let mdb_txn_renew =
  foreign "mdb_txn_renew" (mdb_txn @-> ret_error)


(** {3 Databases} *)

(* int mdb_dbi_open(MDB_txn *txn, const char *name, unsigned int flags, MDB_dbi *dbi); *)
let mdb_dbi_open =
  foreign "mdb_dbi_open" (mdb_txn @-> string_opt @-> mdb_dbi_open_flag @-> ptr mdb_dbi @-> ret_error)

(* int mdb_stat(MDB_txn *txn, MDB_dbi dbi, MDB_stat *stat); *)
let mdb_dbi_stat =
  foreign "mdb_stat" (mdb_txn @-> mdb_dbi @-> ptr mdb_stat @-> ret_error)

(* int mdb_dbi_flags(MDB_txn *txn, MDB_dbi dbi, unsigned int *flags); *)
let mdb_dbi_flags =
  foreign "mdb_dbi_flags" (mdb_txn @-> mdb_dbi @-> ptr mdb_dbi_open_flag @-> ret_error)

(* void mdb_dbi_close(MDB_env *env, MDB_dbi dbi); *)
let mdb_dbi_close =
  foreign "mdb_dbi_close" (mdb_env @-> mdb_dbi @-> returning void)

(* int mdb_drop(MDB_txn *txn, MDB_dbi dbi, int del); *)
let mdb_drop =
  foreign "mdb_drop" (mdb_txn @-> mdb_dbi @-> bool @-> ret_error)

(** {4 Databases options} *)

(* int mdb_set_compare(MDB_txn *txn, MDB_dbi dbi, MDB_cmp_func *cmp); *)
let mdb_set_compare =
  foreign "mdb_set_compare" (mdb_txn @-> mdb_dbi @-> funptr mdb_cmp_func @-> ret_error)

(* int mdb_set_dupsort(MDB_txn *txn, MDB_dbi dbi, MDB_cmp_func *cmp); *)
let mdb_set_dupsort =
  foreign "mdb_set_dupsort" (mdb_txn @-> mdb_dbi @-> funptr mdb_cmp_func @-> ret_error)

(* int mdb_set_relfunc(MDB_txn *txn, MDB_dbi dbi, MDB_rel_func *rel); *)
let mdb_set_relfunc =
  foreign "mdb_set_relfunc" (mdb_txn @-> mdb_dbi @-> funptr mdb_rel_func @-> ret_error)

(* int mdb_set_relctx(MDB_txn *txn, MDB_dbi dbi, void *ctx); *)
let mdb_set_relctx =
  foreign "mdb_set_relctx" (mdb_txn @-> mdb_dbi @-> ptr void @-> ret_error)

(** {3 Items} *)

(* int mdb_get(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data); *)
let mdb_get =
  foreign "mdb_get" (mdb_txn @-> mdb_dbi @-> ptr mdb_val @-> ptr mdb_val @-> ret_error)

(* int mdb_put(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data, unsigned int flags); *)
let mdb_put =
  foreign "mdb_put" (mdb_txn @-> mdb_dbi @-> ptr mdb_val @-> ptr mdb_val @-> mdb_put_flag @-> ret_error)

(* int mdb_del(MDB_txn *txn, MDB_dbi dbi, MDB_val *key, MDB_val *data); *)
let mdb_del =
  foreign "mdb_del" (mdb_txn @-> mdb_dbi @-> ptr mdb_val @-> ptr mdb_val @-> ret_error)

(** {3 Cursor} *)

(* int mdb_cursor_open(MDB_txn *txn, MDB_dbi dbi, MDB_cursor **cursor); *)
let mdb_cursor_open =
  foreign "mdb_cursor_open" (mdb_txn @-> mdb_dbi @-> ptr mdb_cursor @-> ret_error)

(* void mdb_cursor_close(MDB_cursor *cursor); *)
let mdb_cursor_close =
  foreign "mdb_cursor_close" (mdb_cursor @-> returning void)

(* int mdb_cursor_renew(MDB_txn *txn, MDB_cursor *cursor); *)
let mdb_cursor_renew =
  foreign "mdb_cursor_renew" (mdb_txn @-> mdb_cursor @-> ret_error)

(* MDB_txn *mdb_cursor_txn(MDB_cursor *cursor); *)
let mdb_cursor_txn =
  foreign "mdb_cursor_txn" (mdb_cursor @-> returning mdb_txn)

(* MDB_dbi mdb_cursor_dbi(MDB_cursor *cursor); *)
let mdb_cursor_dbi =
  foreign "mdb_cursor_dbi" (mdb_cursor @-> returning mdb_dbi)

(* int mdb_cursor_get(MDB_cursor *cursor, MDB_val *key, MDB_val *data, MDB_cursor_op op); *)
let mdb_cursor_get =
  foreign "mdb_cursor_get" (mdb_cursor @-> ptr mdb_val @-> ptr mdb_val @-> mdb_cursor_op @-> ret_error)

(* int mdb_cursor_put(MDB_cursor *cursor, MDB_val *key, MDB_val *data, unsigned int flags); *)
let mdb_cursor_put =
  foreign "mdb_cursor_put" (mdb_cursor @-> ptr mdb_val @-> ptr mdb_val @-> mdb_put_flag @-> ret_error)

(* int mdb_cursor_del(MDB_cursor *cursor, unsigned int flags); *)
let mdb_cursor_del =
  foreign "mdb_cursor_del" (mdb_cursor @-> uint @-> ret_error)

(* int mdb_cursor_count(MDB_cursor *cursor, size_t *countp); *)
let mdb_cursor_count =
  foreign "mdb_cursor_count" (mdb_cursor @-> ptr size_t @-> ret_error)

(** {3 Comparisons} *)

(* int mdb_cmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b); *)
let mdb_cmp =
  foreign "mdb_cmp" (mdb_txn @-> mdb_dbi @-> ptr mdb_val @-> ptr mdb_val @-> ret_error)

(* int mdb_dcmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b); *)
let mdb_dcmp =
  foreign "mdb_dcmp" (mdb_txn @-> mdb_dbi @-> ptr mdb_val @-> ptr mdb_val @-> ret_error)

(** {3 Others} *)

(* typedef int (MDB_msg_func)(const char *msg, void *ctx); *)
let mdb_msg_func = string @-> ptr void @-> returning int

(* int mdb_reader_list(MDB_env *env, MDB_msg_func *func, void *ctx); *)
let mdb_reader_list =
  foreign "mdb_reader_list" (mdb_env @-> funptr mdb_msg_func @-> ptr void @-> ret_error)

(* int mdb_reader_check(MDB_env *env, int *dead); *)
let mdb_reader_check =
  foreign "mdb_reader_check" (mdb_env @-> ptr int @-> ret_error)
