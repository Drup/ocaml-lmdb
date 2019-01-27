open Unsigned
open Ctypes

module Make (C : Cstubs.Types.TYPE) = struct
  open C

  type mdb_error = int
  let mdb_error : mdb_error typ = int

  let uint_as_int = view ~read:UInt.to_int ~write:UInt.of_int uint
  let size_t_as_int = view ~read:Size_t.to_int ~write:Size_t.of_int size_t

  type mdb_env = unit ptr
  let mdb_env : mdb_env typ = ptr (typedef void "MDB_env")

  type mdb_mode_t = PosixTypes.mode_t
  let mdb_mode_t = PosixTypes.mode_t

  let mdb_filehandle_t : Unix.file_descr typ =
    Unix_representations.
      (view int ~read:file_descr_of_int ~write:int_of_file_descr)

  (* typedef struct MDB_txn MDB_txn; *)
  type mdb_txn = unit ptr
  let mdb_txn : mdb_txn typ = ptr (typedef void "MDB_txn")
  let mdb_txn_opt : mdb_txn option typ = ptr_opt (typedef void "MDB_txn")

  (* typedef unsigned int MDB_dbi; *)
  type mdb_dbi = Unsigned.uint
  let mdb_dbi : mdb_dbi typ = uint


  type mdb_val
  let mdb_val : mdb_val structure typ = structure "MDB_val"
  let mv_size = field mdb_val "mv_size" size_t_as_int
  let mv_data = field mdb_val "mv_data" (ptr void)
  let () = seal mdb_val


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


  (* mdb_env Environment Flags *)
  type mdb_env_flag = uint
  let mdb_env_flag = uint
  let mdb_FIXEDMAP   = constant "MDB_FIXEDMAP"    uint
  let mdb_NOSUBDIR   = constant "MDB_NOSUBDIR"    uint
  let mdb_NOSYNC     = constant "MDB_NOSYNC"      uint
  let mdb_RDONLY     = constant "MDB_RDONLY"      uint
  let mdb_NOMETASYNC = constant "MDB_NOMETASYNC"  uint
  let mdb_WRITEMAP   = constant "MDB_WRITEMAP"    uint
  let mdb_MAPASYNC   = constant "MDB_MAPASYNC"    uint
  let mdb_NOTLS      = constant "MDB_NOTLS"       uint
  let mdb_NOLOCK     = constant "MDB_NOLOCK"      uint
  let mdb_NORDAHEAD  = constant "MDB_NORDAHEAD"   uint
  let mdb_NOMEMINIT  = constant "MDB_NOMEMINIT"   uint


  (* mdb_dbi_open	Database Flags *)
  type mdb_dbi_open_flag = uint
  let mdb_dbi_open_flag = uint
  let mdb_REVERSEKEY    = constant "MDB_REVERSEKEY"     uint
  let mdb_DUPSORT       = constant "MDB_DUPSORT"        uint
  let mdb_INTEGERKEY    = constant "MDB_INTEGERKEY"     uint
  let mdb_DUPFIXED      = constant "MDB_DUPFIXED"       uint
  let mdb_INTEGERDUP    = constant "MDB_INTEGERDUP"     uint
  let mdb_REVERSEDUP    = constant "MDB_REVERSEDUP"     uint
  let mdb_CREATE        = constant "MDB_CREATE"         uint


  (* mdb_put Write Flags *)
  type mdb_put_flag = Unsigned.uint
  let mdb_put_flag = uint
  let mdb_NOOVERWRITE = constant "MDB_NOOVERWRITE"  uint
  let mdb_NODUPDATA   = constant "MDB_NODUPDATA"    uint
  let mdb_CURRENT     = constant "MDB_CURRENT"      uint
  let mdb_RESERVE     = constant "MDB_RESERVE"      uint
  let mdb_APPEND      = constant "MDB_APPEND"       uint
  let mdb_APPENDDUP   = constant "MDB_APPENDDUP"    uint
  let mdb_MULTIPLE    = constant "MDB_MULTIPLE"     uint

  type mdb_copy_flag = Unsigned.uint
  let mdb_copy_flag = uint
  let mdb_CP_COMPACT  = constant "MDB_CP_COMPACT"   mdb_copy_flag

  (* typedef struct MDB_cursor MDB_cursor; *)
  type mdb_cursor = unit ptr
  let mdb_cursor : mdb_cursor typ = ptr (typedef void "MDB_cursor")

  type mdb_cursor_op =
    | MDB_FIRST
    | MDB_FIRST_DUP
    | MDB_GET_BOTH
    | MDB_GET_BOTH_RANGE
    | MDB_GET_CURRENT
    | MDB_GET_MULTIPLE
    | MDB_LAST
    | MDB_LAST_DUP
    | MDB_NEXT
    | MDB_NEXT_DUP
    | MDB_NEXT_MULTIPLE
    | MDB_NEXT_NODUP
    | MDB_PREV
    | MDB_PREV_DUP
    | MDB_PREV_NODUP
    | MDB_SET
    | MDB_SET_KEY
    | MDB_SET_RANGE
    | MDB_UNSUPORTED of int64

  let mdb_cursor_op = int64_t
  let mdb_FIRST          = constant "MDB_FIRST"          mdb_cursor_op
  let mdb_FIRST_DUP      = constant "MDB_FIRST_DUP"      mdb_cursor_op
  let mdb_GET_BOTH       = constant "MDB_GET_BOTH"       mdb_cursor_op
  let mdb_GET_BOTH_RANGE = constant "MDB_GET_BOTH_RANGE" mdb_cursor_op
  let mdb_GET_CURRENT    = constant "MDB_GET_CURRENT"    mdb_cursor_op
  let mdb_GET_MULTIPLE   = constant "MDB_GET_MULTIPLE"   mdb_cursor_op
  let mdb_LAST           = constant "MDB_LAST"           mdb_cursor_op
  let mdb_LAST_DUP       = constant "MDB_LAST_DUP"       mdb_cursor_op
  let mdb_NEXT           = constant "MDB_NEXT"           mdb_cursor_op
  let mdb_NEXT_DUP       = constant "MDB_NEXT_DUP"       mdb_cursor_op
  let mdb_NEXT_MULTIPLE  = constant "MDB_NEXT_MULTIPLE"  mdb_cursor_op
  let mdb_NEXT_NODUP     = constant "MDB_NEXT_NODUP"     mdb_cursor_op
  let mdb_PREV           = constant "MDB_PREV"           mdb_cursor_op
  let mdb_PREV_DUP       = constant "MDB_PREV_DUP"       mdb_cursor_op
  let mdb_PREV_NODUP     = constant "MDB_PREV_NODUP"     mdb_cursor_op
  let mdb_SET            = constant "MDB_SET"            mdb_cursor_op
  let mdb_SET_KEY        = constant "MDB_SET_KEY"        mdb_cursor_op
  let mdb_SET_RANGE      = constant "MDB_SET_RANGE"      mdb_cursor_op


  let mdb_cursor_op = enum "MDB_cursor_op" [
      MDB_FIRST         , mdb_FIRST            ;
      MDB_FIRST_DUP     , mdb_FIRST_DUP        ;
      MDB_GET_BOTH      , mdb_GET_BOTH         ;
      MDB_GET_BOTH_RANGE, mdb_GET_BOTH_RANGE   ;
      MDB_GET_CURRENT   , mdb_GET_CURRENT      ;
      MDB_GET_MULTIPLE  , mdb_GET_MULTIPLE     ;
      MDB_LAST          , mdb_LAST             ;
      MDB_LAST_DUP      , mdb_LAST_DUP         ;
      MDB_NEXT          , mdb_NEXT             ;
      MDB_NEXT_DUP      , mdb_NEXT_DUP         ;
      MDB_NEXT_MULTIPLE , mdb_NEXT_MULTIPLE    ;
      MDB_NEXT_NODUP    , mdb_NEXT_NODUP       ;
      MDB_PREV          , mdb_PREV             ;
      MDB_PREV_DUP      , mdb_PREV_DUP         ;
      MDB_PREV_NODUP    , mdb_PREV_NODUP       ;
      MDB_SET           , mdb_SET              ;
      MDB_SET_KEY       , mdb_SET_KEY          ;
      MDB_SET_RANGE     , mdb_SET_RANGE        ;
    ] ~unexpected:(fun i -> MDB_UNSUPORTED i)


end
