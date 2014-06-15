open Ctypes
open PosixTypes
open Unsigned

module Internal = Lmdb_internal
open Internal

let alloc = allocate_n ~count:1

let opt_iter f = function
  | None -> ()
  | Some x -> f x

(** Stat type *)

type stats = {
  psize : int ;
  depth : int ;
  branch_pages : int ;
  leaf_pages : int ;
  overflow_pages : int ;
  entries : int ;
}
let make_stats stat = {
  psize = getf stat ms_psize ;
  depth = getf stat ms_depth ;
  branch_pages = getf stat ms_branch_pages ;
  leaf_pages = getf stat ms_leaf_pages ;
  overflow_pages = getf stat ms_overflow_pages ;
  entries = getf stat ms_entries ;
}


(** Value type *)

type 'a value = {
  write : 'a -> (mdb_val, [ `Struct ]) structured ;
  read : (mdb_val, [ `Struct ]) structured -> 'a ;
}


let read_int v =
  (* assert (sizeof int = Size_t.to_int (getf v mv_size)) ; *)
  from_voidp camlint (getf v mv_data)
let int_size = Size_t.of_int (sizeof camlint)
let write_int i =
  let v = make mdb_val in
  setf v mv_size int_size ;
  setf v mv_data (to_voidp i) ;
  v

let int_value = { write = write_int ; read = read_int }


let char_size = sizeof char
let read_string v =
  let size = Size_t.to_int (getf v mv_size) in
  let length = size / char_size in
  string_from_ptr ~length @@ from_voidp char @@ getf v mv_data
let write_string s =
  let v = make mdb_val in
  setf v mv_size @@ Size_t.of_int @@ String.length s * char_size ;
  setf v mv_data @@ to_voidp @@ allocate string s ; (* Can we do better ? *)
  v

let string_value = { write = write_string ; read = read_string }


(** {2 High level binding} *)

let version () =
  let major = alloc int in
  let minor = alloc int in
  let patch = alloc int in
  let s = mdb_version major minor patch in
  (s, !@major, !@minor, !@patch)

type env = mdb_env

type transaction = mdb_txn

module Env = struct
  type t = env

  exception Assert of (t * string)

  module Flags = struct
    type t = mdb_env_flag
    let i = Unsigned.UInt.of_int
    let (+) = Unsigned.UInt.logor
    let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
    let eq f f' = Unsigned.UInt.(compare f f' = 0)
    let fixedmap   = i mdb_FIXEDMAP
    let nosubdir   = i mdb_NOSUBDIR
    let nosync     = i mdb_NOSYNC
    let rdonly     = i mdb_RDONLY
    let nometasync = i mdb_NOMETASYNC
    let writemap   = i mdb_NOMETASYNC
    let mapasync   = i mdb_MAPASYNC
    let notls      = i mdb_NOTLS
    let nolock     = i mdb_NOLOCK
    let nordahead  = i mdb_NORDAHEAD
    let nomeminit  = i mdb_NOMEMINIT
  end

  let create ?maxreaders ?mapsize ?maxdbs ?(flags=Unsigned.UInt.zero) path mode =
    let env_ptr = alloc mdb_env in
    mdb_env_create env_ptr ;
    let env = !@env_ptr in
    opt_iter (mdb_env_set_mapsize env) mapsize ;
    opt_iter (mdb_env_set_maxdbs env) maxdbs ;
    opt_iter (mdb_env_set_maxreaders env) maxreaders ;
    mdb_env_set_assert env (fun env s -> raise (Assert (env,s))) ;
    mdb_env_open env path flags mode ;
    Gc.finalise mdb_env_close env ;
    env

  let copy = mdb_env_copy

  let copyfd env (fd : Unix.file_descr) = mdb_env_copyfd env (Obj.magic fd)

  let stats env =
    let stats = make mdb_stat in
    mdb_env_stat env (addr stats) ;
    make_stats stats

  let set_flags = mdb_env_set_flags
  let flags env =
    let flags = alloc mdb_env_flag in
    mdb_env_get_flags env flags ;
    !@flags

  let path env =
    let path = alloc string in
    mdb_env_get_path env path ;
    !@path

  let fd env =
    let fd = alloc mdb_filehandle_t in
    mdb_env_get_fd env fd ;
    !@fd

  let maxreaders env =
    let i = alloc int in
    mdb_env_get_maxreaders env i ;
    !@i

  let get_maxkeysize = mdb_env_get_maxkeysize

  let of_transaction = mdb_txn_env

  let reader_list env f =
    mdb_reader_list env (fun s _ -> f s; 0) null

  let readers env =
    let i = alloc int in
    mdb_reader_check env i ;
    !@i


end

let transaction ?parent ?(write=false) env f =
  let txn = alloc mdb_txn in
  let flag = if write then Env.Flags.rdonly else UInt.zero in
  mdb_txn_begin env parent flag txn ;
  let res = f !@txn in
  match res with
    | `Commit -> mdb_txn_commit !@txn
    | `Abort -> mdb_txn_abort !@txn

type db = mdb_dbi

module PutFlags = struct
  type t = mdb_put_flag
  let i = Unsigned.UInt.of_int
  let (+) = Unsigned.UInt.logor
  let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt.(compare f f' = 0)
  let nooverwrite = i mdb_NOOVERWRITE
  let nodupdata   = i mdb_NODUPDATA
  let current     = i mdb_CURRENT
  let reserve     = i mdb_RESERVE
  let append      = i mdb_APPEND
  let appenddup   = i mdb_APPENDDUP
  let multiple    = i mdb_MULTIPLE
end

module Db = struct

  module Flags = struct
    type t = mdb_env_flag
    let i = Unsigned.UInt.of_int
    let (+) = Unsigned.UInt.logor
    let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
    let eq f f' = Unsigned.UInt.(compare f f' = 0)
    let reversekey = i mdb_REVERSEKEY
    let dupsort    = i mdb_DUPSORT
    let integerkey = i mdb_INTEGERKEY
    let dupfixed   = i mdb_DUPFIXED
    let integerdup = i mdb_INTEGERDUP
    let reversedup = i mdb_REVERSEDUP
    let create     = i mdb_CREATE
  end

  let create ?txn ?name ?(flags=UInt.zero) env =
    let db = alloc mdb_dbi in
    begin match txn with
      | Some txn -> mdb_dbi_open txn name flags db
      | None ->
          transaction env (fun t -> mdb_dbi_open t name flags db ; `Commit)
    end ;
    (* We do not put a finaliser here, as it would break with mdb_drop. *)
    (* Gc.finalise mdb_dbi_close env !@db *)
    !@db

  let stats t db =
    let stats = make mdb_stat in
    mdb_dbi_stat t db (addr stats) ;
    make_stats stats

  let get_flags t db =
    let flags = alloc mdb_dbi_open_flag in
    mdb_dbi_flags t db flags ;
    !@flags

  let drop ?(delete=false) t db =
    mdb_drop t db delete

  let get t db k =
    let v = make mdb_val in
    mdb_get t db (addr k) (addr v) ;
    v

  let put ?(flags=UInt.zero) t db k v =
    mdb_put t db (addr k) (addr v) flags

  let del ?v t db k =
    match v with
      | Some v -> mdb_del t db (addr k) (addr v)
      | None ->  mdb_del t db (addr k) @@ from_voidp mdb_val null


end
