open Ctypes
open PosixTypes
open Unsigned

module Internal = Lmdb_internal
open Internal

let alloc = allocate_n ~count:1

let opt_iter f = function
  | None -> ()
  | Some x -> f x

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

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


(** {2 High level binding} *)

let version () =
  let major = alloc int in
  let minor = alloc int in
  let patch = alloc int in
  let s = mdb_version major minor patch in
  (s, !@major, !@minor, !@patch)

type env = mdb_env

module Env = struct

  exception Assert of (env * string)

  module Flags = struct
    type t = mdb_env_flag
    let i = Unsigned.UInt.of_int
    let (+) = Unsigned.UInt.logor
    let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
    let eq f f' = Unsigned.UInt.(compare f f' = 0)
    let none = Unsigned.UInt.zero
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

  let create ?maxreaders ?mapsize ?maxdbs ?(flags=Flags.none) ?(mode=0o755) path =
    let mode = coerce uint32_t mode_t UInt32.(of_int mode) in
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

  let max_readers env =
    let i = alloc int in
    mdb_env_get_maxreaders env i ;
    !@i

  let max_keysize = mdb_env_get_maxkeysize

  let reader_list env =
    let x = ref [] in
    mdb_reader_list env (fun s _ -> x:=  s::!x ; 0) null ;
    !x

  let readers env =
    let i = alloc int in
    mdb_reader_check env i ;
    !@i

end

(* Use internally for trivial functions *)
let trivial_txn ~write env f =
  let txn = alloc mdb_txn in
  let txn_flag = if write
    then Env.Flags.none
    else Env.Flags.rdonly
  in
  mdb_txn_begin env None txn_flag txn ;
  (f !@txn : unit) ;
  mdb_txn_commit !@txn


module PutFlags = struct
  type t = mdb_put_flag
  let i = Unsigned.UInt.of_int
  let (+) = Unsigned.UInt.logor
  let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt.(compare f f' = 0)
  let none = Unsigned.UInt.zero
  let nooverwrite = i mdb_NOOVERWRITE
  let nodupdata   = i mdb_NODUPDATA
  let current     = i mdb_CURRENT
  let reserve     = i mdb_RESERVE
  let append      = i mdb_APPEND
  let appenddup   = i mdb_APPENDDUP
  let multiple    = i mdb_MULTIPLE
end

module Flags = struct
  type t = mdb_env_flag
  let i = Unsigned.UInt.of_int
  let (+) = Unsigned.UInt.logor
  let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt.(compare f f' = 0)
  let none = Unsigned.UInt.zero
  let reversekey = i mdb_REVERSEKEY
  let dupsort    = i mdb_DUPSORT
  let dupfixed   = i mdb_DUPFIXED
  let integerdup = i mdb_INTEGERDUP
  let reversedup = i mdb_REVERSEDUP

  (* Not exported *)
  let integerkey = i mdb_INTEGERKEY
  let create     = i mdb_CREATE
end


type db_val = (mdb_val, [ `Struct ]) structured ptr

module Val = struct

  module type S = sig
    type t
    val default_flags : Flags.t
    val read : db_val -> t
    val write : t -> db_val
  end

  module Int : S with type t = int = struct
    type t = int
    let default_flags = Flags.integerkey
    let int_size = Size_t.of_int (sizeof camlint)
    let write i =
      let v = make mdb_val in
      setf v mv_size int_size ;
      setf v mv_data (to_voidp @@ allocate camlint i) ;
      addr v
    let read v =
      !@(from_voidp camlint @@ getf !@v mv_data)
  end

  module String : S with type t = string = struct
    type t = string
    let default_flags = Flags.none

    let array_of_string s =
      let l = String.length s in
      let a = CArray.make char l in
      for i = 0 to l - 1 do
        CArray.set a i s.[i]
      done ; a

    let read v =
      let length = Size_t.to_int (getf !@v mv_size) in
      string_from_ptr ~length @@ from_voidp char @@ getf !@v mv_data
    let write s =
      let v = make mdb_val in
      let a = array_of_string s in
      setf v mv_size @@ Size_t.of_int @@ CArray.length a ;
      setf v mv_data @@ to_voidp @@ CArray.start a ;
      addr v
  end


end


module Key = struct

  module type S = Val.S

  module Int : S with type t = int = struct
    include Val.Int
    let default_flags = Flags.integerkey
  end

  module String : S with type t = string = Val.String

end



exception Abort of mdb_txn

module Make (Key : Key.S) (Val : Val.S) = struct

  let def_flags = Flags.(Key.default_flags + Val.default_flags)

  type db = {env : env ; db : mdb_dbi }

  let create ?(create=true) ?name ?(flags=Flags.none) env =
    let db = alloc mdb_dbi in
    let flags =
      if create
      then Flags.(flags + create + def_flags)
      else Flags.(flags + def_flags)
    in
    let f txn = mdb_dbi_open txn name flags db in
    trivial_txn ~write:create env f ;

    (* We do not put a finaliser here, as it would break with mdb_drop.
       Slight memory leak, but nothing terrible. *)
    (* Gc.finalise mdb_dbi_close env !@db *)
    { env ; db = !@db }

  let stats { env ; db } =
    let stats = make mdb_stat in
    trivial_txn ~write:false env (fun t ->
      mdb_dbi_stat t db (addr stats)
    ) ;
    make_stats stats

  let flags { env ; db } =
    let flags = alloc mdb_dbi_open_flag in
    trivial_txn ~write:false env (fun t ->
      mdb_dbi_flags t db flags
    ) ;
    !@flags

  let drop ?(delete=false) { env ; db } =
    trivial_txn ~write:true env (fun t ->
      mdb_drop t db delete
    )

  let get { db ; env } k =
    let v = addr @@ make mdb_val in
    trivial_txn ~write:false env (fun t ->
      mdb_get t db (Key.write k) v)
    ;
    Val.read v

  let put ?(flags=PutFlags.none) { db ; env } k v =
    trivial_txn ~write:true env (fun t ->
      mdb_put t db (Key.write k) (Val.write v) flags
    )

  let del ?v { db ; env } k =
    trivial_txn ~write:true env (fun t ->
      match v with
        | Some v -> mdb_del t db (Key.write k) (Val.write v)
        | None ->  mdb_del t db (Key.write k) @@ from_voidp mdb_val null
    )

  module Txn = struct

    type 'a txn = { rw : 'a ; txn : mdb_txn ; db : mdb_dbi }
      constraint 'a = [< `Read | `Write ]

    let go ?parent ~rw { env ; db } f =
      let txn = alloc mdb_txn in
      let parent = opt_map (fun x -> x.txn) parent in
      let txn_flag = match rw with
        | `Write -> Env.Flags.none
        | `Read -> Env.Flags.rdonly
      in
      mdb_txn_begin env parent txn_flag txn ;
      try match f { rw ; txn = !@txn ; db } with
        | `Ok x -> mdb_txn_commit !@txn ; Some x
        | `Abort -> mdb_txn_abort !@txn ; None
      with
        | Abort t' when t' == !@txn || parent = None ->
            mdb_txn_abort !@txn ; None
        | exn -> mdb_txn_abort !@txn ; raise exn

    let abort { txn } = raise (Abort txn)

    let stats { txn ; db } =
      let stats = make mdb_stat in
      mdb_dbi_stat txn db (addr stats) ;
      make_stats stats

    let flags { txn ; db } =
      let flags = alloc mdb_dbi_open_flag in
      mdb_dbi_flags txn db flags ;
      !@flags

    let drop ?(delete=false) { txn ; db } =
      mdb_drop txn db delete

    let get { db ; txn } k =
      let v = addr @@ make mdb_val in
      mdb_get txn db (Key.write k) v ;
      Val.read v

    let put ?(flags=PutFlags.none) { db ; txn } k v =
      mdb_put txn db (Key.write k) (Val.write v) flags

    let del ?v { db ; txn } k =
      match v with
        | Some v -> mdb_del txn db (Key.write k) (Val.write v)
        | None ->  mdb_del txn db (Key.write k) @@ from_voidp mdb_val null

    let env { txn } = mdb_txn_env txn

  end

end

module Db = Make (Key.String) (Val.String)
module IntDb = Make (Key.Int) (Val.String)
