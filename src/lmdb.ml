open Ctypes
open PosixTypes

module S = Lmdb_bindings.Make(Lmdb_generated)
open S
open Lmdb_bindings.T

let dummy_ref = Weak.create 1
(* keep a alive while b is alive *)
let alive_while a b =
  Gc.finalise (fun _ -> Weak.set dummy_ref 0 (Some (Obj.repr a))) b

let alloc = allocate_n ~count:1

let opt_iter f = function
  | None -> ()
  | Some x -> f x

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

(** {2 High level binding} *)

let version () =
  let major = alloc int in
  let minor = alloc int in
  let patch = alloc int in
  let s = mdb_version major minor patch in
  (s, !@major, !@minor, !@patch)

type error = int

exception Not_found = Lmdb_bindings.Not_found
exception Exists = Lmdb_bindings.Exists
exception Error = Lmdb_bindings.Error

let () =
  Printexc.register_printer @@ function
  | Error i -> Some ("Lmdb.Error(" ^ mdb_strerror i ^ ")")
  | Exists -> Some "Lmdb.Exists"
  | _ -> None

let pp_error fmt i =
  Format.fprintf fmt "%s@." (mdb_strerror i)

module Env = struct

  type t = mdb_env

  (* exception Assert of (t * string) *)

  module Flags = struct
    type t = mdb_env_flag
    let (+) = Unsigned.UInt.logor
    let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
    let eq f f' = Unsigned.UInt.(compare f f' = 0)
    let none = Unsigned.UInt.zero
    let fixed_map   = mdb_FIXEDMAP
    let no_subdir   = mdb_NOSUBDIR
    let no_sync     = mdb_NOSYNC
    let read_only     = mdb_RDONLY
    let no_meta_sync = mdb_NOMETASYNC
    let write_map   = mdb_NOMETASYNC
    let map_async   = mdb_MAPASYNC
    let no_tls      = mdb_NOTLS
    let no_lock     = mdb_NOLOCK
    let no_read_ahead  = mdb_NORDAHEAD
    let no_mem_init  = mdb_NOMEMINIT
  end

  let create ?max_readers ?map_size ?max_dbs ?(flags=Flags.none) ?(mode=0o755) path =
    let mode = Mode.of_int mode in
    let env_ptr = alloc mdb_env in
    mdb_env_create env_ptr ;
    let env = !@env_ptr in
    try
      opt_iter (mdb_env_set_mapsize env) map_size ;
      opt_iter (mdb_env_set_maxdbs env) max_dbs ;
      opt_iter (mdb_env_set_maxreaders env) max_readers ;
      (* mdb_env_set_assert env (fun env s -> raise (Assert (env,s))) ; *)
      mdb_env_open env path flags mode ;
      Gc.finalise mdb_env_close env ;
      env
    with Error _ as exn -> mdb_env_close env; raise exn

  module CopyFlags = struct
    type t = mdb_copy_flag
    let none : t = Unsigned.UInt.zero
    let compact = mdb_CP_COMPACT
  end

  let copy ?(compact=false) db s =
    let flag = if compact then CopyFlags.compact else CopyFlags.none in
    mdb_env_copy2 db s flag

  let copyfd ?(compact=false) env (fd : Unix.file_descr) =
    let flag = if compact then CopyFlags.compact else CopyFlags.none in
    mdb_env_copyfd2 env fd flag

  let set_flags = mdb_env_set_flags
  let flags env =
    let flags = alloc mdb_env_flag in
    mdb_env_get_flags env flags ;
    !@flags

  let set_map_size = mdb_env_set_mapsize

  let path env =
    let path = alloc string in
    mdb_env_get_path env path ;
    !@path

  let fd env =
    let fd = alloc mdb_filehandle_t in
    mdb_env_get_fd env fd ;
    !@fd

  let sync ?(force=false) env = mdb_env_sync env force

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


  let stats env =
    let stats = make mdb_stat in
    mdb_env_stat env (addr stats) ;
    make_stats stats

end

(* Use internally for trivial functions *)
let trivial_txn ~write env f =
  let txn = alloc mdb_txn in
  let txn_flag = if write
    then Env.Flags.none
    else Env.Flags.read_only
  in
  mdb_txn_begin env None txn_flag txn ;
  try
    let x = f !@txn in
    mdb_txn_commit !@txn ;
    x
  with e ->
    mdb_txn_abort !@txn ;
    raise e


module PutFlags = struct
  type t = mdb_put_flag
  let (+) = Unsigned.UInt.logor
  let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt.(compare f f' = 0)
  let none = Unsigned.UInt.zero
  let no_overwrite = mdb_NOOVERWRITE
  let no_dup_data   = mdb_NODUPDATA
  let current     = mdb_CURRENT
  let _reserve     = mdb_RESERVE
  let append      = mdb_APPEND
  let append_dup   = mdb_APPENDDUP
  let _multiple    = mdb_MULTIPLE
end

module Flags = struct
  type t = mdb_env_flag
  let (+) = Unsigned.UInt.logor
  let test f m = Unsigned.UInt.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt.(compare f f' = 0)
  let none = Unsigned.UInt.zero
  let reverse_key = mdb_REVERSEKEY
  let dup_sort    = mdb_DUPSORT
  let dup_fixed   = mdb_DUPFIXED
  let integer_dup = mdb_INTEGERDUP
  let reverse_dup = mdb_REVERSEDUP
  let integer_key = mdb_INTEGERKEY

  (* Not exported *)
  let create     = mdb_CREATE
end

module Bigstring = Bigstringaf
type buffer = Bigstring.t

module Values = struct

  module Flags = Flags

  type db_val = buffer

  module type S = sig
    type t
    val default_flags : Flags.t
    val read : db_val -> t
    val write : (int -> db_val) -> t -> db_val
  end

  module Int : S with type t = int = struct
    type t = int
    let default_flags = Flags.none

    let write, read =
      match Sys.big_endian, (Sys.int_size + 7) / 8 * 8 with
      | true, 32 ->
        (fun alloc x ->
           let a = alloc 4 in
           Bigstring.set_int32_be a 0 Int32.(of_int x);
           a),
        (fun a -> Bigstring.get_int32_be a 0 |> Int32.to_int)
      | true, 64 ->
        (fun alloc x ->
           let a = alloc 8 in
           Bigstring.set_int64_be a 0 Int64.(of_int x);
           a),
        (fun a -> Bigstring.get_int64_be a 0 |> Int64.to_int)
      | false, 32 ->
        (fun alloc x ->
           let a = alloc 4 in
           Bigstring.set_int32_le a 0 Int32.(of_int x);
           a),
        (fun a -> Bigstring.get_int32_le a 0 |> Int32.to_int)
      | false, 64 ->
        (fun alloc x ->
           let a = alloc 8 in
           Bigstring.set_int64_le a 0 Int64.(of_int x);
           a),
        (fun a -> Bigstring.get_int64_le a 0 |> Int64.to_int)
      | _ -> failwith "Lmdb: Unsupported integer size"
  end

  module String : S with type t = string = struct
    type t = string
    let default_flags = Flags.none

    let write, read =
      (fun alloc s ->
         let len = String.length s in
         let a = alloc len in
         Bigstring.blit_from_string s ~src_off:0 a ~dst_off:0 ~len;
         a),
      (fun a -> Bigstring.substring a ~off:0 ~len:(Bigstring.length a))
  end


  module Elt = struct
    module Int = Int
    module String = String
  end

  module Key = struct
    module Int = struct
      include Elt.Int
      let default_flags = Flags.integer_key
    end
    module String = String
  end

end


exception Abort of mdb_txn

module Make (Key : Values.S) (Elt : Values.S) = struct

  let def_flags = Flags.(Key.default_flags + Elt.default_flags)

  let has_dup_flag = Flags.(test dup_sort) def_flags

  type t = {env : Env.t ; db : mdb_dbi }

  type key = Key.t
  type elt = Elt.t

  let dbval_of_buffer b =
    let mvp = addr (make mdb_val) in
    alive_while b mvp; (* Make sure buffer will stay alive while mvp is in use. *)
    (mvp |-> mv_size) <-@ Bigstring.length b ;
    (mvp |-> mv_data) <-@ to_voidp @@ bigarray_start array1 b ; (* Reference to b is lost here! *)
    (*Gc.full_major () ;*) (* trigger possible use-after-free. *)
    mvp
  let buffer_of_dbval mvp =
    (* no need to keep a reference to mvp here, because the memory the bigarray
     * is mapped to is in the lmdb map and only valid as long as the transaction
     * is alive. The user knows this. *)
    bigarray_of_ptr array1
      (!@ (mvp |-> mv_size))
      Char
      (!@ (mvp |-> mv_data) |> from_voidp char)

  let write f v =
    f Bigstring.create v

  let create ?(create=true) env name =
    let db = alloc mdb_dbi in
    let flags =
      if create
      then Flags.(create + def_flags)
      else def_flags
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
    Env.make_stats stats

  let _flags { env ; db } =
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
        mdb_get t db (dbval_of_buffer @@ write Key.write k) v;
        Elt.read @@ buffer_of_dbval v)

  let put ?(flags=PutFlags.none) { db ; env } k v =
    trivial_txn ~write:true env (fun t ->
      mdb_put t db
        (dbval_of_buffer @@ write Key.write k)
        (dbval_of_buffer @@ write Elt.write v)
        flags
    )

  let remove ?elt { db ; env } k =
    trivial_txn ~write:true env (fun t ->
      match elt with
        | Some v ->
          mdb_del t db
            (dbval_of_buffer @@ write Key.write k)
            (dbval_of_buffer @@ write Elt.write v)
        | None ->
          mdb_del t db
            (dbval_of_buffer @@ write Key.write k)
            (from_voidp mdb_val null)
    )

  module Txn = struct

    type 'a txn = { txn : mdb_txn ; db : mdb_dbi }
      constraint 'a = [< `Read | `Write ]

    let go ?parent ~rw { env ; db } f =
      let ptr_txn = alloc mdb_txn in
      let parent = opt_map (fun x -> x.txn) parent in
      let txn_flag = match rw with
        | `Write -> Env.Flags.none
        | `Read -> Env.Flags.read_only
      in
      mdb_txn_begin env parent txn_flag ptr_txn ;
      let txn = !@ptr_txn in
      try
        let x = f { txn = txn ; db } in
        mdb_txn_commit txn ; Some x
      with
        | Abort t' when t' == txn || parent = None ->
          mdb_txn_abort txn ; None
        | exn -> mdb_txn_abort txn ; raise exn

    let abort { txn ; _ } = raise (Abort txn)

    let stats { txn ; db } =
      let stats = make mdb_stat in
      mdb_dbi_stat txn db (addr stats) ;
      Env.make_stats stats

    let flags { txn ; db } =
      let flags = alloc mdb_dbi_open_flag in
      mdb_dbi_flags txn db flags ;
      !@flags

    let drop ?(delete=false) { txn ; db } =
      mdb_drop txn db delete

    let get { db ; txn } k =
      let v = addr @@ make mdb_val in
      mdb_get txn db (dbval_of_buffer @@ write Key.write k) v ;
      Elt.read @@ buffer_of_dbval v

    let put ?(flags=PutFlags.none) { db ; txn } k v =
      mdb_put txn db
        (dbval_of_buffer @@ write Key.write k)
        (dbval_of_buffer @@ write Elt.write v)
        flags

    let append t k v =
      let flags =
        if has_dup_flag then PutFlags.append_dup else PutFlags.append
      in
      put ~flags t k v

    let remove ?elt { db ; txn } k =
      match elt with
        | Some v ->
          mdb_del txn db
            (dbval_of_buffer @@ write Key.write k)
            (dbval_of_buffer @@ write Elt.write v)
        | None ->
          mdb_del txn db
            (dbval_of_buffer @@ write Key.write k)
            (from_voidp mdb_val null)

    let env { txn ; _ } = mdb_txn_env txn

    let compare_key { db ; txn } :key -> key -> int =
      fun x y ->
      mdb_cmp txn db
        (dbval_of_buffer @@ write Key.write x)
        (dbval_of_buffer @@ write Key.write y)

    let compare_elt ({ db ; txn } as t) :elt -> elt -> int =
      if not (Flags.(test dup_sort) @@ flags t) then
        invalid_arg "Lmdb: elements are only comparable in a dup_sort db";
      fun x y ->
      mdb_dcmp txn db
        (dbval_of_buffer @@ write Elt.write x)
        (dbval_of_buffer @@ write Elt.write y)

    let compare = compare_key

  end

  let append { db ; env } k v =
    trivial_txn ~write:true env @@ fun txn -> Txn.append {Txn. db ; txn} k v

  let compare_key {db ; env} x y =
    trivial_txn ~write:false env @@ fun txn -> Txn.compare_key {Txn. db ; txn} x y

  let compare_elt {db ; env} x y =
    trivial_txn ~write:false env @@ fun txn -> Txn.compare_elt {Txn. db ; txn} x y

  let compare = compare_key

  module Cursor = struct

    type 'a t = mdb_cursor constraint 'a = [< `Read | `Write ]

    let go {Txn. txn ; db } ~f =
      let ptr_cursor = alloc mdb_cursor in
      mdb_cursor_open txn db ptr_cursor ;
      let cursor : _ t = !@ptr_cursor in
      try
        let res = f cursor in
        mdb_cursor_close cursor ;
        res
      with exn -> mdb_cursor_close cursor ; raise exn

    let put ?(flags=PutFlags.none) cursor k v =
      mdb_cursor_put cursor
        (dbval_of_buffer @@ write Key.write k)
        (dbval_of_buffer @@ write Elt.write v)
        flags

    let put_here ?(flags=PutFlags.none) cursor k v =
      put ~flags:PutFlags.(current + flags) cursor k v

    let remove ?(all=false) cursor =
      let flag =
        if all
        then
          if has_dup_flag then PutFlags.no_dup_data
          else raise @@ Invalid_argument (Printf.sprintf
                "Lmdb.Cursor.del: Optional argument ~all unsuported: \
                 this database does not have the dupsort flag enabled.")
        else PutFlags.none
      in
      mdb_cursor_del cursor flag

    let get_prim op cursor =
      let k = addr @@ make mdb_val in
      let v = addr @@ make mdb_val in
      mdb_cursor_get cursor k v op ;
      Key.read @@ buffer_of_dbval k,
      Elt.read @@ buffer_of_dbval v

    let get = get_prim MDB_GET_CURRENT
    let first = get_prim MDB_FIRST
    let last = get_prim MDB_LAST
    let next = get_prim MDB_NEXT
    let prev = get_prim MDB_PREV

    let seek_prim op cursor k =
      let v = addr @@ make mdb_val in
      mdb_cursor_get cursor
        (dbval_of_buffer @@ write Key.write k)
        v op ;
      Elt.read @@ buffer_of_dbval v

    let seek = seek_prim MDB_SET
    let seek_range = seek_prim MDB_SET_RANGE

    let test_dup s f flag x =
      if has_dup_flag then f flag x
      else raise @@ Invalid_argument (Printf.sprintf
            "Lmdb.Cursor.%s: Operation unsuported: this database does not have the \
             dupsort flag enabled." s)

    let first_dup = test_dup "first_dup" get_prim MDB_FIRST_DUP
    let last_dup = test_dup "last_dup" get_prim MDB_LAST_DUP
    let next_dup = test_dup "next_dup" get_prim MDB_NEXT_DUP
    let prev_dup = test_dup "prev_dup" get_prim MDB_PREV_DUP

    let seek_dup = test_dup "seek_dup" seek_prim MDB_GET_BOTH
    let seek_range_dup = test_dup "seek_range_dup" seek_prim MDB_GET_BOTH_RANGE

    (* The following two operations are not exposed, due to inherent unsafety:
       - MDB_GET_MULTIPLE
       - MDB_NEXT_MULTIPLE
    *)

  end

end

module Db = Make (Values.Key.String) (Values.Elt.String)
module IntDb = Make (Values.Key.Int) (Values.Elt.String)


module type S = sig

  type t

  type key
  type elt

  val create : ?create:bool -> Env.t -> string -> t
  val get : t -> key -> elt
  val put : ?flags:PutFlags.t -> t -> key -> elt -> unit
  val append : t -> key -> elt -> unit
  val remove : ?elt:elt -> t -> key -> unit

  module Txn : sig

    type 'a txn constraint 'a = [< `Read | `Write ]

    val go :
      ?parent:([< `Read | `Write ] as 'a) txn ->
      rw:'a ->
      t ->
      ('a txn -> 'b) -> 'b option

    val abort : 'a txn -> 'b
    val get : 'a txn -> key -> elt
    val put : ?flags:PutFlags.t -> [> `Write ] txn -> key -> elt -> unit
    val append : [> `Write] txn -> key -> elt -> unit
    val remove : ?elt:elt -> [> `Write ] txn -> key -> unit
    val env : 'a txn -> Env.t
    val stats : 'a txn -> Env.stats
    val compare : 'a txn -> key -> key -> int
    val compare_key : 'a txn -> key -> key -> int
    val compare_elt : 'a txn -> elt -> elt -> int
    val drop : ?delete:bool -> [< `Write ] txn -> unit

  end

  module Cursor : sig

    type 'a t constraint 'a = [< `Read | `Write ]

    val go : 'cap Txn.txn -> f:('cap t -> 'a) -> 'a

    val get : _ t -> key * elt
    val put : ?flags:PutFlags.t -> [> `Write ] t -> key -> elt -> unit
    val put_here : ?flags:PutFlags.t -> [> `Write ] t -> key -> elt -> unit
    val remove : ?all:bool -> [> `Write ] t -> unit

    val first : _ t -> key * elt
    val last : _ t -> key * elt
    val next : _ t -> key * elt
    val prev : _ t -> key * elt

    val seek : _ t -> key -> elt
    val seek_range : _ t -> key -> elt

    val first_dup : _ t -> key * elt
    val last_dup : _ t -> key * elt
    val next_dup : _ t -> key * elt
    val prev_dup : _ t -> key * elt

    val seek_dup : _ t -> key -> elt
    val seek_range_dup : _ t -> key -> elt

  end

  val stats : t -> Env.stats
  val drop : ?delete:bool -> t -> unit
  val compare : t -> key -> key -> int
  val compare_key : t -> key -> key -> int
  val compare_elt : t -> elt -> elt -> int
end
