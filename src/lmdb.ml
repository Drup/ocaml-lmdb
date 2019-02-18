module Mdb = Lmdb_bindings
module Bigstring = Bigstringaf

let version = Mdb.version

exception Not_found = Not_found
exception Exists = Mdb.Exists
exception Error = Mdb.Error

type error = int

let pp_error fmt i =
  Format.fprintf fmt "%s@." (Mdb.strerror i)

type 'a cap =
  | Ro : [ `Read ] cap
  | Rw : [ `Read | `Write ] cap
let ro = Ro
let rw = Rw

module Env = struct

  type t = [> `Read | `Write ] Mdb.env

  (* exception Assert of (t * string) *)

  module Flags = Mdb.EnvFlags

  let create ?max_readers ?map_size ?max_dbs ?(flags=Flags.none) ?(mode=0o755) path =
    let env = Mdb.env_create () in
    try
      let opt_iter f = function
        | None -> ()
        | Some x -> f x
      in
      opt_iter (Mdb.env_set_mapsize env) map_size ;
      opt_iter (Mdb.env_set_maxdbs env) max_dbs ;
      opt_iter (Mdb.env_set_maxreaders env) max_readers ;
      (* Mdb.env_set_assert env (fun env s -> raise (Assert (env,s))) ; *)
      Mdb.env_open env path flags mode ;
      env
    with Error _ as exn -> Mdb.env_close env; raise exn

  let close = Mdb.env_close

  let copy ?(compact=false) db s =
    let flag = if compact then Mdb.CopyFlags.compact else Mdb.CopyFlags.none in
    Mdb.env_copy db s flag

  let copyfd ?(compact=false) env (fd : Unix.file_descr) =
    let flag = if compact then Mdb.CopyFlags.compact else Mdb.CopyFlags.none in
    Mdb.env_copyfd env fd flag

  let set_flags = Mdb.env_set_flags
  let flags = Mdb.env_get_flags

  let set_map_size = Mdb.env_set_mapsize

  let path = Mdb.env_get_path
  let sync ?(force=false) env = Mdb.env_sync env force

  let fd = Mdb.env_get_fd

  let max_readers = Mdb.env_get_maxreaders

  let max_keysize = Mdb.env_get_maxkeysize

  let reader_list env =
    let x = ref [] in
    assert (Mdb.reader_list env (fun s -> x := s::!x ; 0) = 0);
    !x

  let reader_check = Mdb.reader_check

  let stats = Mdb.env_stat
end

module Txn :
sig
  type -'perm t = 'perm Mdb.txn constraint 'perm = [< `Read | `Write ]

  val go :
    'perm cap ->
    ?txn:([< `Read | `Write ] as 'perm) t ->
    Env.t ->
    ('perm t -> 'b) -> 'b option

  val abort : 'a t -> 'b

  val env : 'a t -> Env.t

  (* not exported: *)
  val trivial :
    'a cap ->
    ?txn:'a t ->
    Env.t ->
    ('a t -> 'b) -> 'b
end
=
struct
  type -'perm t = 'perm Mdb.txn constraint 'perm = [< `Read | `Write ]

  exception Abort of Obj.t

  let env txn = Obj.magic @@ Mdb.txn_env txn

  let abort txn = raise (Abort (Obj.repr txn))

  let go :
    'perm cap ->
    ?txn:([< `Read | `Write ] as 'perm) t ->
    Env.t ->
    ('perm t -> 'b) -> 'b option
    =
    fun (type c) (rw :c cap) ?txn:parent env f ->
    let txn_flag =
      match rw with
      | Ro -> Env.Flags.read_only
      | Rw -> Env.Flags.none
    in
    let txn =
      Mdb.txn_begin
        (env : Env.t :> [< `Read | `Write ] Mdb.env)
        (parent :> [< `Read | `Write ] t option)
        txn_flag
    in
    try
      let x = f txn in
      Mdb.txn_commit txn ; Some x
    with
      | Abort t when t == Obj.repr txn || parent = None ->
        Mdb.txn_abort txn ; None
      | exn -> Mdb.txn_abort txn ; raise exn

  (* Used internally for trivial functions, not exported. *)
  let trivial :
    'a cap ->
    ?txn:'a t ->
    Env.t ->
    ('a t -> 'b) -> 'b
    =
    fun rw ?txn e f ->
    match txn with
    | Some txn -> f txn
    | None ->
      match go rw e f with
      | None -> assert false
      | Some x -> x
end


module PutFlags = Mdb.PutFlags

module Flags = Mdb.DbiFlags

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


module Make (Key : Values.S) (Elt : Values.S) = struct

  let def_flags = Flags.(Key.default_flags + Elt.default_flags)

  let has_dup_flag = Flags.(test dup_sort) def_flags

  type t = {env : Env.t ; db : Mdb.dbi }

  type key = Key.t
  type elt = Elt.t

  let write f v =
    f Bigstring.create v

  let create ?(create=true) env name =
    let flags =
      if create
      then Flags.(create + def_flags)
      else def_flags
    in
    let f txn = Mdb.dbi_open txn (Some name) flags in
    let dbi =
      if create
      then Txn.trivial rw env f
      else Txn.trivial ro env f
    in
    (* We do not put a finaliser here, as it would break with Mdb.drop.
       Slight memory leak, but nothing terrible. *)
    (* Gc.finalise Mdb.dbi_close env !@db *)
    { env ; db = dbi }

  let stats { env ; db } =
    Txn.trivial ro env (fun t ->
      Mdb.dbi_stat t db
    )

  let _flags { env ; db } =
    Txn.trivial ro env (fun t ->
      Mdb.dbi_flags t db
    )

  let drop ?(delete=false) { env ; db } =
    Txn.trivial rw env (fun t ->
      Mdb.drop t db delete
    )

  let get { db ; env } ?txn k =
    Txn.trivial ro
      ?txn:(txn :> [ `Read ] Txn.t option)
      env (fun t ->
        Mdb.get t db (write Key.write k) |> Elt.read)

  let put { db ; env } ?txn ?(flags=PutFlags.none) k v =
    Txn.trivial rw ?txn env (fun t ->
      Mdb.put t db
        (write Key.write k)
        (write Elt.write v)
        flags
    )

  let append db ?txn ?(flags=PutFlags.none) k v =
    let flags =
      let open PutFlags in
      flags +
      if has_dup_flag then PutFlags.append_dup else PutFlags.append
    in
    put db ?txn ~flags k v

  let remove { db ; env } ?txn ?elt k =
    Txn.trivial rw ?txn env (fun t ->
      let va = match elt with
        | None -> Mdb.Block_option.none
        | Some value ->
          Mdb.Block_option.some @@ Elt.write Bigstring.create value
      in
      Mdb.del t db
        (write Key.write k)
        va
    )

  let compare_key { db ; env } ?txn x y =
    Txn.trivial ro
      ?txn:(txn :> [ `Read ] Txn.t option)
      env @@ fun txn ->
    Mdb.cmp txn db
      (write Key.write x)
      (write Key.write y)

  let compare_elt { db ; env } ?txn :elt -> elt -> int =
    if not has_dup_flag then
      invalid_arg "Lmdb: elements are only comparable in a dup_sort db";
    fun x y ->
    Txn.trivial ro
      ?txn:(txn :> [ `Read ] Txn.t option)
      env @@ fun txn ->
    Mdb.dcmp txn db
      (write Elt.write x)
      (write Elt.write y)

  let compare = compare_key

  module Cursor = struct

    type -'perm t = 'perm Mdb.cursor constraint 'perm = [< `Read | `Write ]

    let go rw ?txn db f =
      Txn.trivial rw ?txn db.env @@ fun txn ->
      let cursor : _ t = Mdb.cursor_open txn db.db in
      try
        let res = f cursor in
        Mdb.cursor_close cursor ;
        res
      with exn -> Mdb.cursor_close cursor ; raise exn

    let put cursor ?(flags=PutFlags.none) k v =
      Mdb.cursor_put cursor
        (write Key.write k)
        (write Elt.write v)
        flags

    let put_here cursor ?(flags=PutFlags.none) k v =
      put ~flags:PutFlags.(current + flags) cursor k v

    let remove cursor ?(all=false) () =
      let flag =
        if all
        then
          if has_dup_flag then PutFlags.no_dup_data
          else raise @@ Invalid_argument (Printf.sprintf
                "Lmdb.Cursor.del: Optional argument ~all unsuported: \
                 this database does not have the dupsort flag enabled.")
        else PutFlags.none
      in
      Mdb.cursor_del cursor flag

    let get_prim op cursor =
      let k,v =
      Mdb.cursor_get cursor
        Mdb.Block_option.none
        Mdb.Block_option.none
        op
      in
      Key.read k, Elt.read v

    let get = get_prim Mdb.get_current
    let first = get_prim Mdb.first
    let last = get_prim Mdb.last
    let next = get_prim Mdb.next
    let prev = get_prim Mdb.prev

    let seek_prim op cursor k =
      let _,v =
        Mdb.cursor_get cursor
          (Mdb.Block_option.some @@ write Key.write k)
          Mdb.Block_option.none
          op
      in
      Elt.read v

    let seek = seek_prim Mdb.set
    let seek_range = seek_prim Mdb.set_range

    let test_dup s f op cursor =
      if has_dup_flag then f op cursor
      else raise @@ Invalid_argument (Printf.sprintf
            "Lmdb.Cursor.%s: Operation unsuported: this database does not have the \
             dupsort flag enabled." s)

    let first_dup = test_dup "first_dup" get_prim Mdb.first_dup
    let last_dup = test_dup "last_dup" get_prim Mdb.last_dup
    let next_dup = test_dup "next_dup" get_prim Mdb.next_dup
    let prev_dup = test_dup "prev_dup" get_prim Mdb.prev_dup

    let seek_dup = test_dup "seek_dup" seek_prim Mdb.get_both
    let seek_range_dup = test_dup "seek_range_dup" seek_prim Mdb.get_both_range

    (* The following two operations are not exposed, due to inherent unsafety:
       - Mdb.GET_MULTIPLE
       - Mdb.NEXT_MULTIPLE
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
  val get : t -> ?txn:[> `Read] Txn.t -> key -> elt
  val put : t -> ?txn:[> `Read | `Write] Txn.t -> ?flags:PutFlags.t -> key -> elt -> unit
  val append : t -> ?txn:[> `Read | `Write] Txn.t -> ?flags:PutFlags.t -> key -> elt -> unit
  val remove : t -> ?txn:[> `Read | `Write] Txn.t -> ?elt:elt -> key -> unit

  module Cursor : sig
    type db
    type -'a t constraint 'a = [< `Read | `Write ]

    val go : 'c cap -> ?txn:'c Txn.t -> db -> ('c t -> 'a) -> 'a

    val get : [> `Read ] t -> key * elt
    val put : [> `Read | `Write ] t -> ?flags:PutFlags.t -> key -> elt -> unit
    val put_here : [> `Read | `Write ] t -> ?flags:PutFlags.t -> key -> elt -> unit
    val remove : [> `Read | `Write ] t -> ?all:bool -> unit -> unit

    val first : [> `Read ] t -> key * elt
    val last : [> `Read ] t -> key * elt
    val next : [> `Read ] t -> key * elt
    val prev : [> `Read ] t -> key * elt

    val seek : [> `Read ] t -> key -> elt
    val seek_range : [> `Read ] t -> key -> elt

    val first_dup : [> `Read ] t -> key * elt
    val last_dup : [> `Read ] t -> key * elt
    val next_dup : [> `Read ] t -> key * elt
    val prev_dup : [> `Read ] t -> key * elt

    val seek_dup : [> `Read ] t -> key -> elt
    val seek_range_dup : [> `Read ] t -> key -> elt

  end with type db := t

  val stats : t -> Mdb.stats
  val drop : ?delete:bool -> t -> unit
  val compare_key : t -> ?txn:[> `Read ] Txn.t -> key -> key -> int
  val compare : t -> ?txn:[> `Read ] Txn.t -> key -> key -> int
  val compare_elt : t -> ?txn:[> `Read ] Txn.t -> elt -> elt -> int
end
