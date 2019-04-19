module Mdb = Lmdb_bindings
module type Flags = Mdb.Flags
module Bigstring = Bigstringaf

exception Not_found = Not_found
exception Exists = Mdb.Exists
exception Error = Mdb.Error

type 'a perm =
  | Ro : [ `Read ] perm
  | Rw : [ `Read | `Write ] perm

let version = Mdb.version

let pp_error fmt i =
  Format.fprintf fmt "%s@." (Mdb.strerror i)

module Env = struct

  type -'perm t = 'perm Mdb.env constraint 'perm = [< `Read | `Write ]

  (* exception Assert of (t * string) *)

  module Flags = Mdb.EnvFlags

  let create (type p)
      ?max_readers ?map_size ?max_maps
      ?(flags=Flags.none) ?(mode=0o755)
      (perm : p perm)
      path =
    let flags =
      match perm with
      | Rw -> flags
      | Ro -> Flags.(flags + read_only)
    in
    let env = Mdb.env_create () in
    try
      let opt_iter f = function
        | None -> ()
        | Some x -> f x
      in
      opt_iter (Mdb.env_set_mapsize env) map_size ;
      opt_iter (Mdb.env_set_maxdbs env) max_maps ;
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
    ?perm:'perm perm ->
    ?txn:'perm t ->
    'perm Env.t ->
    ('perm t -> 'a) -> 'a option


  val abort : _ t -> 'b

  val env : 'perm t -> 'perm Env.t

  (* not exported: *)
  val trivial :
    'perm perm ->
    ?txn:'perm t ->
    'perm Env.t ->
    ('perm t -> 'a) -> 'a
end
=
struct
  type -'perm t = 'perm Mdb.txn constraint 'perm = [< `Read | `Write ]

  exception Abort of Obj.t

  let env = Mdb.txn_env

  let flags_of_env (type p) (perm: p perm option) env =
    match perm with
    | Some Rw -> Env.Flags.none
    | Some Ro -> Env.Flags.read_only
    | None -> Env.Flags.(read_only * Env.flags env)

  let abort txn = raise (Abort (Obj.repr txn))

  let go (type p) ?(perm:p perm option) ?txn:parent env f =
    let flags = flags_of_env perm env in
    let txn = Mdb.txn_begin env parent flags in
    try
      let x = f txn in
      Mdb.txn_commit txn ; Some x
    with
      | Abort t when t == Obj.repr txn || parent = None ->
        Mdb.txn_abort txn ; None
      | exn -> Mdb.txn_abort txn ; raise exn

  (* Used internally for trivial functions, not exported. *)
  let trivial perm ?txn e f =
    match txn with
    | Some txn ->
      if e != env txn
      (* Cave: this error is not caught by lmdb *)
      then invalid_arg "Lmdb: transaction from wrong environment."
      else f txn
    | None ->
      match go ~perm e f with
      | None -> assert false
      | Some x -> x
end

module Map = struct
  module Conv = struct
    type bigstring = Bigstring.t

    module Flags = Mdb.DbiFlags

    module type S = sig
      type t
      val flags : Flags.t
      val read : Bigstring.t -> t
      val write : (int -> Bigstring.t) -> t -> Bigstring.t
    end

    type 'a t = {
      flags : Flags.t ;
      read : Bigstring.t -> 'a ;
      write : (int -> Bigstring.t) -> 'a -> Bigstring.t ;
    }
    let mk (type a) (module M : S with type t = a) =
      let open M in { flags ; read ; write }
    let as_module (type a) { flags ; read ; write } =
      let module M = struct
        type t = a
        let flags = flags
        let read = read
        let write = write
      end
      in
      (module M : S with type t = a)

    let is_int_size n = n = Mdb.sizeof_int || n = Mdb.sizeof_size_t

    module Int32_be = struct
      type t = Int32.t
      let flags =
        if Sys.big_endian && is_int_size 4
        then Flags.(integer_key + integer_dup + dup_fixed)
        else Flags.(dup_fixed)
      let read a = Bigstring.get_int32_be a 0
      let write alloc x =
        let a = alloc 4 in
        Bigstring.set_int32_be a 0 x;
        a
    end

    module Int32_le = struct
      type t = Int32.t
      let flags =
        if not Sys.big_endian && is_int_size 4
        then Flags.(integer_key + integer_dup + dup_fixed)
        else Flags.(reverse_key + reverse_dup + dup_fixed)
      let read a = Bigstring.get_int32_le a 0
      let write alloc x =
        let a = alloc 4 in
        Bigstring.set_int32_le a 0 x;
        a
    end

    module Int32_as_int (Conv :S with type t = Int32.t) : S with type t = int
    = struct
      type t = int
      let flags = Conv.flags
      let read =
        if Sys.int_size >= 32
        then fun a ->
          Conv.read a |> Int32.to_int
        else fun a ->
          let ix = Conv.read a in
          let i = Int32.to_int ix in
          if Int32.of_int i = ix
          then i
          else invalid_arg "Lmdb: Integer truncated"
      let write =
        if Sys.int_size <= 32
        then fun alloc i ->
          Conv.write alloc @@ Int32.of_int i
        else fun alloc i ->
          let ix = Int32.of_int i in
          if Int32.to_int ix = i
          then Conv.write alloc ix
          else invalid_arg "Lmdb: Integer truncated"
    end

    module Int32_be_as_int = Int32_as_int (Int32_be)
    module Int32_le_as_int = Int32_as_int (Int32_le)

    module Int64_be = struct
      type t = Int64.t
      let flags =
        if Sys.big_endian && is_int_size 8
        then Flags.(integer_key + integer_dup + dup_fixed)
        else Flags.(dup_fixed)
      let read a = Bigstring.get_int64_be a 0
      let write alloc x =
        let a = alloc 8 in
        Bigstring.set_int64_be a 0 x;
        a
    end

    module Int64_le = struct
      type t = Int64.t
      let flags =
        if not Sys.big_endian && is_int_size 8
        then Flags.(integer_key + integer_dup + dup_fixed)
        else Flags.(reverse_key + reverse_dup + dup_fixed)
      let read a = Bigstring.get_int64_le a 0
      let write alloc x =
        let a = alloc 8 in
        Bigstring.set_int64_le a 0 x;
        a
    end

    module Int64_as_int (Conv :S with type t = Int64.t) :S with type t = int
    = struct
      type t = int
      let flags = Conv.flags
      let read =
        if Sys.int_size >= 64
        then fun a ->
          Conv.read a |> Int64.to_int
        else fun a ->
          let ix = Conv.read a in
          let i = Int64.to_int ix in
          if Int64.of_int i = ix
          then i
          else invalid_arg "Lmdb: Integer truncated"
      let write alloc i =
          Conv.write alloc @@ Int64.of_int i
    end

    module Int64_be_as_int = Int64_as_int (Int64_be)
    module Int64_le_as_int = Int64_as_int (Int64_le)
    let int32_be        = mk (module Int32_be)
    let int32_le        = mk (module Int32_le)
    let int64_be        = mk (module Int64_be)
    let int64_le        = mk (module Int64_le)
    let int32_be_as_int = mk (module Int32_be_as_int)
    let int32_le_as_int = mk (module Int32_le_as_int)
    let int64_be_as_int = mk (module Int64_be_as_int)
    let int64_le_as_int = mk (module Int64_le_as_int)

    module String = struct
      type t = string
      let flags = Flags.none
      let read a = Bigstring.substring a ~off:0 ~len:(Bigstring.length a)
      let write alloc s =
        let len = String.length s in
        let a = alloc len in
        Bigstring.blit_from_string s ~src_off:0 a ~dst_off:0 ~len;
        a
    end
    let string = mk (module String)

    module Bigstring = struct
      type t = Bigstring.t
      let flags = Flags.none
      let read b = b
      let write _ b = b
    end
    let bigstring = mk (module Bigstring)
  end

  type ('k, 'v, -'perm, -'dup) t =
    { env               :'perm Env.t
    ; mutable dbi       :Mdb.dbi
    ; flags             :Mdb.DbiFlags.t
    ; key               : 'k Conv.t
    ; value             : 'v Conv.t
    }
    constraint 'perm = [< `Read | `Write ]
    constraint 'dup = [< `Dup | `Uni ]

  let env { env; _ } = env

  type 'a card = Dup | Nodup constraint 'a = [< `Dup | `Uni ]
  let dup :[ `Uni | `Dup ] card = Dup
  let nodup :[ `Uni ] card = Nodup

  let create
      (dup      :([< `Dup | `Uni ] as 'dup) card)
      (type key value)
      ~(key     :key Conv.t)
      ~(value   :value Conv.t)
      ?(txn     :'perm Txn.t option)
      ?(name    :string option)
      (env      :'perm Env.t)
    :(key, value, 'perm, 'dup) t
    =
    let flags =
      let open Conv.Flags in
      create +
      key.flags * (reverse_key + integer_key) +
      match dup with
      | Nodup -> Conv.Flags.none
      | Dup ->
        dup_sort +
        value.flags * (dup_fixed + integer_dup + reverse_dup)
    in
    let dbi, flags =
      Txn.trivial Rw env ?txn @@ fun txn ->
      let dbi = Mdb.dbi_open txn name flags in
      let flags = Mdb.dbi_flags txn dbi in
      begin match dup with
        | Dup when not (Conv.Flags.(test dup_sort) flags) ->
          invalid_arg "Lmdb.Map.create: duplicates not supported on this map"
        | Dup | Nodup -> ()
      end;
      dbi, flags
    in
    let db_t = { env; dbi; flags; key; value } in
    Gc.finalise
      (fun {env; dbi; _} -> if dbi != Mdb.invalid_dbi then Mdb.dbi_close env dbi)
      db_t;
    db_t

  let open_existing
      (dup       :([< `Dup | `Uni ] as 'dup) card)
      (type key value)
      ~(key     :key Conv.t)
      ~(value   :value Conv.t)
      ?(txn     :_ Txn.t option)
      ?(name    :string option)
      (env      :'perm Env.t)
    :(key, value, 'perm, 'dup) t
    =
    let dbi, flags =
      Txn.trivial Ro
        (env :> [ `Read ] Env.t)
        ?txn:(txn :> [ `Read ] Txn.t option) @@ fun txn ->
      let dbi = Mdb.dbi_open txn name Mdb.DbiFlags.none in
      let flags = Mdb.dbi_flags txn dbi in
      begin match dup with
        | Dup when not (Conv.Flags.(test dup_sort) flags) ->
          invalid_arg "Lmdb.Map.create: duplicates not supported on this map"
        | Dup | Nodup -> ()
      end;
      dbi, flags
    in
    let db_t = { env; dbi; flags; key; value } in
    Gc.finalise
      (fun {env; dbi; _} -> if dbi != Mdb.invalid_dbi then Mdb.dbi_close env dbi)
      db_t;
    db_t

  let stats ?txn {env; dbi; _} =
    Txn.trivial Ro
      ?txn:(txn :> [ `Read ] Txn.t option)
      (env :> [ `Read ] Env.t)
    @@ fun txn ->
    Mdb.dbi_stat txn dbi

  let _flags ?txn {env; dbi; _} =
    Txn.trivial Ro env ?txn @@ fun txn ->
    Mdb.dbi_flags txn dbi

  let drop ?txn ?(delete=false) ({dbi ;env ;_ } as map) =
    if delete then map.dbi <- Mdb.invalid_dbi;
    Txn.trivial Rw ?txn env @@ fun txn ->
    Mdb.drop txn dbi delete

  let get map ?txn k =
    Txn.trivial Ro
      ?txn:(txn :> [ `Read ] Txn.t option)
      (map.env :> [ `Read ] Env.t)
    @@ fun txn ->
    Mdb.get txn map.dbi (map.key.write Bigstring.create k)
    |> map.value.read

  module Flags = Mdb.PutFlags

  let put map ?txn ?(flags=Flags.none) k v =
    let key = map.key and value = map.value in
    if Conv.Flags.(test dup_sort map.flags)
    then begin
      let ka = key.write Bigstring.create k in
      let va = value.write Bigstring.create v in
      Txn.trivial Rw ?txn map.env @@ fun txn ->
      Mdb.put txn map.dbi ka va flags
    end
    else begin
      let ka = key.write Bigstring.create k in
      Txn.trivial Rw ?txn map.env @@ fun txn ->
      let va_opt = ref Mdb.Block_option.none in
      let alloc len =
        if Mdb.Block_option.is_some !va_opt then
          invalid_arg "Lmdb: converting function tried to allocate twice.";
        let va = Mdb.put_reserve txn map.dbi ka len flags in
        va_opt := Mdb.Block_option.some va;
        va
      in
      let va = value.write alloc v in
      if Mdb.Block_option.is_some !va_opt
      then begin
        if Mdb.Block_option.get_unsafe !va_opt != va then
          invalid_arg "Lmdb: converting function allocated, but returned different buffer."
      end
      else Mdb.put txn map.dbi ka va flags
    end

  let remove map ?txn ?value:v k =
    let key = map.key and value = map.value in
    let ka = key.write Bigstring.create k in
    let va = match v with
      | None -> Mdb.Block_option.none
      | Some v ->
        Mdb.Block_option.some @@ value.write Bigstring.create v
    in
    Txn.trivial Rw ?txn map.env @@ fun txn ->
    Mdb.del txn map.dbi ka va

  let compare_key map ?txn x y =
    let key = map.key in
    let xa = key.write Bigstring.create x in
    let ya = key.write Bigstring.create y in
    Txn.trivial Ro
      ?txn:(txn :> [ `Read ] Txn.t option)
      (map.env :> [ `Read ] Env.t)
    @@ fun txn ->
    Mdb.cmp txn map.dbi xa ya

  let compare_val map ?txn =
    if not Conv.Flags.(test dup_sort map.flags) then
      invalid_arg "Lmdb: elements are only comparable in a dup_sort map";
    let value = map.value in
    fun x y ->
    let xa = value.write Bigstring.create x in
    let ya = value.write Bigstring.create y in
    Txn.trivial Ro
      ?txn:(txn :> [ `Read ] Txn.t option)
      (map.env :> [ `Read ] Env.t)
    @@ fun txn ->
    Mdb.dcmp txn map.dbi xa ya

  let compare = compare_key
end

module Cursor = struct

  module Ops = Mdb.Ops

  module Flags = Mdb.PutFlags

  type ('k, 'v, -'perm, -'dup) t =
    { cursor: 'perm Mdb.cursor
    ; map: ('k, 'v, 'perm, 'dup) Map.t }
    constraint 'perm = [< `Read | `Write ]
    constraint 'dup = [< `Dup | `Uni ]

  exception Abort of Obj.t

  let go perm ?txn (map :_ Map.t) f =
    Txn.trivial perm map.env ?txn @@ fun t ->
    let cursor =
      { cursor = Mdb.cursor_open t map.dbi
      ; map = map }
    in
    try
      let res = f cursor in
      Mdb.cursor_close cursor.cursor;
      Some res
    with
    | Abort c when c == Obj.repr cursor ->
      Mdb.cursor_close cursor.cursor;
      if txn = None
      then (Mdb.txn_abort t; None)
      else invalid_arg "Lmdb.Cursor.abort: won't abort enclosing transaction."
    | exn ->
      Mdb.cursor_close cursor.cursor;
      raise exn

  let abort cursor = raise (Abort (Obj.repr cursor))

  (* Used internally for trivial functions, not exported. *)
  let trivial perm ?cursor (map :_ Map.t) f =
    match (cursor :_ t option) with
    | Some cursor ->
      if cursor.map != map
      then invalid_arg
          "Lmdb.Cursor.fold: Got cursor for wrong map";
      f cursor
    | None ->
      match go perm map f with
      | None -> assert false
      | Some x -> x

  let seek { cursor ; map } k =
    let key = map.key and value = map.value in
    let ka = key.write Bigstring.create k in
    let ka', va =
      Mdb.cursor_get cursor
        (Mdb.Block_option.some ka)
        Mdb.Block_option.none
        Ops.set
    in
    assert (ka' = ka);
    k, value.read va

  let get cursor k = snd @@ seek cursor k

  let seek_range { cursor ; map } k =
    let key = map.key and value = map.value in
    let ka, va =
      Mdb.cursor_get cursor
        (Mdb.Block_option.some (key.write Bigstring.create k))
        Mdb.Block_option.none
        Ops.set_range
    in
    key.read ka, value.read va

  let get_prim op { cursor ; map } =
    let key = map.key and value = map.value in
    let ka, va =
      Mdb.cursor_get cursor
        Mdb.Block_option.none Mdb.Block_option.none
        op
    in
    key.read ka, value.read va

  let current    c = get_prim Ops.get_current c
  let first      c = get_prim Ops.first c
  let last       c = get_prim Ops.last c
  let next       c = get_prim Ops.next c
  let prev       c = get_prim Ops.prev c
  let next_nodup c = get_prim Ops.next_nodup c
  let prev_nodup c = get_prim Ops.prev_nodup c

  let count { cursor; _ } = Mdb.cursor_count cursor

  let seek_dup { cursor ; map } k v =
    let key = map.key and value = map.value in
    let ka = key.write Bigstring.create k in
    let va = value.write Bigstring.create v in
    let ka', va' =
      Mdb.cursor_get
        cursor
        (Mdb.Block_option.some ka)
        (Mdb.Block_option.some va)
        Ops.get_both
    in
    assert (ka' = ka);
    assert (va' = va)

  let seek_range_dup { cursor ; map } k v =
    let key = map.key and value = map.value in
    let ka, va =
      Mdb.cursor_get cursor
        (Mdb.Block_option.some (key.write Bigstring.create k))
        (Mdb.Block_option.some (value.write Bigstring.create v))
        Ops.get_both_range
    in
    key.read ka, value.read va

  let get_dup_prim op { cursor ; map } =
    let value = map.value in
    let _, va =
      Mdb.cursor_get cursor
        Mdb.Block_option.none Mdb.Block_option.none
        op
    in
    value.read va

  let first_dup c = get_dup_prim Ops.first_dup c
  let last_dup  c = get_dup_prim Ops.last_dup c
  let next_dup  c = get_dup_prim Ops.next_dup c
  let prev_dup  c = get_dup_prim Ops.prev_dup c

  let cursor_none cursor = Mdb.cursor_get cursor.cursor
      Mdb.Block_option.none Mdb.Block_option.none

  let get_values_multiple cursor len =
    let value = cursor.map.value in
    assert Map.Conv.Flags.(test dup_fixed cursor.map.flags);
    let _, first = cursor_none cursor Ops.first_dup in
    let size = Bigstring.length first in
    let values = Array.make len (Obj.magic ()) in
    let _, buf = cursor_none cursor Ops.get_multiple in
    let rec convert buf off i =
      if off+size <= Bigstring.length buf
      then begin
        values.(i) <- value.read @@ Bigstring.sub buf ~off ~len:size;
        convert buf (off+size) (i+1)
      end
      else begin
        assert (off = Bigstring.length buf);
        i
      end
    in
    let i = convert buf 0 0 in
    let rec loop i =
      match
        try Some (cursor_none cursor Ops.next_multiple) with Not_found -> None
      with
      | None -> i
      | Some (_, buf) ->
        loop (convert buf 0 i);
    in
    let i = loop i in
    assert (i = len);
    values


  let get_values_from_first cursor first =
    if not Map.Conv.Flags.(test dup_sort cursor.map.flags)
    then [| first |]
    else begin
      let len = Mdb.cursor_count cursor.cursor in
      if len > 1 && Map.Conv.Flags.(test (dup_sort + dup_fixed) cursor.map.flags)
      then get_values_multiple cursor len
      else begin
        let values = Array.make len first in
        for i = 1 to len - 1 do
          values.(i) <- next_dup cursor
        done;
        values
      end
    end

  let get_values_from_last cursor last =
    if not Map.Conv.Flags.(test dup_sort cursor.map.flags)
    then [| last |]
    else begin
      let len = Mdb.cursor_count cursor.cursor in
      if len > 1 && Map.Conv.Flags.(test (dup_sort + dup_fixed) cursor.map.flags)
      then begin
        let values = get_values_multiple cursor len in
        cursor_none cursor Ops.first_dup |> ignore;
        values
      end
      else begin
        let values = Array.make len last in
        for i = len - 2 downto 0 do
          values.(i) <- prev_dup cursor
        done;
        values
      end
    end

  let get_all cursor k =
    let first = get cursor k in
    get_values_from_first cursor first

  let all_prim_from_first cursor f =
    let key, first = f cursor in
    key, get_values_from_first cursor first
  let all_prim_from_last cursor f =
    let key, first = f cursor in
    key, get_values_from_last cursor first

  let first_all c    = all_prim_from_first c first
  let next_all c     = all_prim_from_first c next_nodup
  let last_all c     = all_prim_from_last  c last
  let prev_all c     = all_prim_from_last  c prev_nodup
  let seek_all c k = all_prim_from_first c (fun c -> seek c k)
  let seek_range_all c k = all_prim_from_first c (fun c -> seek_range c k)
  let current_all c =
    first_dup c |> ignore;
    all_prim_from_first c current

  let put_raw_key { cursor ; map } ~flags ka v =
    let value = map.value in
    if Map.Conv.Flags.(test dup_sort map.flags)
    then begin
      let va = value.write Bigstring.create v in
      Mdb.cursor_put cursor ka va flags
    end
    else begin
      let va_opt = ref Mdb.Block_option.none in
      let alloc len =
        if Mdb.Block_option.is_some !va_opt then
          invalid_arg "Lmdb: converting function tried to allocate twice.";
        va_opt :=
          Mdb.Block_option.some @@
          Mdb.cursor_put_reserve cursor ka len flags;
        Mdb.Block_option.get_unsafe !va_opt
      in
      let va = value.write alloc v in
      if Mdb.Block_option.is_some !va_opt
      then begin
        if Mdb.Block_option.get_unsafe !va_opt != va then
          invalid_arg "Lmdb: converting function allocated, but returned different buffer."
      end
      else Mdb.cursor_put cursor ka va flags
    end

  let put cursor ?(flags=Flags.none) k v =
    let key = cursor.map.key in
    let ka = key.write Bigstring.create k in
    put_raw_key cursor ~flags ka v

  let replace cursor v =
    (* mdb_put mdb_current is supposed to replace the current _value_.
     * LMDB API documentation says the current key needs to be passed, too.
     * So first get the raw current key to pass it back in. *)
    let ka, _ =
      Mdb.cursor_get cursor.cursor
        Mdb.Block_option.none Mdb.Block_option.none
        Ops.get_current
    in
    put_raw_key cursor ~flags:Flags.current ka v

  let remove ?(all=false) cursor =
    Mdb.cursor_del cursor.cursor
      (if all then Flags.no_dup_data else Flags.none)

  let fold_prim_all init step get_all ?cursor ~f acc map =
    let fold cursor =
      match
        try Some (init cursor)
        with Not_found -> None
      with
      | None -> acc
      | Some (key,first) ->
        let values = get_all cursor first in
        let acc = f acc key values in
        let rec loop acc =
          match
            try Some (step cursor)
            with Not_found -> None
          with
          | None -> acc
          | Some (key,first) ->
            let values = get_all cursor first in
            let acc = f acc key values in
            loop acc
        in loop acc
    in
    trivial Ro
      ?cursor:(cursor :> (_, _, [ `Read ], 'dup) t option)
      (map :> (_, _, [ `Read ], 'dup) Map.t)
      fold

  let fold_left_all ?cursor ~f acc map =
    fold_prim_all first next_nodup get_values_from_first ?cursor ~f acc map

  let fold_right_all ?cursor ~f map acc =
    let f acc key values = f key values acc in
    fold_prim_all last prev_nodup get_values_from_last ?cursor ~f acc map

  let iter_all ?cursor ~f map =
    fold_left_all ?cursor () map ~f:(fun () -> f)

  (* The following two operations are not exposed, due to inherent unsafety:
     - Ops.get_multiple
     - Ops.next_multiple
  *)
end
