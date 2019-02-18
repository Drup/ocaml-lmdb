exception Exists
exception Error of int

(* return codes *)
external strerror : int -> string = "mdbs_strerror"

(* Initialise constants and exceptions *)
external init : unit -> ((string * int * int * int) * int array)
  = "mdbs_init"
let [@ocaml.warning "-8"]
  ( version
  , [| append
     ; appenddup
     ; cp_compact
     ; create
     ; current
     ; dupfixed
     ; dupsort
     ; first
     ; first_dup
     ; fixedmap
     ; get_both
     ; get_both_range
     ; get_current
     ; get_multiple
     ; integerdup
     ; integerkey
     ; last
     ; last_dup
     ; mapasync
     ; multiple
     ; next
     ; next_dup
     ; next_multiple
     ; next_nodup
     ; nodupdata
     ; nolock
     ; nomeminit
     ; nometasync
     ; nooverwrite
     ; nordahead
     ; nosubdir
     ; nosync
     ; notls
     ; prev
     ; prev_dup
  (* ; prev_multiple - only since lmdb 0.9.19 *)
     ; prev_nodup
     ; rdonly
     ; reserve
     ; reversedup
     ; reversekey
     ; set
     ; set_key
     ; set_range
     ; writemap
     ; sizeof_int
     ; sizeof_size_t
    |] )
  =
  Callback.register_exception "LmdbExists" Exists;
  Callback.register_exception "LmdbError" (Error 0);
  Printexc.register_printer @@ begin function
    | Error i -> Some ("Lmdb.Error(" ^ strerror i ^ ")")
    | Exists -> Some "Lmdb.Exists"
    | _ -> None
  end;
  init ()

module type Flags = sig
  type t
  external ( + ) : t -> t -> t = "%orint"
  external ( * ) : t -> t -> t = "%andint"
  val test : t -> t -> bool
  external eq : t -> t -> bool = "%equal"
  val none : t
end
module Flags :Flags with type t = int = struct
  type t = int
  external ( + ) : t -> t -> t = "%orint"
  external ( * ) : t -> t -> t = "%andint"
  let test f m = f * m = f
  external eq : t -> t -> bool = "%equal"
  let none :t = 0
end

(* stats returned by env_stat and dbi_stat *)
type stats =
  { psize : int
  ; depth : int
  ; branch_pages : int
  ; leaf_pages : int
  ; overflow_pages : int
  ; entries : int
  }

(* bigstring *)
module Bigstring = Bigstringaf

(* env *)
type -'perm env constraint 'perm = [< `Read | `Write ]
module EnvFlags = struct
  include Flags
  let fixed_map       = fixedmap
  let no_subdir       = nosubdir
  let no_sync         = nosync
  let read_only       = rdonly
  let no_meta_sync    = nometasync
  let write_map       = writemap
  let map_async       = mapasync
  let no_tls          = notls
  let no_lock         = nolock
  let no_read_ahead   = nordahead
  let no_mem_init     = nomeminit
end
module CopyFlags = struct
  include Flags
  let compact         = cp_compact
end
external env_create : unit -> _ env
  = "mdbs_env_create"
external env_open : _ env -> string -> EnvFlags.t -> int -> unit
  = "mdbs_env_open"
external env_close : _ env -> unit
  = "mdbs_env_close"
external env_set_mapsize : _ env -> int -> unit
  = "mdbs_env_set_mapsize"
external env_set_maxdbs : _ env -> int -> unit
  = "mdbs_env_set_maxdbs"
external env_set_maxreaders : _ env -> int -> unit
  = "mdbs_env_set_maxreaders"
external env_copy : _ env -> string -> CopyFlags.t -> unit
  = "mdbs_env_copy2"
external env_copyfd : _ env -> Unix.file_descr -> CopyFlags.t -> unit
  = "mdbs_env_copyfd2"
external env_set_flags : _ env -> EnvFlags.t -> bool -> unit
  = "mdbs_env_set_flags"
external env_get_flags : _ env -> int
  = "mdbs_env_get_flags"
external env_get_path : _ env -> string
  = "mdbs_env_get_path"
external env_get_fd : _ env -> Unix.file_descr
  = "mdbs_env_get_fd"
external env_sync : _ env -> bool -> unit
  = "mdbs_env_sync"
external env_get_maxreaders : _ env -> int
  = "mdbs_env_get_maxreaders"
external env_get_maxkeysize : _ env -> int
  = "mdbs_env_get_maxkeysize"
external reader_list : _ env -> (string -> int) -> int
  = "mdbs_reader_list"
external reader_check : _ env -> int
  = "mdbs_reader_check"
external env_stat : _ env -> stats
  = "mdbs_env_stat"

(* txn *)
type -'perm txn constraint 'perm = [< `Read | `Write ]
external txn_env : 'perm txn -> 'perm env
  = "mdbs_txn_env"
external txn_begin : 'perm env -> 'perm txn option -> EnvFlags.t -> 'perm txn
  = "mdbs_txn_begin"
external txn_commit : _ txn -> unit
  = "mdbs_txn_commit"
external txn_abort : _ txn -> unit
  = "mdbs_txn_abort"

(* dbi *)
type dbi
let invalid_dbi :dbi = Obj.magic ~-1
module DbiFlags = struct
  include Flags
  let reverse_key	= reversekey
  let dup_sort   	= dupsort
  let integer_key	= integerkey
  let dup_fixed  	= dupfixed
  let integer_dup	= integerdup
  let reverse_dup     = reversedup
  let create          = create
end
module PutFlags = struct
  include Flags
  let no_overwrite    = nooverwrite
  let no_dup_data     = nodupdata
  let current         = current
  let _reserve        = reserve
  let append          = append
  let append_dup      = appenddup
  let _multiple       = multiple
end
module Block_option = struct
  type +'a t
  let none :_ t = Obj.magic None
  external some_unsafe : 'a -> 'a t = "%identity"
  external get_unsafe  : 'a t -> 'a = "%identity"
  let is_some o = Obj.(is_block (repr o))
  let _is_none o = not (is_some o)
  let some x = assert (is_some x); some_unsafe x
  let _get_exn o =
    if is_some o
    then get_unsafe o
    else raise Not_found
end
external dbi_open
  : 'perm txn -> string option -> Flags.t -> dbi
  = "mdbs_dbi_open"
external dbi_close : _ env -> dbi -> unit
  = "mdbs_dbi_close"
external dbi_flags : [> `Read ] txn -> dbi -> Flags.t
  = "mdbs_dbi_flags"
external dbi_stat : [> `Read ] txn -> dbi -> stats
  = "mdbs_stat"
external drop : [> `Write ] txn -> dbi -> bool -> unit
  = "mdbs_drop"
external get
  : [> `Read ] txn -> dbi -> Bigstring.t -> Bigstring.t
  = "mdbs_get"
external put
  : [> `Read | `Write ] txn -> dbi -> Bigstring.t -> Bigstring.t ->
    PutFlags.t -> unit
  = "mdbs_put"
external put_reserve
  : [> `Read | `Write ] txn -> dbi -> Bigstring.t -> int ->
    PutFlags.t -> Bigstring.t
  = "mdbs_put"
external del
  : [> `Read | `Write ] txn -> dbi ->
    Bigstring.t -> Bigstring.t Block_option.t -> unit
  = "mdbs_del"
external cmp
  : _ txn -> dbi -> Bigstring.t -> Bigstring.t -> int
  = "mdbs_cmp"
external dcmp
  : _ txn -> dbi -> Bigstring.t -> Bigstring.t -> int
  = "mdbs_dcmp"

(* cursor *)
type -'perm cursor constraint 'perm = [< `Read | `Write ]
module Ops = struct
  type t = int
  let first           = first
  let first_dup       = first_dup
  let get_both        = get_both
  let get_both_range  = get_both_range
  let get_current     = get_current
  let get_multiple    = get_multiple
  let last            = last
  let last_dup        = last_dup
  let next            = next
  let next_dup        = next_dup
  let next_multiple   = next_multiple
  let next_nodup      = next_nodup
  let prev            = prev
  let prev_dup        = prev_dup
  let prev_nodup      = prev_nodup
  let set             = set
  let _set_key        = set_key
  let set_range       = set_range
  (* let _prev_multiple  = prev_multiple - only since lmdb 0.9.19 *)
end
external cursor_open : 'perm txn -> dbi -> 'perm cursor
  = "mdbs_cursor_open"
external cursor_close : 'perm cursor -> unit
  = "mdbs_cursor_close"
external cursor_put
  : [> `Read | `Write ] cursor -> Bigstring.t -> Bigstring.t ->
    PutFlags.t -> unit
  = "mdbs_cursor_put"
external cursor_put_reserve
  : [> `Read | `Write ] cursor -> Bigstring.t -> int ->
    PutFlags.t -> Bigstring.t
  = "mdbs_cursor_put"
external cursor_del
  : [> `Read | `Write ] cursor -> PutFlags.t -> unit
  = "mdbs_cursor_del"
external cursor_get
  : [> `Read ] cursor ->
    Bigstring.t Block_option.t -> Bigstring.t Block_option.t -> Ops.t ->
    (Bigstring.t * Bigstring.t)
  = "mdbs_cursor_get"
external cursor_count : [> `Read ] cursor -> int
  = "mdbs_cursor_count"
