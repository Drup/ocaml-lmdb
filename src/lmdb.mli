(** OCaml binding for LMDB. *)

(** The {{:https://symas.com/products/lightning-memory-mapped-database/#overview}LMDB} database is a fast in-file database that supports ACID transactions.

    These bindings attempt to expose a typesafe yet low-overhead API.

    First, an environment must be opened using {!Env.create}:

    {[let env = Env.create "mydb" ]}

    Database implementations are specialized both by keys and values and answer the {!S} signature.
    Two module are predefined:
    - {!Db} uses [string] keys and [string] values.
    - {!IntDb} uses [int] keys and [string] values.

    Using {!Db}, we can open a new database and add our first value:
    {[
let db = Db.create ~create:true env "Camelidae" in
Db.put db "Bactrian camel" "Elegant and beautiful animal with two humps."
    ]}

    {{!Txn}Transactions} and {{!S.Cursor}Iterators} are also available.

    You can define new database implementations using the {!Make} functor.
*)


(** {2 Capabilities} *)

type 'cap cap
val ro : [ `Read ] cap
val rw : [ `Read | `Write ] cap

(** {2 Environments} *)

(** Operations on environment. *)
module Env : sig

  (** A DB environment supports multiple databases,
      all residing in the same shared-memory map.*)
  type -'cap t

  module Flags :  sig
    type t
    val ( + ) : t -> t -> t
    val test : t -> t -> bool
    val eq : t -> t -> bool
    val none : t

    val fixed_map : t
    val no_subdir : t
    val no_sync : t
    val no_meta_sync : t
    val write_map : t
    val map_async : t
    val no_tls : t
    val no_lock : t
    val no_read_ahead : t
    val no_mem_init : t
  end

  (** [create "/mydb"] create an environment in the directory [mydb/] and return an handle {Env.t}.
      The returned handle will be closed automatically when the OCaml handle
      is garbage collected.

      @param map_size Size of the memory map.
      @param max_readers Maximum number of threads/reader slots.
      @param max_dbs Maximum number of named databases.
      @param mode The UNIX permissions to set on created files and semaphores. Default is [0o755].
  *)
  val create :
    'cap cap -> ?max_readers:int -> ?map_size:int -> ?max_dbs:int ->
    ?flags:Flags.t -> ?mode:int -> string -> 'cap t

  val close: _ t -> unit

  val copy : ?compact:bool -> [> `Read ] t -> string -> unit

  val copyfd : ?compact:bool -> [> `Read ] t -> Unix.file_descr -> unit

  val set_flags : 'cap t -> Flags.t -> bool -> unit

  val flags : 'cap t -> Flags.t

  val set_map_size : [> `Write ] t -> int -> unit

  val path : 'cap t -> string

  val fd : 'cap t -> Unix.file_descr

  val sync : ?force:bool -> [> `Write ] t -> unit

  type stats = {
    psize : int;
    depth : int;
    branch_pages : int;
    leaf_pages : int;
    overflow_pages : int;
    entries : int;
  }

  val stats : [> `Read ] t -> stats

  val max_readers : 'cap t -> int

  val max_keysize : 'cap t -> int

  val reader_list : 'cap t -> string list

  val readers : 'cap t -> int

end

(** {2 Transactions} *)

(** A series of operations performed atomically. *)
module Txn : sig
  (** A transaction handle. A transaction may be read-only or read-write. *)
  type -'cap t

  (** [go cap env ?txn f] makes a transaction in [env] with the capabilities [cap]
      and using the function [f txn].

      The function [f] will receive the transaction handle [txn].
      All changes to the environment [env] done using the transaction handle [txn]
      will be persisted to the environment when [f] returns.
      After [f] returned, the transaction handle is invalid and should
      therefore not be leaked outside [f].

      @return [None] if the transaction was aborted with [abort], and [Some _] otherwise.
      @param txn Create a child transaction to [txn].
      This is not supported on an [env] with [write_map].

      Here is an example incrementing a value atomically:
      {[
go rw env begin fun t ->
let v = Map.get ~txn k in
Map.put ~txn k (v+1) ;
v
end
      ]}
  *)
  val go :
    'cap cap ->
    'cap Env.t ->
    ?txn:'cap t ->
    ('cap t -> 'a) -> 'a option

  (** [abort txn] will abort the transaction. *)
  val abort : 'cap t -> 'b


  (** {2 Misc} *)

  val env : 'cap t -> 'cap Env.t

end

(** {2 Databases} *)

(** Flags usable with the [put] operation. *)
module PutFlags : sig
  type t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val none : t

  val no_overwrite : t
  val no_dup_data : t
end

module Map : sig

  (** A handle for an individual database. *)
  type ('k, 'v, -'cap) t

  module Flags : sig
    type t
    val ( + ) : t -> t -> t
    val test : t -> t -> bool
    val eq : t -> t -> bool
    val none : t

    val reverse_key : t
    val dup_sort : t
    val dup_fixed : t
    val integer_dup : t
    val reverse_dup : t
    val integer_key : t
  end

  module Conv : sig
    type bigstring =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    type 'a t = ((int -> bigstring) -> 'a -> bigstring) * (bigstring -> 'a)

    val int : int t
    val int32_be : int t
    val int32_le : int t
    val int64_be : int t
    val int64_le : int t
    val string : string t
    val bigstring : bigstring t
  end

  type 'cap create_mode
  val new_db : [> `Read | `Write ] create_mode
  val existing_db : [> `Read ] create_mode

  (** [create env "foo"] open the database ["foo"] in the environment [env].

      @param create if [true], the database will be created if it doesn't
      exists. Invalid if [env] is read only.

      @raise Not_found if the database doesn't exist. and [create] wasn't [true].
  *)
  val create : 'cap create_mode ->
    conv_key: 'k Conv.t ->
    conv_val: 'v Conv.t ->
    ?flags:   Flags.t ->
    ?txn:     'cap Txn.t ->
    ?name:    string ->
    'cap Env.t ->
    ('k, 'v, 'cap) t

  val create_new :
    conv_key: 'k Conv.t ->
    conv_val: 'v Conv.t ->
    ?flags:   Flags.t ->
    ?txn:     'cap Txn.t ->
    ?name:    string ->
    ([> `Read | `Write ] as 'cap) Env.t ->
    ('k, 'v, 'cap) t

  val create_existing :
    conv_key: 'k Conv.t ->
    conv_val: 'v Conv.t ->
    ?flags:   Flags.t ->
    ?txn:     'cap Txn.t ->
    ?name:    string ->
    ([> `Read ] as 'cap) Env.t ->
    ('k, 'v, 'cap) t

  (** [get map k] returns the value associated to [k].
      @raise Not_found if the key is not in the database.
  *)
  val get : ('k, 'v, [> `Read ]) t -> ?txn:[> `Read ] Txn.t -> 'k -> 'v

  (** [put map k v] associates the key [k] to the value [v] in the database [map].

      @param flags Flags that allow to modify the behavior of [put].
      @raise Exists if the key is already in the database and
      [PutFlagse.no_overwrite] or [PutFlags.no_dup_data] was passed in [flags]
      or if the [map] does not support [Values.Flags.dup_sort].
  *)
  val put : ('k, 'v, [> `Read | `Write ]) t -> ?txn:[> `Read | `Write ] Txn.t -> ?flags:PutFlags.t -> 'k -> 'v -> unit

  (** [append map k v] like [put], but append [k, v] at the end of the database [map] without performing comparisons.

      Should only be used to quickly add already-sorted data to the database.

      @raise Error if a key is added that is smaller than the largest key already in the database.
  *)
  val append : ('k, 'v, [> `Read | `Write ]) t -> ?txn: [> `Read | `Write ] Txn.t -> ?flags:PutFlags.t -> 'k -> 'v -> unit

  (** [remove map k] removes [k] from [map].

      If the database accepts duplicates:
      @param v only the specified binding is removed. Otherwise,
      all the bindings with [k] are removed.

      @raise Not_found if the key is not in the database.
  *)
  val remove : ('k, 'v, [> `Read | `Write ]) t -> ?txn:([> `Read | `Write ]) Txn.t -> ?v:'v -> 'k -> unit


  (** {2 Misc} *)

  val stats : ?txn: [> `Read ] Txn.t -> ('k, 'v, [> `Read ]) t -> Env.stats

  (** [drop ?delete map] Empties [map].
      @param delete If [true] [map] is also deleted from the environment
      and the handle [map] invalidated. *)
  val drop : ?txn: [> `Write ] Txn.t -> ?delete:bool -> ('k, 'v, [> `Write ]) t -> unit

  (** [compare_key map ?txn a b]
     Compares [a] and [b] as if they were keys in [map]. *)
  val compare_key : ('k, 'v, 'cap) t -> ?txn:'cap' Txn.t -> 'k -> 'k -> int

  (** [compare map ?txn a b] Same as [compare_key]. *)
  val compare : ('k, 'v, 'cap) t -> ?txn:'cap' Txn.t -> 'k -> 'k -> int

  (** [compare_elt map ?txn a b]
     Compares [a] and [b] as if they were data elements in a [dup_sort] [map]. *)
  val compare_elt : ('k, 'v, 'cap) t -> ?txn:'cap' Txn.t -> 'v -> 'v -> int
end

(** Manual iterators. *)
module Cursor : sig

  (** A cursor allows to iterate manually on the database.
      A cursor may be read-only or read-write. *)
  type ('k, 'v, -'cap) t

  (** [go cap map ?txn f] makes a cursor in the transaction [txn] using the
      function [f cursor].

      The function [f] will receive the [cursor].
      A cursor can only be created and used inside a transaction. The cursor
      inherits the permissions of the transaction.
      The cursor should not be leaked outside of [f].

      Here is an example that returns the first 5 elements of a [map]:
      {[
go ro map begin fun c ->
let h = first c in
let rec aux i =
  if i < 5 then next c :: aux (i+1)
  else []
in
h :: aux 1
end
      ]}

      @param txn if not provided, a transaction will implicitely be created
      before calling [f] and be committed after [f] returns.
  *)
  val go : 'cap cap -> ('k, 'v, 'cap) Map.t -> ?txn:'cap Txn.t ->
    (('k, 'v, 'cap) t -> 'a) -> 'a

  (** [seek cursor k] moves the cursor to the key [k].
      [get] is another name for [seek]. *)
  val get : ('k, 'v, [> `Read ]) t -> 'k -> 'v

  (** [put cursor k v] adds [k,v] to the database and move the cursor to
      its position. *)
  val put : ('k, 'v, [> `Read | `Write ]) t -> ?flags:PutFlags.t -> 'k -> 'v -> unit

  (** [put_here cursor k v] adds [k,v] at the current position.

      @raise Error if the key provided is not the current key.
  *)
  val put_here : ('k, 'v, [> `Read | `Write ]) t -> ?flags:PutFlags.t -> 'k -> 'v -> unit

  (** [remove cursor] removes the current binding.

      If the database allow duplicate keys and if [all] is [true], removes
      all the bindings associated to the current key.
  *)
  val remove : ?all:bool -> ('k, 'v, [> `Read | `Write ]) t -> unit

  (** [get cursor] returns the binding at the position of the cursor. *)
  val current : ('k, 'v, [> `Read ]) t -> 'k * 'v

  (** [first cursor] moves the cursor to the first binding. *)
  val first : ('k, 'v, [> `Read ]) t -> 'k * 'v

  (** [last cursor] moves the cursor to the last binding. *)
  val last : ('k, 'v, [> `Read ]) t -> 'k * 'v

  (** [next cursor] moves the cursor to the next binding. *)
  val next : ('k, 'v, [> `Read ]) t -> 'k * 'v

  (** [prev cursor] moves the cursor to the prev binding. *)
  val prev : ('k, 'v, [> `Read ]) t -> 'k * 'v

  (** [seek cursor k] moves the cursor to the key [k]. *)
  val seek : ('k, 'v, [> `Read ]) t -> 'k -> 'v

  (** [seek_range cursor k] moves the cursor to the first key
      greater or equal to [k]. *)
  val seek_range : ('k, 'v, [> `Read ]) t -> 'k -> 'v

  (** {2 Operations on duplicated keys}

      Similar to the previous operations, but only inside a set of binding
      sharing the same key.

      Raise {!Invalid_argument} if used on a
      database that does not support duplicate keys.
  *)

  val first_dup : ('k, 'v, [> `Read ]) t -> 'k * 'v
  val last_dup : ('k, 'v, [> `Read ]) t -> 'k * 'v
  val next_dup : ('k, 'v, [> `Read ]) t -> 'k * 'v
  val prev_dup : ('k, 'v, [> `Read ]) t -> 'k * 'v

  val seek_dup : ('k, 'v, [> `Read ]) t -> 'k -> 'v
  val seek_range_dup : ('k, 'v, [> `Read ]) t -> 'k -> 'v
end

(** Main signature for a database module. *)
module type S = sig

  (** A handle for an individual database. *)
  type -'cap t

  (** The key of the database. *)
  type key

  (** The values of the database. *)
  type elt

  type 'cap create_mode = 'cap Map.create_mode
  val new_db : [> `Read | `Write ] create_mode
  val existing_db : [> `Read ] create_mode

  (** [create env "foo"] open the database ["foo"] in the environment [env].

      @param create if [true], the database will be created if it doesn't
      exists. Invalid if [env] is read only.

      @raise Not_found if the database doesn't exist. and [create] wasn't [true].
  *)
  val create : 'cap create_mode -> ?txn:'cap Txn.t -> ?name:string -> 'cap Env.t -> 'cap t
  val create_new : ?txn:'cap Txn.t -> ?name:string -> ([> `Read | `Write ] as 'cap) Env.t -> 'cap t
  val create_existing : ?txn:'cap Txn.t -> ?name:string -> ([> `Read ] as 'cap) Env.t -> 'cap t

  (** [get db k] returns the value associated to [k].
      @raise Not_found if the key is not in the database.
  *)
  val get : [> `Read ] t -> ?txn:[> `Read ] Txn.t -> key -> elt

  (** [put db k v] associates the key [k] to the value [v] in the database [db].

      @param flags Flags that allow to modify the behavior of [put].
      @raise Exists if the key is already in the database and
      [PutFlagse.no_overwrite] or [PutFlags.no_dup_data] was passed in [flags]
      or if the [db] does not support [Values.Flags.dup_sort].
  *)
  val put : [> `Read | `Write ] t -> ?txn:[> `Read | `Write ] Txn.t -> ?flags:PutFlags.t -> key -> elt -> unit

  (** [append db k v] like [put], but append [k, v] at the end of the database [db] without performing comparisons.

      Should only be used to quickly add already-sorted data to the database.

      @raise Error if a key is added that is smaller than the largest key already in the database.
  *)
  val append : [> `Read | `Write ] t -> ?txn: [> `Read | `Write ] Txn.t -> ?flags:PutFlags.t -> key -> elt -> unit

  (** [remove db k] removes [k] from [db].

      If the database accepts duplicates:
      - if [elt] is provided, only the specified binding is removed.
      - if [elt] is not provided, all the bindings with [k] are removed.

      @raise Not_found if the key is not in the database.
  *)
  val remove : [> `Read | `Write ] t -> ?txn:([> `Read | `Write ]) Txn.t -> ?elt:elt -> key -> unit


  (** Manual iterators. *)
  module Cursor : sig
    type -'cap db

    (** A cursor allows to iterates manually on the database.
        A cursor may be read-only or read-write. *)
    type -'cap t

    (** [go cap db ?txn f] makes a cursor in the transaction [txn] using the
       function [f cursor].

        The function [f] will receive the [cursor].
        A cursor can only be created and used inside a transaction. The cursor
        inherits the permissions of the transaction.
        The cursor should not be leaked outside of [f].

        Here is an example that returns the first 5 elements of a [db]:
        {[
go ro db begin fun c ->
  let h = first c in
  let rec aux i =
    if i < 5 then next c :: aux (i+1)
    else []
  in
  h :: aux 1
end
        ]}

        @param txn if not provided, a transaction will implicitely be created
        and committed after [f] returns.
    *)
    val go : 'cap cap -> 'cap db -> ?txn:'cap Txn.t -> ('cap t -> 'a) -> 'a

    (** [seek cursor k] moves the cursor to the key [k].
        [get] is another name for [seek]. *)
    val get : [> `Read ] t -> key -> elt

    (** [put cursor k v] adds [k,v] to the database and move the cursor to
        its position. *)
    val put : [> `Read | `Write ] t -> ?flags:PutFlags.t -> key -> elt -> unit

    (** [put_here cursor k v] adds [k,v] at the current position.

        @raise Error if the key provided is not the current key.
    *)
    val put_here : [> `Read | `Write ] t -> ?flags:PutFlags.t -> key -> elt -> unit

    (** [remove cursor] removes the current binding.

        If the database allow duplicate keys and if [all] is [true], removes
        all the bindings associated to the current key.
    *)
    val remove : [> `Read | `Write ] t -> ?all:bool -> unit -> unit

    (** [get cursor] returns the binding at the position of the cursor. *)
    val current : [> `Read ] t -> key * elt

    (** [first cursor] moves the cursor to the first binding. *)
    val first : [> `Read ] t -> key * elt

    (** [last cursor] moves the cursor to the last binding. *)
    val last : [> `Read ] t -> key * elt

    (** [next cursor] moves the cursor to the next binding. *)
    val next : [> `Read ] t -> key * elt

    (** [prev cursor] moves the cursor to the prev binding. *)
    val prev : [> `Read ] t -> key * elt

    (** [seek cursor k] moves the cursor to the key [k]. *)
    val seek : [> `Read ] t -> key -> elt

    (** [seek_range cursor k] moves the cursor to the first key
        greater or equal to [k]. *)
    val seek_range : [> `Read ] t -> key -> elt

    (** {2 Operations on duplicated keys}

        Similar to the previous operations, but only inside a set of binding
        sharing the same key.

        Raise {!Invalid_argument} if used on a
        database that does not support duplicate keys.
    *)

    val first_dup : [> `Read ] t -> key * elt
    val last_dup : [> `Read ] t -> key * elt
    val next_dup : [> `Read ] t -> key * elt
    val prev_dup : [> `Read ] t -> key * elt

    val seek_dup : [> `Read ] t -> key -> elt
    val seek_range_dup : [> `Read ] t -> key -> elt
  end with type -'cap db := 'cap t

  (** {2 Misc} *)

  val stats : ?txn: [> `Read ] Txn.t -> [> `Read ] t -> Env.stats

  (** [drop ?delete db] Empties [db].
      @param delete If [true] [db] is also deleted from the environment
      and the handle [db] invalidated. *)
  val drop : ?txn: [> `Write ] Txn.t -> ?delete:bool -> [> `Write ] t -> unit

  (** [compare_key db ?txn a b]
     Compares [a] and [b] as if they were keys in [db]. *)
  val compare_key : 'cap t -> ?txn:'cap' Txn.t -> key -> key -> int

  (** [compare db ?txn a b] Same as [compare_key]. *)
  val compare : 'cap t -> ?txn:'cap' Txn.t -> key -> key -> int

  (** [compare_elt db ?txn a b]
     Compares [a] and [b] as if they were data elements in a [dup_sort] [db]. *)
  val compare_elt : 'cap t -> ?txn:'cap' Txn.t -> elt -> elt -> int
end

(** Database with string keys and string elements. *)
module Db : S with type key = string and type elt = string

(** Database with integer keys and string elements. *)
module IntDb : S with type key = int and type elt = string

(** {2 Error reporting} *)

type error = int
(** Error return code. See Lmdb's documentation for details. *)

exception Exists
exception Not_found
exception Error of error
(** Errors are reported with those exceptions. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error Format.std_formatter e] will print a human-readable description
    of the given error.
*)

(** {2 Databases with custom datatypes} *)

module Values : sig

  module Flags : module type of Map.Flags

  type db_val = Map.Conv.bigstring

  module type S = sig
    type t
    val default_flags : Flags.t
    val read : db_val -> t
    val write : (int -> db_val) -> t -> db_val
  end

  module Key : sig
    module Int : S with type t = int
    module String : S with type t = string
  end

  module Elt : sig
    module Int : S with type t = int
    module String : S with type t = string
  end

end

module Make (Key : Values.S) (Elt : Values.S) :
  S with type key = Key.t
     and type elt = Elt.t

val version : unit -> string * int * int * int
