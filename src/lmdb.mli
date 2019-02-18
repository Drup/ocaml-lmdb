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


(** {2 Raw bindings} *)

module Mdb :module type of Lmdb_bindings

(** {2 Capabilities} *)

type 'a cap
val ro : [ `Read ] cap
val rw : [ `Read | `Write ] cap

(** {2 Environments} *)

(** Operations on environment. *)
module Env : sig

  (** A DB environment supports multiple databases,
      all residing in the same shared-memory map.*)
  type t

  module Flags :  sig
    type t
    val ( + ) : t -> t -> t
    val test : t -> t -> bool
    val eq : t -> t -> bool
    val none : t

    val fixed_map : t
    val no_subdir : t
    val no_sync : t
    val read_only : t
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
    ?max_readers:int -> ?map_size:int -> ?max_dbs:int ->
    ?flags:Flags.t -> ?mode:int -> string -> t

  val close : t -> unit

  val copy : ?compact:bool -> t -> string -> unit

  val copyfd : ?compact:bool -> t -> Unix.file_descr -> unit

  val set_flags : t -> Flags.t -> bool -> unit

  val flags : t -> Flags.t

  val set_map_size : t -> int -> unit

  val path : t -> string

  val fd : t -> Unix.file_descr

  val sync : ?force:bool -> t -> unit

  val stats : t -> Mdb.stats

  val max_readers : t -> int

  val max_keysize : t -> int

  val reader_list : t -> string list

  val reader_check : t -> int

end

(** {2 Transactions} *)

(** A series of operations performed atomically. *)
module Txn : sig
  (** A transaction handle. A transaction may be read-only or read-write. *)
  type -'a t constraint 'a = [< `Read | `Write ]

  (** [go cap env ?txn f] makes a transaction in [env] with the capabilities [cap]
      and using the function [f txn].

      The function [f] will receive the transaction handle [txn].
      All changes to the environment [env] done using the transaction handle [txn]
      will be persisted to the environment when [f] returns.
      After [f] returned, the transaction handle is invalid and should
      therefore not be leaked outside [f].

      @return [None] if the transaction was aborted with [abort], and [Some _] otherwise.
      @param txn Create a child transaction to [txn].

      Here is an example incrementing a value atomically:
      {[
go rw env begin fun t ->
let v = Db.get ~txn k in
Db.put ~txn k (v+1) ;
v
end
      ]}
  *)
  val go :
    'a cap ->
    ?txn:([< `Read | `Write ] as 'a) t ->
    Env.t ->
    ('a t -> 'b) -> 'b option

  (** [abort txn] will abort the transaction. *)
  val abort : 'a t -> 'b


  (** {2 Misc} *)

  val env : 'a t -> Env.t

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

(** Main signature for a database module. *)
module type S = sig

  (** A handle for an individual database. *)
  type t

  (** The key of the database. *)
  type key

  (** The values of the database. *)
  type elt

  (** [create env "foo"] open the database ["foo"] in the environment [env].

      @param create if [true], the database will be created if it doesn't
      exists. Invalid if [env] is read only.

      @raise Not_found if the database doesn't exist. and [create] wasn't [true].
  *)
  val create : ?create:bool -> Env.t -> string -> t

  (** [get db k] returns the value associated to [k].
      @raise Not_found if the key is not in the database.
  *)
  val get : t -> ?txn:[> `Read ] Txn.t -> key -> elt

  (** [put db k v] associates the key [k] to the value [v] in the database [db].

      @param flags Flags that allow to modify the behavior of [put].
      @raise Exists if the key is already in the database and
      [PutFlagse.no_overwrite] or [PutFlags.no_dup_data] was passed in [flags]
      or if the [db] does not support [Values.Flags.dup_sort].
  *)
  val put : t -> ?txn:([> `Read | `Write ]) Txn.t -> ?flags:PutFlags.t -> key -> elt -> unit

  (** [append db k v] like [put], but append [k, v] at the end of the database [db] without performing comparisons.

      Should only be used to quickly add already-sorted data to the database.

      @raise Error if a key is added that is smaller than the largest key already in the database.
  *)
  val append : t -> ?txn:([> `Read | `Write ]) Txn.t -> ?flags:PutFlags.t -> key -> elt -> unit

  (** [remove db k] removes [k] from [db].

      If the database accepts duplicates:
      - if [elt] is provided, only the specified binding is removed.
      - if [elt] is not provided, all the bindings with [k] are removed.

      @raise Not_found if the key is not in the database.
  *)
  val remove : t -> ?txn:([> `Read | `Write ]) Txn.t -> ?elt:elt -> key -> unit


  (** Manual iterators. *)
  module Cursor : sig
    type db

    (** A cursor allows to iterates manually on the database.
        A cursor may be read-only or read-write. *)
    type -'a t constraint 'a = [< `Read | `Write ]

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
    val go : 'c cap -> ?txn:'c Txn.t -> db -> ('c t -> 'a) -> 'a

    (** [get cursor] returns the binding at the position of the cursor. *)
    val get : [> `Read ] t -> key * elt

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
  end with type db := t

  (** {2 Misc} *)

  val stats : t -> Mdb.stats

  (** [drop ?delete db] Empties [db].
      @param delete If [true] [db] is also deleted from the environment
      and the handle [db] invalidated. *)
  val drop : ?delete:bool -> t -> unit

  (** [compare_key db ?txn a b]
     Compares [a] and [b] as if they were keys in [db]. *)
  val compare_key : t -> ?txn:[> `Read ] Txn.t -> key -> key -> int

  (** [compare db ?txn a b] Same as [compare_key]. *)
  val compare : t -> ?txn:[> `Read ] Txn.t -> key -> key -> int

  (** [compare_elt db ?txn a b]
     Compares [a] and [b] as if they were data elements in a [dup_sort] [db]. *)
  val compare_elt : t -> ?txn:[> `Read ] Txn.t -> elt -> elt -> int
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

  type db_val =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

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

val version : string * int * int * int
