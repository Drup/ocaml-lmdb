(** OCaml binding for LMDB. *)

(** The {{:https://symas.com/products/lightning-memory-mapped-database/#overview}LMDB} database is a fast in-file database that supports ACID transactions.

    These bindings attempts to expose a typesafe yet low-overhead API.

    First, an environment must be open using {!Env.create}:

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

    {{!S.Txn}Transactions} and {{!S.Cursor}Iterators} are also available.

    You can define new database implementations using the {!Make} functor.
*)


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

  (** [create "/mydb"] create an environment in the directory [mydb/].

      @param map_size Size of the memory map.
      @param max_readers Maximum number of threads/reader slots.
      @param max_dbs Maximum number of named database.
      @param mode The UNIX permissions to set on created files and semaphores. Default is [0o755].
  *)
  val create :
    ?max_readers:int -> ?map_size:int -> ?max_dbs:int ->
    ?flags:Flags.t -> ?mode:int -> string -> t

  val copy : ?compact:bool -> t -> string -> unit

  val copyfd : ?compact:bool -> t -> Unix.file_descr -> unit

  val set_flags : t -> Flags.t -> bool -> unit

  val flags : t -> Flags.t

  val path : t -> string

  val fd : t -> Unix.file_descr

  val sync : ?force:bool -> t -> unit

  type stats = {
    psize : int;
    depth : int;
    branch_pages : int;
    leaf_pages : int;
    overflow_pages : int;
    entries : int;
  }

  val stats : t -> stats

  val max_readers : t -> int

  val max_keysize : t -> int

  val reader_list : t -> string list

  val readers : t -> int

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

      If [create] is set to [true], the database will be created if it doesn't
      exists. Invalid if [env] is read only.

      @raise Not_found if the database doesn't exist.
  *)
  val create : ?create:bool -> Env.t -> string -> t

  (** [get db k] returns the value associated to [k].
      @raise Not_found if the key is not in the database.
  *)
  val get : t -> key -> elt

  (** [put db k v] associates the key [k] to the value [v] in the database [db].

      @param flags Flags that allow to modify the behavior of [put].
  *)
  val put : ?flags:PutFlags.t -> t -> key -> elt -> unit

  (** [append db k v] append [k, v] at the end of the database [db] without performing comparisons.

      Should only be used to quickly add already-sorted data to the database.
  *)
  val append : t -> key -> elt -> unit

  (** [remove db k] removes [k] from [db].

      If the database accepts duplicates:
      - if [elt] is provided, only the specified binding is removed.
      - if [elt] is not provided, all the bindings with [k] are removed.
  *)
  val remove : ?elt:elt -> t -> key -> unit

  (** A series of operation performed atomically. *)
  module Txn : sig

    (** A transaction handle. A transaction may be read-only or read-write. *)
    type 'a txn constraint 'a = [< `Read | `Write ]

    (** [go ~rw db f] makes a transaction in [db] with the permission [rw]
        and using the function [f].

        The function [f] will receive the transaction handle.
        All the operations called using the {!Txn} module will be executed when
        [f] returns.
        The transaction handle should not be leaked outside of [f].

        Return [None] if the transaction was aborted with [abort], and [Some v] otherwise.

        Here is an example incrementing a value atomically:
        {[
go ~rw:`Write db begin fun txn ->
  let v = get k in
  put k (v+1) ;
  v
end
        ]}
    *)
    val go :
      ?parent:([< `Read | `Write ] as 'a) txn ->
      rw:'a ->
      t ->
      ('a txn -> 'b) -> 'b option

    (** [abort txn] will abort the transaction. *)
    val abort : 'a txn -> 'b

    (** [get txn k] returns the value associated to [k].
        @raise Not_found if the key is not in the database.
    *)
    val get : 'a txn -> key -> elt

    (** [put txn k v] associates the key [k] to the value [v].

        @param flags Flags that allow to modify the behavior of [put].
    *)
    val put : ?flags:PutFlags.t -> [> `Write ] txn -> key -> elt -> unit

    (** [append txn k v] append [k, v] at the end of the database without performing comparisons.

        Should only be used to quickly add already-sorted data to the database.
    *)
    val append : [> `Write] txn -> key -> elt -> unit

    (** [remove txn k] removes [k] from the database.

        If the database accepts duplicates:
        - if [elt] is provided, only the specified binding is removed.
        - if [elt] is not provided, all the bindings with [k] are removed.
    *)
    val remove : ?elt:elt -> [> `Write ] txn -> key -> unit


    (** {2 Misc} *)

    val env : 'a txn -> Env.t

    val stats : 'a txn -> Env.stats

    val compare : 'a txn -> key -> key -> int
    (** The comparison function used by the database. *)

    val drop : ?delete:bool -> [< `Write ] txn -> unit

  end

  (** Manual iterators. *)
  module Cursor : sig

    (** A cursor allows to iterates manually on the database.
        A cursor may be read-only or read-write. *)
    type 'a t constraint 'a = [< `Read | `Write ]

    (** [go txn f] makes a cursor in the transaction [txn] using the function [f].

        The function [f] will receive the cursor.
        A cursor can only be create and used inside a transaction. The cursor
        inherits the permissions of the transaction.
        The cursor should not be leaked outside of [f].

        Here is an example that returns the first 5 elements of a database:
        {[
go txn begin fun c ->
  let h = first c in
  let rec aux i =
    if i < 5 then next c :: aux (i+1)
    else []
  in
  h :: aux 1
end
        ]}
    *)
    val go : 'cap Txn.txn -> f:('cap t -> 'a) -> 'a

    (** [get cursor] returns the binding at the position of the cursor. *)
    val get : _ t -> key * elt

    (** [put cursor k v] adds [k,v] to the database and move the cursor to
        its position. *)
    val put : ?flags:PutFlags.t -> [> `Write ] t -> key -> elt -> unit

    (** [put_here cursor k v] adds [k,v] at the current position.

        @raise Error if the key provided is not the current key.
    *)
    val put_here : ?flags:PutFlags.t -> [> `Write ] t -> key -> elt -> unit

    (** [remove cursor] removes the current binding.

        If the database allow duplicate keys and if [all] is [true], removes
        all the bindings associated to the current key.
    *)
    val remove : ?all:bool -> [> `Write ] t -> unit

    (** [first cursor] moves the cursor to the first binding. *)
    val first : _ t -> key * elt

    (** [last cursor] moves the cursor to the last binding. *)
    val last : _ t -> key * elt

    (** [next cursor] moves the cursor to the next binding. *)
    val next : _ t -> key * elt

    (** [prev cursor] moves the cursor to the prev binding. *)
    val prev : _ t -> key * elt

    (** [seek cursor k] moves the cursor to the key [k]. *)
    val seek : _ t -> key -> elt

    (** [seek_range cursor k] moves the cursor to the first key
        greater or equal to [k]. *)
    val seek_range : _ t -> key -> elt

    (** {2 Operations on duplicated keys}

        Similar to the previous operations, but only inside a set of binding
        sharing the same key.

        Raise {!Invalid_argument} if used on a
        database that does not support duplicate keys.
    *)

    val first_dup : _ t -> key * elt
    val last_dup : _ t -> key * elt
    val next_dup : _ t -> key * elt
    val prev_dup : _ t -> key * elt

    val seek_dup : _ t -> key -> elt
    val seek_range_dup : _ t -> key -> elt
  end

  (** {2 Misc} *)

  val stats : t -> Env.stats

  val drop : ?delete:bool -> t -> unit

  val compare : t -> key -> key -> int
  (** The comparison function used by the database. *)
end

(** Database with string keys and string elements. *)
module Db : S with type key = string and type elt = string

(** Database with integer keys and string elements. *)
module IntDb : S with type key = int and type elt = string

(** {2 Error reporting} *)

type error = int
(** Error return code. See Lmdb's documentation for details. *)

exception Error of error
(** Error are reported with this exception or with {!Not_found}. *)

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

  type db_val

  module type S = sig
    type t
    val default_flags : Flags.t
    val read : db_val -> t
    val write : t -> db_val
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
