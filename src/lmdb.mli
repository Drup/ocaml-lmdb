(** OCaml binding for LMDB. *)

(** {2 Error reporting} *)

type error = int
(** Error return code. See Lmdb's documentation for details. *)

exception Error of error
(** Error are reported with this exception. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error Format.std_formatter e] will print a human-readable description
    of the given error.
*)

(** Operations on environment. *)
module Env : sig

  type t

  module Flags :  sig
    type t
    val ( + ) : t -> t -> t
    val test : t -> t -> bool
    val eq : t -> t -> bool
    val none : t

    val fixedmap : t
    val nosubdir : t
    val nosync : t
    val rdonly : t
    val nometasync : t
    val writemap : t
    val mapasync : t
    val notls : t
    val nolock : t
    val nordahead : t
    val nomeminit : t
  end

  val create :
    ?maxreaders:int -> ?mapsize:int -> ?maxdbs:int ->
    ?flags:Flags.t -> ?mode:int -> string -> t

  val copy : ?compact:bool -> t -> string -> unit

  val copyfd : ?compact:bool -> t -> Unix.file_descr -> unit

  val set_flags : t -> Flags.t -> bool -> unit

  val flags : t -> Flags.t

  val path : t -> string

  val fd : t -> Unix.file_descr

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


module PutFlags : sig
  type t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val none : t

  val no_overwrite : t
  val no_dup_data : t
  val current : t
  val append : t
  val append_dup : t
  val multiple : t
end



module type S = sig

  type t

  type key
  type elt

  val create : ?create:bool -> ?name:string -> Env.t -> t

  val stats : t -> Env.stats
  val drop : ?delete:bool -> t -> unit
  val get : t -> key -> elt
  val put : ?flags:PutFlags.t -> t -> key -> elt -> unit
  val del : ?elt:elt -> t -> key -> unit

  val compare : t -> key -> key -> int
  (** The comparison function used by the database. *)

  module Txn : sig

    type 'a txn constraint 'a = [< `Read | `Write ]

    val go :
      ?parent:([< `Read | `Write ] as 'a) txn ->
      rw:'a ->
      t ->
      ('a txn -> [< `Abort | `Ok of 'b ]) -> 'b option

    val abort : 'a txn -> 'b

    val stats : 'a txn -> Env.stats

    val compare : 'a txn -> key -> key -> int
    (** The comparison function used by the database. *)

    val drop : ?delete:bool -> [< `Write ] txn -> unit

    val get : 'a txn -> key -> elt
    val put : ?flags:PutFlags.t -> [> `Write ] txn -> key -> elt -> unit
    val del : ?elt:elt -> [> `Write ] txn -> key -> unit

    val env : 'a txn -> Env.t

  end

  module Cursor : sig

    type 'a t constraint 'a = [< `Read | `Write ]

    val go : 'cap Txn.txn -> f:('cap t -> 'a) -> 'a

    val get : _ t -> key * elt
    val put : ?flags:PutFlags.t -> [> `Write ] t -> key -> elt -> unit
    val del : ?all:bool -> [> `Write ] t -> unit

    val first : _ t -> key * elt
    val last : _ t -> key * elt
    val next : _ t -> key * elt
    val prev : _ t -> key * elt

    val seek : _ t -> key -> elt
    val seek_range : _ t -> key -> elt

    (** {2 Operations on duplicated keys}

        The following function raise {!Invalid_argument} if they are used on a
        database that was not created with {!Values.Flags.dup_sort}. *)

    val first_dup : _ t -> key * elt
    val last_dup : _ t -> key * elt
    val next_dup : _ t -> key * elt
    val prev_dup : _ t -> key * elt

    val seek_dup : _ t -> key -> elt
    val seek_range_dup : _ t -> key -> elt
  end
end


module Db : S with type key = string and type elt = string
module IntDb : S with type key = int and type elt = string

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
