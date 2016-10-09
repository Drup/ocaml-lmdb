(** OCaml binding for LMDB. *)

(** {2 Misc} *)

type stats = {
  psize : int;
  depth : int;
  branch_pages : int;
  leaf_pages : int;
  overflow_pages : int;
  entries : int;
}

val version : unit -> string * int * int * int

type env

(** Operations on environment. *)
module Env : sig

  exception Assert of (env * string)

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
    ?flags:Flags.t -> ?mode:int -> string -> env

  val copy : ?compact:bool -> env -> string -> unit

  val copyfd : ?compact:bool -> env -> Unix.file_descr -> unit

  val stats : env -> stats

  val set_flags : env -> Flags.t -> bool -> unit

  val flags : env -> Flags.t

  val path : env -> string

  val fd : env -> Unix.file_descr

  val max_readers : env -> int

  val max_keysize : env -> int

  val reader_list : env -> string list

  val readers : env -> int

end


module PutFlags : sig
  type t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val none : t

  val nooverwrite : t
  val nodupdata : t
  val current : t
  val append : t
  val appenddup : t
  val multiple : t
end


module Flags : sig
  type t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val none : t

  val reversekey : t
  val dupsort : t
  val dupfixed : t
  val integerdup : t
  val reversedup : t
end


type db_val

module Element : sig
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

  module Val : sig
    module Int : S with type t = int
    module String : S with type t = string
  end

end

module Make (Key : Element.S) (Val : Element.S) : sig

  type db

  type key = Key.t
  type element = Val.t

  val create : ?create:bool -> ?name:string -> ?flags:Flags.t -> env -> db

  val stats : db -> stats
  val flags : db -> Flags.t
  val drop : ?delete:bool -> db -> unit
  val get : db -> Key.t -> Val.t
  val put : ?flags:PutFlags.t -> db -> Key.t -> Val.t -> unit
  val del : ?v:Val.t -> db -> Key.t -> unit

  val compare : db -> Key.t -> Key.t -> int
  (** The comparison function used by the database. *)

  module Txn : sig

    type 'a txn constraint 'a = [< `Read | `Write ]

    val go :
      ?parent:([< `Read | `Write ] as 'a) txn ->
      rw:'a ->
      db ->
      ('a txn -> [< `Abort | `Ok of 'b ]) -> 'b option

    val abort : 'a txn -> 'b

    val stats : 'a txn -> stats
    val flags : 'a txn -> Flags.t

    val compare : 'a txn -> Key.t -> Key.t -> int
    (** The comparison function used by the database. *)

    val drop : ?delete:bool -> [< `Write ] txn -> unit

    val get : 'a txn -> Key.t -> Val.t
    val put : ?flags:PutFlags.t -> [> `Write ] txn -> Key.t -> Val.t -> unit
    val del : ?v:Val.t -> [> `Write ] txn -> Key.t -> unit

    val env : 'a txn -> env

  end
end


module Db : module type of Make (Element.Key.String) (Element.Val.String)
module IntDb : module type of Make (Element.Key.Int) (Element.Val.String)
