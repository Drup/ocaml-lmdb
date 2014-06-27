
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

module Env : sig

    exception Assert of (env * string)

    module Flags :  sig
        type t
        val ( + ) : t -> t -> t
        val test : t -> t -> bool
        val eq : t -> t -> bool
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
      ?maxreaders:int ->
      ?mapsize:int -> ?maxdbs:int -> ?flags:Flags.t -> string -> env

    val copy : env -> string -> unit

    val copyfd : env -> Unix.file_descr -> unit

    val stats : env -> stats

    val set_flags : env -> Flags.t -> bool -> unit

    val flags : env -> Flags.t

    val path : env -> string

    val fd : env -> Unix.file_descr

    val max_readers : env -> int

    val max_keysize : env -> int

    val reader_list : env -> string list

    val readers : env -> int

    (* val of_transaction : Internal.mdb_txn -> env *)

end


module PutFlags : sig
  type t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val nooverwrite : t
  val nodupdata : t
  val current : t
  val reserve : t
  val append : t
  val appenddup : t
  val multiple : t
end


module Flags : sig
  type t
  val i : int -> t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val reversekey : t
  val dupsort : t
  val dupfixed : t
  val integerdup : t
  val reversedup : t
end


type db_val

module type KEY = sig
  type t
  val default_flags : Flags.t
  val write : t -> db_val
end

module type VAL = sig
  type t
  val default_flags : Flags.t
  val read : db_val -> t
  val write : t -> db_val
end

module KeyInt : KEY with type t = int

module ValString : VAL with type t = string

module Make (Key : KEY) (Val : VAL) : sig

  type db

  val create : ?create:bool -> ?name:string -> ?flags:Flags.t -> env -> db

  val stats : db -> stats
  val flags : db -> Flags.t
  val drop : ?delete:bool -> db -> unit
  val get : db -> Key.t -> Val.t
  val put : ?flags:Flags.t -> db -> Key.t -> Val.t -> unit
  val del : ?v:Val.t -> db -> Key.t -> unit

  module Txn : sig

    type 'a txn constraint 'a = [< `Read | `Write ]

    val go :
      ?parent:'b txn -> db ->
      ([ `Read ] txn -> [< `Abort | `Ok of 'a ]) -> 'a option

    val gow :
      ?parent:[> `Write] txn -> db ->
      ([ `Read | `Write ] txn -> [< `Abort | `Ok of 'a ]) -> 'a option

    val abort : 'a txn -> 'b

    val stats : [> `Read ] txn -> stats
    val flags : [> `Read ] txn -> Flags.t
    val drop : ?delete:bool -> [< `Write ] txn -> unit

    val get : [> `Read ] txn -> Key.t -> Val.t
    val put : ?flags:Flags.t -> [> `Write ] txn -> Key.t -> Val.t -> unit
    val del : ?v:Val.t -> [> `Write ] txn -> Key.t -> unit
  end
end
