
module type Buffer = sig 
  type t

  val create : int -> t

  val reset : t -> unit

  val len : t -> int

  val sub : t -> int -> int -> t
end

module Make( B : Buffer ) : sig
  type t
  type buffer = B.t
  val alloc : t -> int -> B.t
  val release : t -> buffer -> unit
  val make : unit -> t
end 
