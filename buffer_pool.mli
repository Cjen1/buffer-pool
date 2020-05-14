
module type Buffer = sig 
  type t

  val create : int -> t

  val reset : t -> unit

  val len : t -> int

  val sub : t -> int -> t
end

module Make ( B : Buffer ) : sig
  type t
  (** type of a buffer pool. *)

  type pooled_buffer 
  (** type of a buffer in the pool. *)

  type buffer = B.t
  (** type of the buffer. *)

  val get_buf : pooled_buffer -> buffer
  (** [get_buf b] gets the buffer represented by [b] *) 

  val create : t -> int -> pooled_buffer
  (** [create p len] create a new buffer from the pool [p] of length [len].
      This may be a new buffer or it may be a one which was previously
      released. *)

  val release : t -> pooled_buffer -> unit
  (** [release p buf] releases [buf] use and adds it back to [p].
      NOTE: this buffer must not be used again. *)

  val make : unit -> t
  (** [make ()] creates a new buffer pool *)
end 


