module type Buffer = sig 
  type t

  val create : int -> t

  val reset : t -> unit

  val len : t -> int

  val sub : t -> int -> t
end

module Make( B : Buffer ) : sig
  type t
  type pooled_buffer
  type buffer = B.t
  val get_buf: pooled_buffer -> buffer
  val create : t -> int -> pooled_buffer
  val release : t -> pooled_buffer -> unit
  val make : unit -> t
end 
  = struct

  module M = Map.Make(Int)

  type buffer = B.t

  type pooled_buffer = {buf:buffer; underlying:buffer}

  type t = {mutable available : buffer list M.t}

  let make () = {available = M.empty}

  let lg2 =
    let base = log(2.) in
    fun x -> log x /. base

  let pop m i f =
    match M.find_opt i m with
    | Some [] -> raise @@ Invalid_argument "Empty list in map"
    | Some [b] -> 
      M.remove i m, b
    | Some (b::bs) -> 
      let m' = M.remove i m |> M.add i bs in
      m', b
    | None -> m, f()

  let push m i v = 
    match M.find_opt i m with
    | Some bs -> M.add i (v :: bs) m
    | None -> M.add i [v] m

  let create t size = 
    let e = size |> Int.to_float |> lg2 |> ceil |> Int.of_float in
    let default () = B.create @@ Int.of_float (2. ** (Float.of_int e)) in
    let avail', b = pop t.available e default in
    t.available <- avail';
    {buf = B.sub b size; underlying = b}

  let release t buf = 
    let buf = buf.underlying in
    B.reset buf;
    let id = buf |> B.len |> Int.to_float |> lg2 |> ceil |> Int.of_float in
    t.available <- push t.available id buf

  let get_buf buf = 
    buf.buf

  let sub buf off len = 
    let buf' = B.sub buf.buf len in
    {buf with buf = buf'}
end 
