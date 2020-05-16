module type Buffer = sig 
  type t

  val create : int -> t

  val reset : t -> unit

  val len : t -> int

  val sub : t -> int -> int -> t
end

module Make( B : Buffer ) : sig
  type t
  type pooled_buffer
  type buffer = B.t
  val get_buf: pooled_buffer -> buffer
  val alloc : t -> int -> pooled_buffer
  val create : B.t -> pooled_buffer
  val release : t -> pooled_buffer -> unit
  val make : unit -> t
  val sub : pooled_buffer -> int -> int -> pooled_buffer
end 
  = struct

  module M = Map.Make(struct type t = int let compare = compare end)

  type buffer = B.t

  type pooled_buffer = {buf:buffer; underlying:buffer}

  type t = {mutable available : buffer list M.t}

  let make () = {available = M.empty}

  let lg2 =
    let base = log(2.) in
    fun x -> log x /. base

  let find_opt i m = 
  try
    let res = M.find i m in
    Some res
  with Not_found -> 
    None

  let pop m i f =
    match find_opt i m with
    | Some [] -> raise @@ Invalid_argument "Empty list in map"
    | Some [b] -> 
      M.remove i m, b
    | Some (b::bs) -> 
      let m' = M.remove i m |> M.add i bs in
      m', b
    | None -> m, f()

  let push m i v = 
    match find_opt i m with
    | Some bs -> M.add i (v :: bs) m
    | None -> M.add i [v] m

  let alloc t size = 
    let e = size |> float_of_int |> lg2 |> ceil |> int_of_float in
    let default () = B.create @@ int_of_float (2. ** (float_of_int e)) in
    let avail', b = pop t.available e default in
    t.available <- avail';
    {buf = B.sub b 0 size; underlying = b}

  let create buf = 
    {buf; underlying=buf}

  let release t buf = 
    let buf = buf.underlying in
    B.reset buf;
    let id = buf |> B.len |> float_of_int |> lg2 |> ceil |> int_of_float in
    t.available <- push t.available id buf

  let get_buf buf = 
    buf.buf

  let sub buf off len = 
    let buf' = B.sub buf.buf off len in
    {buf with buf = buf'}
end 
