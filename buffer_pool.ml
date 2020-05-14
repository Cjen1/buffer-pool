
module type S = sig 
  type t

  val create : int -> t

  val reset : t -> t

  val len : t -> int
end

module Make(B : S ) = struct

  module M = Map.Make(Int)

  type buffer = B.t

  type t = {mutable available : B.t list M.t}


  let make () = []

  let lg2 =
    let base = log(2.) in
    fun x -> log x /. base

  let find_or_default m i default = 
    match M.find_opt m i with 
    | Some v -> v
    | None -> 
      default

  let pop_or_create m i f =
    match M.find_opt i m with
    | Some [] -> raise @@ Invalid_argument "Empty list in map"
    | Some [b] -> 
      M.remove i m, b
    | Some (b::bs) -> 
      let m' = M.remove i m |> M.add i bs in
      m', b
    | None -> m, f()

  let push_or_create m i v = 
    match M.find_opt i m with
    | Some bs -> M.add i (v :: bs) m
    | None -> M.add i [v] m

  let new_id =
    let max = Int.of_float @@ 2. ** 30. -. 1. in
    fun () -> Random.int max

  let create t size = 
    let e = lg2 size |> ceil |> Int.of_float in
    let default () = B.create @@ Int.of_float (2. ** (Float.of_int e)) in
    let avail', b = pop_or_create t.available e default in
    t.available <- avail';
    b

  let release t buf = 
    let buf = B.reset buf in
    let id = B.len buf |> Int.to_float |> lg2 |> ceil |> Int.of_float in
    t.available <- push_or_create t.available id buf
end 
