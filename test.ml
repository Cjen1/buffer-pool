module BP_buffer = struct 
  include Cstruct

  let reset t = Cstruct.memset t 0
  let sub t = Cstruct.sub t 0
end 

module BP = Buffer_pool.Make(BP_buffer) 

(* Create two buffers editing one should not affect the other *)
let independence l = 
  let bp = BP.make () in
  let a = BP.create bp l in
  let b = BP.create bp l in
  Cstruct.memset (BP.get_buf a) 5;
  Crowbar.check @@ not (a == b)

(* Size of a buffer that you get out is always >= what you request*)
let sizing l = 
  let bp = BP.make () in
  let a = BP.create bp l in
  Crowbar.check @@ (Cstruct.len (BP.get_buf a) >= l)

(* When a buffer is released it is zero'd *)
let release l = 
  let bp = BP.make () in
  let buf = BP.create bp l in
  Cstruct.memset (BP.get_buf buf) 5;
  BP.release bp buf;
  let buf' = BP.create bp l in
  Crowbar.check_eq 
    ~cmp:Cstruct.compare 
    (Cstruct.create (Cstruct.len (BP.get_buf buf))) (BP.get_buf buf')

let () =
  let open Crowbar in
  add_test ~name:"independence" [uint16] independence;
  add_test ~name:"sizing" [uint16] sizing;
  add_test ~name:"release" [uint16] release


