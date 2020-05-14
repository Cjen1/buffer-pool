
module BP = Buffer_pool.Make(struct
    include Cstruct

    let reset t = Cstruct.memset t 0
    end)

let strings = 
  [ "1"
  ; "12"
  ; "1234"
  ; "123456"
  ; "223456" ]

let (==) a b = Cstruct.equal a b

(* Create two buffers editing one should not affect the other *)
let independence l = 
  let bp = BP.make () in
  let a = BP.create bp l in
  let b = BP.create bp l in
  Cstruct.memset a 5;
  Crowbar.check @@ not (a == b)

(* Size of a buffer that you get out is always >= what you request*)
let sizing l = 
  let bp = BP.make () in
  let a = BP.create bp l in
  Crowbar.check @@ (Cstruct.len a >= l)

(* When a buffer is released it is zero'd *)
let release l = 
  let bp = BP.make () in
  let buf = BP.create bp l in
  Cstruct.memset buf 5;
  BP.release bp buf;
  let buf' = BP.create bp l in
  Crowbar.check_eq 
    ~cmp:Cstruct.compare 
    (Cstruct.create (Cstruct.len buf)) buf'

let () =
  let open Crowbar in
  add_test ~name:"independence" [int16] independence;
  add_test ~name:"sizing" [int16] sizing;
  add_test ~name:"release" [int16] release


