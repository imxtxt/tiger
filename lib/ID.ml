type t = int

let gen_id =
  let idx = ref 0 in
  fun () ->
    incr idx ;
    !idx
