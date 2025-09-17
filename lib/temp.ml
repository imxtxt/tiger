type t = string

let gen_temp =
  let idx = ref 0 in
  fun () ->
    incr idx ;
    Printf.sprintf "T%d" !idx