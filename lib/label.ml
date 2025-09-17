type t = string

let gen_label =
  let idx = ref 0 in
  fun () ->
    incr idx ;
    Printf.sprintf "L%d" !idx