type t = string

let compare t1 t2 = String.compare t1 t2

module SMap = struct
  include Map.Make (String)
end
