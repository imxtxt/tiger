type t = 
  | Int
  | String
  | Record of (Symbol.t * t) list * ID.t
  | Array of t * ID.t
  | Nil
  | Unit
  | Name of Symbol.t * t option ref
