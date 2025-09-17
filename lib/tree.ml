type binop =
  | Plus
  | Minus
  | Mul
  | Div

type relop =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type exp =
  | Const of Int64.t
  | Name of Label.t
  | Temp of Temp.t
  | Binop of exp * binop * exp
  | Mem of exp
  | Call of Label.t * exp list

type stm =
  | Move of exp * exp
  | Call_stm of Label.t * exp list
  | Jump of Label.t
  | CJump of exp * relop * exp * Label.t * Label.t
  | Label of Label.t

type t = stm list
