type aop =
  | Plus
  | Minus
  | Times
  | Divide

type cop =
  | Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le

type var =
  | Simple of Symbol.t
  | Field of var * Symbol.t
  | Subscript of var * exp

and exp =
  | Var of var
  | Nil
  | Int of Int64.t
  | String of string
  | Call of Symbol.t * exp list
  | Aop of exp * aop * exp
  | Cop of exp * cop * exp
  | Record of Symbol.t * (Symbol.t * exp) list
  | Seq of exp list
  | Assign of var * exp
  | If2 of exp * exp
  | If3 of exp * exp * exp
  | While of exp * exp
  | Break
  | Let of dec list * exp
  | Array of Symbol.t * exp * exp

and dec =
  | Func_dec of func_dec list
  | Var_dec of var_dec
  | Type_dec of type_dec list

and func_dec = {
  name : Symbol.t;
  params : param list;
  ret_ty : Symbol.t option;
  body : exp;
}

and param = {
  name : Symbol.t;
  escape : bool ref;
  ty : Symbol.t;
}

and var_dec = {
  name : Symbol.t;
  escape : bool ref;
  ty : Symbol.t option;
  init : exp;
}

and type_dec = {
  name : Symbol.t;
  ty_desc : ty_desc;
}

and ty_desc =
  | Name of Symbol.t
  | Record of (Symbol.t * Symbol.t) list
  | Array of Symbol.t
