type ty = Types.t

type env_entry =
  | Var_entry of ty
  | Fun_entry of ty list * ty

let base_tenv : ty Symbol.SMap.t =
  Symbol.SMap.empty 
  |> Symbol.SMap.add "Int" Types.Int
  |> Symbol.SMap.add "String" Types.String
  
let base_venv : env_entry Symbol.SMap.t = 
  Symbol.SMap.empty
  |> Symbol.SMap.add "print" (Fun_entry ([Types.String], Types.Int))
  |> Symbol.SMap.add "flush" (Fun_entry ([], Types.Unit))
  |> Symbol.SMap.add "getchar" (Fun_entry ([], Types.String))
  |> Symbol.SMap.add "ord" (Fun_entry ([Types.String], Types.Int))
  |> Symbol.SMap.add "chr" (Fun_entry ([Types.Int], Types.String))
  |> Symbol.SMap.add "size" (Fun_entry ([Types.String], Types.Int))
  |> Symbol.SMap.add "substring" (Fun_entry ([Types.String; Types.Int; Types.Int], Types.String))
  |> Symbol.SMap.add "concat" (Fun_entry ([Types.String; Types.String], Types.String))
  |> Symbol.SMap.add "not" (Fun_entry ([Types.Int], Types.Int))
  |> Symbol.SMap.add "exit" (Fun_entry ([Types.Int], Types.Unit))
