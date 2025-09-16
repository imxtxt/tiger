module SMap = Symbol.SMap

type venv = Env.env_entry Symbol.SMap.t
type tenv = Env.ty Symbol.SMap.t

type stms_exp_ty = {
  stms : Translate.stm list;
  exp : Translate.exp;
  ty : Types.t;
}

[@@@warning "-partial-match"]
[@@@warning "-unused-var-strict"]

let rec trans_var (venv : venv) (tenv : tenv) (var : Ast.var) : stms_exp_ty =
  match var with
  | Simple id ->
      let Var_entry ty = SMap.find id venv in
      { stms = []; exp = (); ty }
  | Field (var, id) ->
      let { stms; exp; ty = Record (fields, _) } = trans_var venv tenv var in
      { stms = []; exp = (); ty = List.assoc id fields }
  | Subscript (var, idx) ->
      let { stms; exp; ty = Array (ty, _) } = trans_var venv tenv var in
      let { stms; exp; ty = Int } = trans_exp venv tenv idx in
      { stms = []; exp = (); ty }

and trans_exp (venv : venv) (tenv : tenv) (exp : Ast.exp) : stms_exp_ty =
  failwith "TODO"

[@@@warning "+partial-match"]
[@@@warning "+unused-var-strict"]
