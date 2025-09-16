module SMap = Symbol.SMap

type venv = Env.env_entry Symbol.SMap.t
type tenv = Env.ty Symbol.SMap.t

[@@@warning "-partial-match"]
[@@@warning "+partial-match"]
