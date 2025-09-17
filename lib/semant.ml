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

let rec actual_type (t : Types.t) : Types.t =
  match t with
  | Name (_, { contents = Some t }) -> actual_type t
  | t -> t

let rec check_comp (t1 : Types.t) (t2 : Types.t) =
  match t1, t2 with
  | Int, Int
  | String, String
  | Unit, Unit
  | Nil, Record _ -> ()
  | Record (_, id1), Record (_, id2) when id1 = id2 -> ()
  | Array (_, id1), Array (_, id2) when id1 = id2 -> ()
  | Name (_, { contents = Some t }), t2 -> check_comp t1 t2
  | t1, Name (_, { contents = Some t2 }) -> check_comp t1 t2

let rec check_equal (t1 : Types.t) (t2 : Types.t) =
  match t1, t2 with
  | Int, Int
  | String, String
  | Unit, Unit
  | Nil, Record _
  | Record _, Nil -> ()
  | Record (_, id1), Record (_, id2) when id1 = id2 -> ()
  | Array (_, id1), Array (_, id2) when id1 = id2 -> ()
  | Name (_, { contents = Some t }), t2 -> check_equal t1 t2
  | t1, Name (_, { contents = Some t2 }) -> check_equal t1 t2

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
  let trexp exp = trans_exp venv tenv exp in
  match exp with
  | Var var -> trans_var venv tenv var
  | Nil -> { stms = []; exp = (); ty = Nil }
  | Int _ -> { stms = []; exp = (); ty = Int }
  | String _ -> { stms = []; exp = (); ty = String }
  | Call (f, args) ->
      let args_stmss_exps_tys = List.map trexp args in
      let Fun_entry (tys, ty) = SMap.find f venv in
      List.iter2 (fun a t -> check_comp a.ty t) args_stmss_exps_tys tys ;
      { stms = []; exp = (); ty }
  | Aop (e1, aop, e2) ->
      let { stms; exp; ty = Int } = trexp e1 in
      let { stms; exp; ty = Int } = trexp e2 in
      { stms = []; exp = (); ty = Int }
  | Cop (e1, (Eq | Ne), e2) -> 
      let { stms; exp; ty = e1_ty } = trexp e1 in
      let { stms; exp; ty = e2_ty } = trexp e2 in
      begin match e1_ty, e2_ty with
      | Int, Int -> { stms = []; exp = (); ty = Int }
      | String, String -> { stms = []; exp = (); ty = Int }
      | Record (_, id1), Record (_, id2) when id1 = id2 -> { stms = []; exp = (); ty = Int }
      | Record _, Nil
      | Nil, Record _ -> { stms = []; exp = (); ty = Int }
      | Array (_, id1), Array (_, id2) when id1 = id2 -> { stms = []; exp = (); ty = Int }
      | Unit, Unit -> { stms = []; exp = (); ty = Int }
      end
  | Cop (e1, cop, e2) ->
      let { stms; exp; ty = Int } = trexp e1 in
      let { stms; exp; ty = Int } = trexp e2 in
      { stms = []; exp = (); ty = Int }
  | Record (t, inits) -> 
      let Record (fields, id) = SMap.find t tenv in
      let inits_stmss_exps_tys = List.map (fun (n, e) -> n, trexp e) inits in
      List.iter2 
        (fun (n1, {ty = t1; _}) (n2, t2)-> 
          if n1 = n2 then 
            check_comp t1 t2
          else
            assert false)
        inits_stmss_exps_tys fields ;
      { stms = []; exp = (); ty = Record (fields, id) }
  | Seq [] -> { stms = []; exp = (); ty = Unit }
  | Seq (e1 :: es) ->
      List.fold_left
        (fun acc e -> trexp e)
        (trexp e1) es
  | Assign (var, e1) ->
      let { stms; exp; ty = var_ty } = trans_var venv tenv var in
      let { stms; exp; ty = e1_ty } = trexp e1 in
      check_comp e1_ty var_ty ;
      { stms = []; exp = (); ty = Unit }
  | If2 (e1, e2) ->
      let { stms; exp; ty = Int } = trexp e1 in
      let { stms; exp; ty = Unit } = trexp e2 in
      { stms = []; exp = (); ty = Unit }
  | If3 (e1, e2, e3) ->
      let { stms; exp; ty = Int } = trexp e1 in
      let { stms; exp; ty = e2_ty } = trexp e2 in
      let { stms; exp; ty = e3_ty } = trexp e3 in
      check_equal e2_ty e3_ty ;
      { stms; exp; ty = e2_ty }
  | While (e1, e2) ->
      let { stms; exp; ty = Int } = trexp e1 in
      let { stms; exp; ty = Unit } = trexp e2 in
      { stms = []; exp = (); ty = Unit }
  | Break -> { stms = []; exp = (); ty = Unit }
  | Let _ -> failwith "TODO"
  | Array (t, e1, e2) -> 
      let Array (ty, id) = SMap.find t tenv in
      let { stms; exp; ty = Int } = trexp e1 in
      let { stms; exp; ty = e2_ty } = trexp e2 in
      check_comp e2_ty ty ;
      { stms = []; exp = (); ty = Array (ty, id) }

[@@@warning "+partial-match"]
[@@@warning "+unused-var-strict"]

let trans_prog (exp : Ast.exp) =
  trans_exp Env.base_venv Env.base_tenv exp
  |> ignore
