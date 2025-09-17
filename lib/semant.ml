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
  | Let (decs, e1) ->
      let venv, tenv =
        List.fold_left
          (fun (venv, tenv) dec ->
            trans_dec venv tenv dec) 
          (venv, tenv) decs
      in
      trans_exp venv tenv e1
  | Array (t, e1, e2) -> 
      let Array (ty, id) = SMap.find t tenv in
      let { stms; exp; ty = Int } = trexp e1 in
      let { stms; exp; ty = e2_ty } = trexp e2 in
      check_comp e2_ty ty ;
      { stms = []; exp = (); ty = Array (ty, id) }

and trans_dec (venv : venv) (tenv : tenv) (dec : Ast.dec) : venv * tenv =
  match dec with
  | Func_dec func_decs -> 
      let venv = 
        List.fold_left
          (fun venv Ast.{name; params; ret_ty; _} ->
            let params_tys = List.map (fun (p : Ast.param) -> SMap.find p.ty tenv) params in
            let ret_ty = 
              match ret_ty with
              | Some ret_ty -> SMap.find ret_ty tenv
              | None -> Unit
            in
            SMap.add name (Env.Fun_entry (params_tys, ret_ty)) venv) 
          venv func_decs
      in
      List.iter
        (fun Ast.{name; params; ret_ty; body} -> 
          let venv = 
            List.fold_left
              (fun venv (p : Ast.param) ->
                SMap.add p.name (Env.Var_entry (SMap.find p.ty tenv)) venv) 
              venv params
          in
          let { stms; exp; ty = body_ty } = trans_exp venv tenv body in
          match ret_ty with
          | None -> check_equal body_ty Unit
          | Some ret_ty -> check_comp body_ty (SMap.find ret_ty tenv)) 
        func_decs ;
      venv, tenv
  | Var_dec { name; escape; ty; init } ->
      let { stms; exp; ty = init_ty } = trans_exp venv tenv init in
      begin match ty with
      | Some ty ->
          let ty = SMap.find ty tenv in
          check_comp init_ty ty ;
          let venv = SMap.add name (Env.Var_entry ty) venv in
          venv, tenv
      | None when init_ty <> Nil ->
          let venv = SMap.add name (Env.Var_entry init_ty) venv in
          venv, tenv
      end
  | Type_dec type_decs ->
      let name_tys = List.map (fun Ast.{name; _} -> name, ref None) type_decs in
      let tenv =
        List.fold_left
          (fun tenv (name, ty) ->
            SMap.add name (Types.Name (name, ty)) tenv) 
          tenv name_tys
      in
      let real_tys = List.map (fun Ast.{ty_desc; _} -> trans_ty_desc tenv ty_desc) type_decs in
      List.iter2 (fun (_, t1) t2 -> t1 := Some t2) name_tys real_tys ;
      venv, tenv

and trans_ty_desc (tenv : tenv) (ty : Ast.ty_desc) : Types.t =
  match ty with
  | Name ty -> SMap.find ty tenv
  | Record fields ->
      let fields = List.map (fun (n, t) -> n, SMap.find t tenv) fields in
      Record (fields, ID.gen_id ())
  | Array ty ->
      let ty = SMap.find ty tenv in
      Array (ty, ID.gen_id ())

[@@@warning "+partial-match"]
[@@@warning "+unused-var-strict"]

let trans_prog (exp : Ast.exp) =
  trans_exp Env.base_venv Env.base_tenv exp
  |> ignore
