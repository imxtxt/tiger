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

let rec trans_var (venv : venv) (tenv : tenv) (level : Translate.level) (var : Ast.var) : stms_exp_ty =
  match var with
  | Simple id ->
      let Var_entry (access, ty) = SMap.find id venv in
      { stms = []; exp = Tree.Const 0L; ty = actual_type ty }
  | Field (var, id) ->
      let { stms; exp; ty = Record (fields, _) } = trans_var venv tenv level var in
      let ty = List.assoc id fields in
      { stms = []; exp = Tree.Const 0L; ty =  actual_type ty }
  | Subscript (var, idx) ->
      let { stms; exp; ty = Array (ty, _) } = trans_var venv tenv level var in
      let { stms; exp; ty = Int } = trans_exp venv tenv level idx in
      { stms = []; exp = Tree.Const 0L; ty = actual_type ty }

and trans_exp (venv : venv) (tenv : tenv) (level : Translate.level)  (exp : Ast.exp): stms_exp_ty =
  let trexp exp = trans_exp venv tenv level exp in
  match exp with
  | Var var -> trans_var venv tenv level var
  | Nil -> 
      let stms, exp = Translate.nil in
      { stms; exp; ty = Nil }
  | Int num -> 
      let stms, exp = Translate.int num in
      { stms; exp; ty = Int }
  | String str -> 
      let stms, exp = Translate.string str in
      { stms; exp; ty = String }
  | Call (f, args) ->
      let args_stmss_exps_tys = List.map trexp args in
      let Fun_entry (fun_level, fun_label, tys, ty) = SMap.find f venv in
      List.iter2 (fun a t -> check_comp a.ty t) args_stmss_exps_tys tys ;
      { stms = []; exp = Tree.Const 0L; ty = actual_type ty } (* TODO *)
  | Aop (e1, aop, e2) ->
      let { stms = e1_stms; exp = e1_exp; ty = Int } = trexp e1 in
      let { stms = e2_stms; exp = e2_exp; ty = Int } = trexp e2 in
      let stms, exp =
        match aop with
        | Plus -> Translate.aop_plus e1_stms e1_exp e2_stms e2_exp
        | Minus -> Translate.aop_minus e1_stms e1_exp e2_stms e2_exp
        | Times -> Translate.aop_times e1_stms e1_exp e2_stms e2_exp
        | Divide -> Translate.aop_divide e1_stms e1_exp e2_stms e2_exp
      in
      { stms; exp; ty = Int }
  | Cop (e1, (Eq | Ne as cop), e2) -> 
      let { stms = e1_stms; exp = e1_exp; ty = e1_ty } = trexp e1 in
      let { stms = e2_stms; exp = e2_exp; ty = e2_ty } = trexp e2 in
      let stms, exp =
        match e1_ty, e2_ty, cop with
        | Int, Int, Eq -> Translate.cop_eq e1_stms e1_exp e2_stms e2_exp
        | Int, Int, Ne -> Translate.cop_ne e1_stms e1_exp e2_stms e2_exp
        | Record (_, id1), Record (_, id2), Eq when id1 = id2 ->
            Translate.cop_eq e1_stms e1_exp e2_stms e2_exp
        | Record (_, id1), Record (_, id2), Ne when id1 = id2 ->
            Translate.cop_ne e1_stms e1_exp e2_stms e2_exp
        | Record _, Nil, Eq
        | Nil, Record _, Eq -> Translate.cop_eq e1_stms e1_exp e2_stms e2_exp
        | Record _, Nil, Ne
        | Nil, Record _, Ne -> Translate.cop_ne e1_stms e1_exp e2_stms e2_exp
        | Array (_, id1), Array (_, id2), Eq when id1 = id2 ->
            Translate.cop_eq e1_stms e1_exp e2_stms e2_exp
        | Array (_, id1), Array (_, id2), Ne when id1 = id2 ->
            Translate.cop_ne e1_stms e1_exp e2_stms e2_exp
      in
      { stms; exp; ty = Int }
  | Cop (e1, cop, e2) ->
      let { stms = e1_stms; exp = e1_exp; ty = Int } = trexp e1 in
      let { stms = e2_stms; exp = e2_exp; ty = Int } = trexp e2 in
      let stms, exp =
        match cop with
        | Eq -> Translate.cop_eq e1_stms e1_exp e2_stms e2_exp
        | Ne -> Translate.cop_ne e1_stms e1_exp e2_stms e2_exp
        | Gt -> Translate.cop_gt e1_stms e1_exp e2_stms e2_exp
        | Ge -> Translate.cop_ge e1_stms e1_exp e2_stms e2_exp
        | Lt -> Translate.cop_lt e1_stms e1_exp e2_stms e2_exp
        | Le -> Translate.cop_le e1_stms e1_exp e2_stms e2_exp
      in
      { stms; exp; ty = Int }
  | Record (t, inits) -> 
      let Record (fields, id) = actual_type (SMap.find t tenv) in
      let inits_stmss_exps_tys = List.map (fun (n, e) -> n, trexp e) inits in
      List.iter2 
        (fun (n1, {ty = t1; _}) (n2, t2)-> 
          if n1 = n2 then 
            check_comp t1 t2
          else
            assert false)
        inits_stmss_exps_tys fields ;
      { stms = []; exp = Tree.Const 0L; ty = Record (fields, id) } (* TODO *)
  | Seq [] -> 
      let stms, exp = Translate.seq0 in
      { stms; exp; ty = Unit }
  | Seq es ->
      let es_stmss_exps_tys = List.map trexp es in
      let stmss_exps = List.map (fun {stms; exp; ty = _} -> stms, exp) es_stmss_exps_tys in
      let stms, exp = Translate.seq stmss_exps in
      let {ty; _} = List.nth es_stmss_exps_tys (List.length es_stmss_exps_tys - 1) in
      { stms; exp; ty }
  | Assign (var, e1) ->
      let { stms = var_stms; exp = var_exp; ty = var_ty } = trans_var venv tenv level var in
      let { stms = e1_stms; exp = e1_exp; ty = e1_ty } = trexp e1 in
      check_comp e1_ty var_ty ;
      let stms, exp = Translate.assign var_stms var_exp e1_stms e1_exp in
      { stms; exp; ty = Unit }
  | If2 (e1, e2) ->
      let { stms = e1_stms; exp = e1_exp; ty = Int } = trexp e1 in
      let { stms = e2_stms; exp = _; ty = Unit } = trexp e2 in
      let stms, exp = Translate.if2 e1_stms e1_exp e2_stms in
      { stms; exp; ty = Unit }
  | If3 (e1, e2, e3) ->
      let { stms = e1_stms; exp = e1_exp; ty = Int } = trexp e1 in
      let { stms = e2_stms; exp = e2_exp; ty = e2_ty } = trexp e2 in
      let { stms = e3_stms; exp = e3_exp; ty = e3_ty } = trexp e2 in
      let stms, exp = Translate.if3 e1_stms e1_exp e2_stms e2_exp e3_stms e3_exp in
      check_equal e2_ty e3_ty ;
      { stms; exp; ty = e2_ty }
  | While (e1, e2) ->
      let { stms = e1_stms; exp = e1_exp; ty = Int } = trexp e1 in
      let { stms = e2_stms; exp = _; ty = Unit } = trexp e2 in
      let stms, exp = Translate.while_exp e1_stms e1_exp e2_stms in
      { stms; exp; ty = Unit }
  | Break -> { stms = []; exp = Tree.Const 0L; ty = Unit } (* TODO *)
  | Let (decs, e1) ->
      let venv, tenv =
        List.fold_left
          (fun (venv, tenv) dec ->
            trans_dec venv tenv level dec) 
          (venv, tenv) decs
      in
      trans_exp venv tenv level e1
  | Array (t, e1, e2) -> 
      let Array (ty, id) = actual_type (SMap.find t tenv) in
      let { stms = e1_stms; exp = e1_exp; ty = Int } = trexp e1 in
      let { stms = e2_stms; exp = e2_exp; ty = e2_ty } = trexp e2 in
      check_comp e2_ty ty ;
      let stms, exp = Translate.array e1_stms e1_exp e2_stms e2_exp in
      { stms; exp; ty = Array (ty, id) }

and trans_dec (venv : venv) (tenv : tenv) (level : Translate.level) (dec : Ast.dec) : venv * tenv =
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
            let formals = List.map (fun (p : Ast.param) -> p.escape.contents) params in
            let fun_label = Label.gen_label () in
            let fun_level = Translate.new_level level fun_label formals in
            SMap.add name (Env.Fun_entry (fun_level, fun_label, params_tys, ret_ty)) venv) 
          venv func_decs
      in
      List.iter
        (fun Ast.{name; params; ret_ty; body} -> 
          let Fun_entry (fun_level, _, _, _) = SMap.find name venv in
          let accesses = Translate.formals fun_level in
          let venv = 
            List.fold_left2
              (fun venv (p : Ast.param) access ->
                SMap.add p.name (Env.Var_entry (access, SMap.find p.ty tenv)) venv) 
              venv params accesses
          in
          let { stms; exp; ty = body_ty } = trans_exp venv tenv fun_level body in
          Translate.proc_entry_exit fun_level stms exp ;
          match ret_ty with
          | None -> check_comp body_ty Unit
          | Some ret_ty -> check_comp body_ty (SMap.find ret_ty tenv)) 
        func_decs ;
      venv, tenv
  | Var_dec { name; escape; ty; init } ->
      let { stms; exp; ty = init_ty } = trans_exp venv tenv level init in
      let access = Translate.alloc_local level !escape in
      begin match ty with
      | Some ty ->
          let ty = SMap.find ty tenv in
          check_comp init_ty ty ;
          let venv = SMap.add name (Env.Var_entry (access, ty)) venv in
          venv, tenv
      | None when init_ty <> Nil ->
          let venv = SMap.add name (Env.Var_entry (access, init_ty)) venv in
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
  let level = Translate.new_level Translate.outermost "tiger_main" [] in
  let {stms; exp; _} = trans_exp Env.base_venv Env.base_tenv level exp in
  Translate.proc_entry_exit level stms exp ;
  Translate.get_result ()
