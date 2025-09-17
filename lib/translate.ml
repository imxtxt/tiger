module T = Tree

type stm = T.stm
type exp = T.exp

type level = 
  | Outermost
  | Level of level * Frame.frame

type access = level * Frame.access

let outermost = Outermost

let new_level (parent : level) (name : Label.t) (formals : bool list) : level =
  let frame = Frame.new_frame name (true :: formals) in
  Level (parent, frame)

[@@@warning "-partial-match"]

let formals (level : level) : access list =
  match level with
  | Level (_, frame) ->
      let formals = List.tl (Frame.formals frame) in
      List.map (fun formal -> level, formal) formals

let alloc_local (level : level) (in_frame : bool) : access =
  match level with
  | Level (_, frame) ->
      let access = Frame.alloc_local frame in_frame in
      level, access

[@@@warning "+partial-match"]

let frags = ref []

let nil = [], T.Const 0L

let int num = [], T.Const num

let string string =
  let label = Label.gen_label () in
  frags := Frame.String (label, string) :: !frags ;
  [], T.Name label

let aop_plus stms1 exp1 stms2 exp2 =
  stms1 @ stms2, T.Binop (exp1, T.Plus, exp2)

let aop_minus stms1 exp1 stms2 exp2 =
  stms1 @ stms2, T.Binop (exp1, T.Minus, exp2)

let aop_times stms1 exp1 stms2 exp2 =
  stms1 @ stms2, T.Binop (exp1, T.Mul, exp2)

let aop_divide stms1 exp1 stms2 exp2 =
  stms1 @ stms2, T.Binop (exp1, T.Div, exp2)

let cop op stms1 exp1 stms2 exp2 =
  let temp = Temp.gen_temp () in
  let then_label = Label.gen_label () in
  let else_label = Label.gen_label () in
  let join_label = Label.gen_label () in
  let stms = 
    stms1 @
    stms2 @
    [ T.CJump (exp1, op, exp2, then_label, else_label);
      T.Label then_label;
      T.Move (T.Temp temp, T.Const 1L);
      T.Jump join_label;
      T.Label else_label;
      T.Move (T.Temp temp, T.Const 0L);
      T.Jump join_label;
      T.Label join_label ]
  in
  stms, T.Temp temp

let cop_eq = cop T.Eq
let cop_ne = cop T.Ne
let cop_gt = cop T.Gt
let cop_ge = cop T.Ge
let cop_lt = cop T.Lt
let cop_le = cop T.Le

let seq0 = [], T.Const 0L

let seq (stmss_exps : (T.stm list * T.exp) list) =
  let stmss, exps = List.split stmss_exps in
  let stms = List.flatten stmss in
  let exp = List.nth exps (List.length exps - 1)  in
  stms, exp

let assign stms1 exp1 stms2 exp2 =
  let stms =
    stms1 @
    stms2 @
    [ T.Move (exp1, exp2) ]
  in
  stms, T.Const 0L

let if2 stms1 exp1 stms2 =
  let then_label = Label.gen_label () in
  let join_label = Label.gen_label () in
  let stms = 
    stms1 @
    [ T.CJump (exp1, T.Ne, T.Const 0L, then_label, join_label) ] @
    [ T.Label then_label ] @
    stms2 @
    [ T.Jump join_label ] @
    [ T.Label join_label]
  in
  stms, T.Const 0L

let if3 stms1 exp1 stms2 exp2 stms3 exp3 =
  let temp = Temp.gen_temp () in
  let then_label = Label.gen_label () in
  let else_label = Label.gen_label () in
  let join_label = Label.gen_label () in
  let stms = 
    stms1 @
    [ T.CJump (exp1, T.Ne, T.Const 0L, then_label, else_label) ] @
    [ T.Label then_label ] @
    stms2 @
    [ T.Move (T.Temp temp, exp2) ] @
    [ T.Jump join_label ] @
    [ T.Label else_label ] @
    stms3 @
    [ T.Move (T.Temp temp, exp3) ] @
    [ T.Jump join_label ] @
    [ T.Label join_label]
  in
  stms, T.Temp temp

let while_exp stms1 exp1 stms2 =
  let start_label = Label.gen_label () in
  let then_label = Label.gen_label () in
  let else_label = Label.gen_label () in
  let stms =
    [ T.Label start_label ] @
    stms1 @
    [ T.CJump (exp1, T.Ne, T.Const 0L, then_label, else_label) ] @
    [ T.Label then_label ] @
    stms2 @
    [ T.Jump start_label ] @
    [ T.Label else_label ]
  in
  stms, T.Const 0L

let array stms1 exp1 stms2 exp2 =
  let temp = Temp.gen_temp () in
  let exp = Frame.external_call "init_array" [exp1; exp2] in
  let stms =
    stms1 @ 
    stms2 @
    [ T.Move (T.Temp temp, exp) ]
  in
  stms, T.Temp temp


[@@@warning "-partial-match"]

let proc_entry_exit (level : level) (stms : T.stm list) (exp : T.exp) =
  match level with
  | Level (_, frame) ->
      let stms = Frame.proc_entry_exit1 frame (stms @ [T.Move (T.Temp Frame.rv, exp)]) in
      frags := Frame.Proc (stms, frame) :: !frags
  
[@@@warning "+partial-match"]

let get_result () = !frags
