type stm = unit
type exp = unit

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
