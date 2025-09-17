let rv = Temp.gen_temp ()

type frame = {
  name : Label.t;
  formals : access list;
  mutable allocated : int;
}

and access =
  | In_frame of int
  | In_reg of Temp.t

and frag =
  | Proc of Tree.t * frame
  | String of Label.t * string

let new_frame (name : Label.t) (formals : bool list) : frame =
  let allocated, accesses = 
    List.fold_right
      (fun formal (allocated, accesses) ->
        if formal then
          let allocated = allocated + 1 in
          let access = In_frame (-8 * allocated) in
          allocated, access :: accesses
        else
          let access = In_reg (Temp.gen_temp ()) in
          allocated, access :: accesses) 
      formals (1, [])
  in
  { name; formals = accesses; allocated }

let name (frame : frame) : Label.t = 
  frame.name

let formals (frame : frame) : access list =
  frame.formals

let alloc_local (frame : frame) (in_frame : bool) : access =
  if in_frame then
    begin
      frame.allocated <- frame.allocated + 1 ;
      In_frame (-8 * frame.allocated)
    end
  else
    In_reg (Temp.gen_temp ())

let external_call name exps = Tree.Call (name, exps)

let proc_entry_exit1 (_frame : frame) (stms : Tree.t) : Tree.t =
  stms
