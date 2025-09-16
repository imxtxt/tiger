open Hello_world

let parse file =
  let in_handle = open_in file in
  let lexbuf = Lexing.from_channel in_handle in
  let ast = Parser.start Lexer.token lexbuf in
  close_in in_handle;
  ast

let () = 
  let _ast = parse Sys.argv.(1) in
  ()