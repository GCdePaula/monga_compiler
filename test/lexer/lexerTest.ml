open Src

let rec parse lexbuf =
  let token = Lexer.monga_lexer lexbuf in
  Printf.printf "%s\n" (Types.string_of_tk token);

  match token with
    | Types.Eof -> ()
    | _ -> parse lexbuf

let test fileName =
  let cin = open_in fileName in
  let lexbuf = Lexing.from_channel cin in
  try parse lexbuf
  with End_of_file -> ()

