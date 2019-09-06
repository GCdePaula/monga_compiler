
let main () =
  let fileName = "inputs/input1.in" in
  LexerTest.test fileName;
  let fileName = "inputs/input2.in" in
  LexerTest.test fileName


let _ = Printexc.print main ()
