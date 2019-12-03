open Base

let main () =
  let fname = "recursion_test.in" in
  Gen.test fname

let _ = Backtrace.Exn.with_recording true ~f:main
