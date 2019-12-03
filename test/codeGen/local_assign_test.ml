open Base

let main () =
  let fname = "local_assign_test.in" in
  Gen.test fname

let _ = Backtrace.Exn.with_recording true ~f:main
