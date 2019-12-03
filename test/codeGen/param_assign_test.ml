open Base

let main () =
  let fname = "param_assign_test.monga" in
  Gen.test fname

let _ = Backtrace.Exn.with_recording true ~f:main
