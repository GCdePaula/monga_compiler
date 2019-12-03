open Base

let main () =
  let fname = "condition_exp_test.monga" in
  Gen.test fname

let _ = Backtrace.Exn.with_recording true ~f:main
