open Base

let main () =
  let fname = "array_test.monga" in
  Gen.test fname

let _ = Backtrace.Exn.with_recording true ~f:main
