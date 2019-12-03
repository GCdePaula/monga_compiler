open Base

let main () =
  let fname = "function_call_test.in" in
  Gen.test fname

let _ = Backtrace.Exn.with_recording true ~f:main
